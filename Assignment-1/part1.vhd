-- TODO
-- PREGUNTAR COMO SOLUCIONAR LO DEL PC = 0

library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.common.all;

entity part1 is
    port (reset : in  std_logic;
          clk   : in  std_logic;
          a : out word;
          b : out word;
			    y : out word;
          p : out std_logic_vector(5 downto 0);
          i : out word);
end part1;

architecture behavioral of part1 is

--          ALU signals
signal alu_func : alu_func_t := ALU_NONE; -- ALU Function
signal alu_A : word := x"00000000";       -- ALU input A
signal alu_B : word := x"00000000";       -- ALU input B
signal alu_out : word := x"00000000";     -- ALU output

--          Instruction Memory signals
signal pc : unsigned(word'range) := x"00000000"; -- PC (input of IMEM)
signal ir : word := x"00000000";       -- Instruction (output of IMEM)

--          Data Memory
-- raddr => alu_out(7 downto 2)           input address for Reading
signal dmem_out : word := x"00000000"; -- output data from DMEM
-- waddr => alu_out(7 downto 2),          input address for Writing
signal reg_B : word := x"00000000";    -- input data to Write

--          Register Bank
signal rs1 : std_logic_vector(4 downto 0); -- Input Adress for Register 1
signal rs2 : std_logic_vector(4 downto 0); -- Input Adress for Register 2
--  alu_out                                -- Input Data for Register 1
--  reg_B                                  -- Input Address for the data to be written
signal rd : std_logic_vector(4 downto 0);  -- Addres to Write the data
signal rf_wdata : word := x"00000000";     -- Data to Write

--          Immediate values
signal imm : word := x"00000000";
signal imm_rd : word := x"00000000";
signal branch_imm : unsigned(word'range) := x"00000000"; -- Branch Immediate
signal branch_abs : unsigned(word'range) := x"00000000"; -- Branch Immediate to an Absolute position

-- instruction fields
signal opcode : opcode_t;  -- Type of operation to be executed (R, I, B, S)
signal funct3 : std_logic_vector(2 downto 0);
signal funct7 : std_logic_vector(6 downto 0);

-- control signals
signal regwrite : std_logic;  -- Control the Writing into the Data Memory (DMEM)
signal wbsel : std_logic;
signal memwrite : std_logic;  -- Control if we write data into the Data Memory (DMEM)
signal op2sel : std_logic_vector(1 downto 0); -- Control the Second Input Value to the ALU
signal PCSel: std_logic_vector(1 downto 0);  -- Selects the input value of the PC

-- ALU
component alu is
port (alu_func : in  alu_func_t;
		op1      : in  word;
		op2      : in  word;
		result   : out word);
end component alu;

-- Instruction Memory
component imem is
port(
	addr : in std_logic_vector(5 downto 0);
	dout : out word);
end component  imem;

-- Data Memory
component dmem is
port (reset : in  std_logic;
      clk   : in  std_logic;
      raddr : in  std_logic_vector(5 downto 0);
      dout  : out word;
      waddr : in  std_logic_vector(5 downto 0);
      din   : in  word;
      we    : in  std_logic);
end component dmem;

-- Register Bank
component regfile is
port (reset : in  std_logic;
      clk   : in  std_logic;
      addra : in  std_logic_vector(4 downto 0);
      addrb : in  std_logic_vector(4 downto 0);
      rega  : out word;
      regb  : out word;
      addrw : in  std_logic_vector(4 downto 0);
      dataw : in  word;
      we    : in  std_logic);
end component regfile;

begin
	-- datapath
  -- ALU
	alu0: alu port map(
		  alu_func => alu_func,
      op1 => alu_A,
      op2 => alu_B,
			result => alu_out);

  -- Main Instruction Memory
	imem0: imem port map(
      addr => std_logic_vector(pc(7 downto 2)),
      dout => ir);

  -- Data Memory
	dmem0: dmem port map(
	    reset => reset,
		  clk => clk,
      raddr => alu_out(7 downto 2),
		  dout => dmem_out,
		  waddr => alu_out(7 downto 2),
		  din => reg_B,
		  we => memwrite);

  -- Register file
	rf0: regfile port map(
	    reset => reset,
		  clk => clk,
      addra => rs1,
      addrb => rs2,
		  rega => alu_A,
		  regb => reg_B,
		  addrw => rd,
		  dataw => rf_wdata,
		  we => regwrite);


  -- op2sel | alu_B
  -------------------
  --  "00"  | reg_B
  --  "01"  | imm
  --  "10"  | unsigned(imm)
  --  "11"  | imm_rd


  -- Select the second input to the ALU
	alu_B <= reg_B WHEN op2sel = "00" else
			imm WHEN op2sel = "01" else
            std_logic_vector(unsigned(imm)) WHEN op2sel = "10" else
			imm_rd;


  -- wbsel | rf_wdata
  --------------------
  --  "0"  |  alu_out
  --  "1"  |  reg_B


  -- Select the Data to Write into the Register Bank
   rf_wdata <= alu_out WHEN wbsel = '0' else reg_B;

	-- instruction fields
	imm(31 downto 12) <= (others => ir(31));
	imm(11 downto 0) <= ir(31 downto 20);
	imm_rd(31 downto 12) <= (others => funct7(6));
	imm_rd(11 downto 5) <= funct7;
	imm_rd(4 downto 0) <= rd;
  rs1 <= ir(19 downto 15);
  rs2 <= ir(24 downto 20);
	rd <= ir(11 downto 7);
	funct3 <= ir(14 downto 12);
	funct7 <= ir(31 downto 25);
	opcode <= ir(6 downto 0);
	branch_imm(31 downto 13) <= (others => ir(31));
	branch_imm(12 downto 0) <= unsigned(ir(31) & ir(7) &
								ir(30 downto 25) & ir(11 downto 8) & '0');
	jump_imm()

  --                  DECODE
   decode_proc : process (ir, funct7, funct3, opcode, alu_A,reg_B) is
	begin
    -- All the values to 0 as a default
		regwrite <= '0';
		op2sel   <= "00";
		memwrite <= '0';
		wbsel    <= '0';
		alu_func <= ALU_NONE;
		PCSel    <= "00";
    -- Check what type of operation it is
		case opcode is
      -- IMMEDIATE OPERATION
			WHEN OP_ITYPE =>
				regwrite <= '1'; -- Write into the Register Bank
				op2sel <= "01";  -- The second input to the ALU is the Immediate Value
        -- Check what operation to be executed
				case (funct3) is
                    WHEN "000" => alu_func <= ALU_ADD;
                    WHEN "001" => alu_func <= ALU_SLL;
                    WHEN "010" => alu_func <= ALU_SLT;
                    WHEN "011" => alu_func <= ALU_SLTU;
                    WHEN "100" => alu_func <= ALU_XOR;
                    WHEN "110" => alu_func <= ALU_OR;
                    WHEN "111" => alu_func <= ALU_AND;
                    WHEN "101" =>
                        if (ir(30) = '1') then
                            alu_func <= ALU_SRA;
                        else
                            alu_func <= ALU_SRL;
                        end if;
                    WHEN others => null;
                end case;
      -- REGISTER-REGISTER OPERATION
			WHEN OP_RTYPE =>
				regwrite <= '1'; -- Write into the Register Bank
        -- The second input to the ALU is reg_B
				case (funct3) is
					WHEN "000" =>
						if (ir(30) = '1') then
							 alu_func <= ALU_SUB;
						else
							 alu_func <= ALU_ADD;
						end if;
					WHEN "001" => alu_func <= ALU_SLL;
					WHEN "010" => alu_func <= ALU_SLT;
					WHEN "011" => alu_func <= ALU_SLTU;
					WHEN "100" => alu_func <= ALU_XOR;
					WHEN "101" =>
						if (ir(30) = '1') then
							 alu_func <= ALU_SRA;
						else
							 alu_func <= ALU_SRL;
						end if;
					WHEN "110"  => alu_func <= ALU_OR;
					WHEN "111"  => alu_func <= ALU_AND;
					WHEN others => null;
				end case;
      -- STORE OPERATION
			WHEN OP_STORE =>
			  regwrite <= '0'; -- Do NOT Write into the Register Bank
				memwrite <= '1'; -- Write into the Data Memory (DMEM)
				op2sel <= "11";  -- The second input to the ALU is imm_rd
				case (funct3) is
          WHEN "000" => alu_func <= ALU_NONE;
          WHEN "001" => alu_func <= ALU_NONE;
				  WHEN "010" => alu_func <= ALU_ADD;
          WHEN "011" => alu_func <= ALU_NONE;
          WHEN "100" => alu_func <= ALU_NONE;
          WHEN "101" => alu_func <= ALU_NONE;
			    WHEN others => null;
			  end case;
      -- BRANCH OPERATION
			WHEN	OP_BRANCH =>
			  regwrite <= '0'; -- Do NOT Write into the Register Bank

        -- PCSel |        pc
        -----------------------------
        --  "00" |      pc + 4
        --  "01" |  pc + branch_imm
        --  "10" |     branch_abs
        --  "11" |     branch_imm

				case (funct3) is
				  -- BEQ
				  WHEN "000" =>
					if (alu_A = reg_B) then
					  PCSel <= "01";
					else
					  PCSel <= "00";
					end if;
				  -- BNE
				  WHEN "001" =>
					if (alu_A /= reg_B) then
					  PCSel <= "01";
					else
					  PCSel <= "00";
					end if;
				  -- BLT
				  WHEN "100" =>
					if (alu_A < reg_B) then
					  PCSel <= "01";
					else
					  PCSel <= "00";
					end if;
				  -- BGE
				  WHEN "101" =>
					if (alu_A > reg_B) then
					  PCSel <= "01";
					else
					  PCSel <= "00";
					end if;
				  -- BGTU
				  WHEN "110" =>
					if (unsigned(alu_A) > unsigned((reg_B))) then
					  PCSel <= "01";
					else
					  PCSel <= "00";
					end if;
				  -- BGEU
				  WHEN "111" =>
					if (unsigned(alu_A) >= unsigned(( reg_B))) then
					  PCSel <= "01";
					else
					  PCSel <= "00";
					end if;
				  WHEN others => null;
				end case;
      -- UNCONDITIONAL BRANCH
      -- JAL, J
      WHEN OP_UBRANCH =>
          println("Unconditional");
          regwrite <= '0'; -- Do NOT Write into the Register Bank
          case (rd) is
            -- J
            WHEN "00000" =>
              println("J");
			  PCSel <= "10"; -- Set the pc directly using branch_imm
			  println(hstr(std_logic_vector(branch_imm)));
			  println(hstr(std_logic_vector(ir)));
			  --println(hstr(shift_right(unsigned(branch_imm), 6)));
              branch_abs <= shift_right(unsigned(branch_imm), 6);
            -- JAL
            WHEN others =>
			  println("JAL");
			  println(hstr(std_logic_vector(branch_imm)));
              PCSel <= "10"; -- Set the pc directly using branch_imm
              branch_abs <= shift_right(unsigned(branch_imm), 6);
          end case;
      -- JALR
      -- Similar to JAL, except destination is relative to the value of rs1
      WHEN OP_LRBRANCH =>
        case (funct3) is
          WHEN "000" =>
          println("JALR");
          println(hstr(std_logic_vector(ir)));
          println(hstr(std_logic_vector(opcode)));
          WHEN others => null;
        end case;
      -- LUI
      WHEN OP_LUI =>
        regwrite <= '1'; -- Write into the Register Bank
        op2sel <= "10";  -- The second input to the ALU is the Immediate Value Unsigned
        alu_func <= ALU_ADD;

      -- AUIPC
      -- Adds a 20-bit upper immediate to the PC
      WHEN OP_AUIPC =>
        case (funct3) is
          WHEN "000" =>
          println("AUIPC");
          println(hstr(std_logic_vector(ir)));
          println(hstr(std_logic_vector(opcode)));
          WHEN others => null;
        end case;
      WHEN others => null;
		 end case;
    end process;

	y <= alu_out; -- We output the value of the ALU, we could change it if we want to
  p <= std_logic_vector(pc(7 downto 2));
  a <= alu_A;
  b <= alu_B;
  i <= ir;

  -- Check the reset & Updates the PC
	acc: process(reset, clk)
	begin
    -- Check reset
		if (reset = '1') then
      -- When we reset we start in the main
			pc <= (others=>'0');
		elsif rising_edge(clk) then
      -- PC+4
		  if( PCSel = "00") then
			   pc <= pc + 4;
      -- PC + BRANCH
      elsif (PCSel = "01") then
  			  pc <= unsigned(pc + branch_imm);
      -- PC <= BRANCH
      elsif (PCSel = "10") then
        pc <= branch_abs;
      elsif (PCSel = "11") then
          pc <= branch_abs;
  		end if;
		end if;
	end process;
end architecture;

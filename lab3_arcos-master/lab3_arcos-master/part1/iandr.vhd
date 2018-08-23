-- execute I and R type instructions

library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.common.all;

entity iandr is
    port (reset : in  std_logic;
          clk   : in  std_logic;
             y : out word;
             regA : out word;
             regB : out word);
end iandr;

architecture behavioral of iandr is

signal alu_func : alu_func_t := ALU_NONE;
signal alu_A : word := x"00000000"; -- Input 1 ALU
signal alu_B : word := x"00000000"; -- Input 2 ALU
signal alu_out : word := x"00000000"; -- Output ALU
signal reg_B : word := x"00000000";
signal imm : word := x"00000000";
signal ir : word := x"00000000";

signal zeros : std_logic_vector(4 downto 0) := (others => '0');

-- instruction fields
signal opcode : opcode_t; -- Operation Code
signal funct3 : std_logic_vector(2 downto 0);
signal funct7 : std_logic_vector(6 downto 0);
signal rs1 : std_logic_vector(4 downto 0); -- Origin Register 1
signal rs2 : std_logic_vector(4 downto 0); -- Origin Register 2
signal rd : std_logic_vector(4 downto 0); -- Destination Register
signal pc : unsigned(word'range) := x"00000000"; -- PC

-- control signals
signal regwrite : std_logic; -- Enable Write
signal op2sel : std_logic;

-- Register Bank Signals
signal dataw : word := x"00000000";

component alu is
port (alu_func : in  alu_func_t;
        op1      : in  word; -- Input 1
        op2      : in  word; -- Input 2
        result   : out word); -- Output
end component alu;

component imem is
port(
    addr : in std_logic_vector(3 downto 0); -- Input memory
    dout : out word); -- Output value
end component imem;

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
    alu0: alu port map(
        alu_func => alu_func,
        op1 => alu_A,
        op2 => alu_B,
        result => alu_out);

    imem0: imem port map(
        addr => std_logic_vector(pc(3 downto 0)),
        dout => ir); -- Instruction Register

    -- instantiate regfile here
    regfile0: regfile port map(
	reset => reset,
	clk => clk,
	addra => rs1,
	addrb => rs2,
	rega => alu_A,
	regb => reg_B,
	addrw => rd,
	dataw => dataw,
	we => regwrite);

    alu_B <= imm;

    -- instruction fields
    imm(31 downto 12) <= (others => ir(31));
    imm(11 downto 0) <= ir(31 downto 20);
    rs1 <= ir(19 downto 15);
    rs2 <= ir(24 downto 20);
    rd <= ir(11 downto 7);
    funct3 <= ir(14 downto 12);
    funct7 <= ir(31 downto 25);
    opcode <= ir(6 downto 0);

    -- control signals
    op2sel <= '1';
    regwrite <= '0'  when rd = "00000" else
      '1'  when ir /= NOP ;

    regA <= alu_A;
    regB <= alu_B;

    decode_proc : process (funct7, funct3) is
    begin
           case (funct3) is
               when "000" =>
                   if (funct7(5) = '1') then
                       alu_func <= ALU_SUB;
                   else
                       alu_func <= ALU_ADD;
                   end if;
	       when others => null;
           end case;
    end process;

    y <= alu_out;

    save_values : process (alu_out) is
    begin
      dataw <= alu_out;
    end process;

    acc: process(reset, clk)
    begin
        if (reset = '1') then
            pc <= (others => '0');
        elsif rising_edge(clk) then
            pc <= pc + 1;
        end if;
    end process;
end architecture;

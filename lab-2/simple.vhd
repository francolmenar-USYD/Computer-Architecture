library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.common.all;

entity simple is
    port (reset : in  std_logic;
          clk   : in  std_logic;
			 y : out word);
end simple;

architecture behavioral of simple is

signal func : alu_func_t := ALU_NONE;
signal A : word := x"00000000";
signal B : word := x"00000000";
signal C : word := x"00000000";
signal second_argument : std_logic_vector(11 downto 0);
signal address : std_logic_vector(3 downto 0);
signal m_out : word := x"00000000";
signal aluout : word := x"00000000";
signal pc : unsigned(word'range) := x"00000000";
signal regout : word := x"00000000";

signal imm : std_logic_vector(11 downto 0);
signal rs1 : std_logic_vector(5 downto 0);
signal func3_mem : funct3_t;
signal rd : std_logic_vector(5 downto 0);
signal opcode_m : op_t;
	 
component alu is
port (alu_func : in  alu_func_t;
      op1      : in  word;
      op2      : in  word;
		result   : out word);
end component alu;

component imem is
port (addr : in std_logic_vector(3 downto 0);
        dout : out word);
end component imem;

begin
	alu0: alu port map(
				alu_func => func,
            op1 => A, -- Cumulative result
            op2 => C, -- Second argument from the memory
				result => aluout);

	imem0: imem port map(
				     addr => address, -- Address for the memory (PC)
         			 dout => m_out); -- Output value of the memory
		  
	A <= regout;
	B <= std_logic_vector(pc);
	address <= B(3 downto 0);
	
	imm <= m_out(31 downto 20);
	rs1 <= m_out(19 downto 15);
	--func3_mem <= m_out(14 downto 12);
	--rd <= m_out(11 downto 7);
	--opcode_m <= m_out(6 downto 0);

	func <= ALU_ADD;
	y <= regout;
	
	acc: process(reset, clk) 
	begin 
		if (reset = '1') then 
			regout <= (others => '0'); 
			pc <= (others => '0');
		elsif rising_edge(clk) then 
			regout <= aluout;
			pc <= pc + 1;
		end if; 
	end process; 
end architecture;



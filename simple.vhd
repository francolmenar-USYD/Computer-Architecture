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
signal C : std_logic_vector(3 downto 0);
signal D : word := x"00000000";
signal aluout : word := x"00000000";
signal pc : unsigned(word'range) := x"00000000";
signal regout : word := x"00000000";
	 
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
            op1 => A,
            op2 => D,
				result => aluout);

	imem0: imem port map(
				addr => C,
            			dout => D);
		  
	A <= regout;
	-- B <= std_logic_vector(pc);
	-- C <= std_logic_vector((pc)3 downto 0);
	-- C <=  std_logic_vector((X"00000000")3 downto 0);
	C <= std_logic_vector(pc);

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

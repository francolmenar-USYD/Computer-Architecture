library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.common.all;

entity part1_tb is
end part1_tb;

architecture behavioral of part1_tb is

constant clk_period : time := 10 ns;
signal clk : std_logic;
signal reset : std_logic;
signal aluOut : word;
signal pc : std_logic_vector(5 downto 0);
signal alu_A : word := x"00000000";       -- ALU input A
signal alu_B : word := x"00000000";       -- ALU input B
signal inst : word := x"00000000";

component part1 is
	port (reset : in  std_logic;
				clk   : in  std_logic;
				a : out word;
				b : out word;
				y : out word;
				p : out std_logic_vector(5 downto 0);
				i : out word);
end component;

begin

	  u0: part1 port map(
				reset => reset,
				clk => clk,
				a => alu_A,
				b => alu_B,
				y => aluOut,
				p => pc,
				i => inst);

    proc_clock: process
    begin
        clk <= '0';
        wait for clk_period/2;
        clk <= '1';
        wait for clk_period/2;
    end process;

    proc_stimuli: process
    begin
        reset <= '1';
        wait for clk_period * 2;
        reset <= '0';
		wait for clk_period * 50;
				-- assert (pout = x"00000030") report "error pc" severity failure;
        assert false report "success - end of simulation" severity failure;
    end process;
end architecture;

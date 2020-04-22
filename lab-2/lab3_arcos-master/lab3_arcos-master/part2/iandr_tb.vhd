library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.common.all;

entity iandr_tb is
end iandr_tb;

architecture behavioral of iandr_tb is

constant clk_period : time := 10 ns;
signal clk : std_logic;
signal reset : std_logic;
signal cpuout : word;
signal A : word;
signal B : word;


-- Register Bank Signals
signal addra : std_logic_vector(4 downto 0);
signal addrb :  std_logic_vector(4 downto 0);
signal rega  : word := x"00000000";
signal regb  : word := x"00000000";
signal addrw : std_logic_vector(4 downto 0);
signal dataw : word := x"00000000";
signal we    : std_logic := '0';

component iandr is
	port (reset : in  std_logic;
  		clk   : in  std_logic;
		y : out word;
    regA : out word;
    regB : out word);
end component;

-- Registers
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

	 u0: iandr port map(
		reset => reset,
		clk => clk,
		y => cpuout,
		regA => A,
		regB => B);

    regfile0: regfile port map(
	reset => reset,
	clk => clk,
	addra => addra,
	addrb => addrb,
	rega => rega,
	regb => regb,
	addrw => addrw,
	dataw => dataw,
	we => we);

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
		  wait for clk_period * 16;
		  assert (cpuout = x"00000078") report "error 'cpuout' is "
				& integer'image(to_integer(unsigned(cpuout))) severity failure;
        assert false report "success - end of simulation" severity failure;
    end process;
end architecture;

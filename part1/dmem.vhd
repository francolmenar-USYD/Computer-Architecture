library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

library work;
use work.common.all;

entity dmem is
    port (reset : in  std_logic;
          clk   : in  std_logic;
          raddr : in  std_logic_vector(5 downto 0); -- Address of memory of the data to read
          dout  : out word;                         -- Data read from the memory
          waddr : in  std_logic_vector(5 downto 0); -- The address to store the data
          din   : in  word;       -- Data to be stored in memory
          we    : in  std_logic); -- Enable the writing
end dmem;

architecture behavioral of dmem is

  constant MAX_BOUND: Integer := 64;

  type ramtype is array (MAX_BOUND-1 downto 0) of STD_LOGIC_VECTOR(31 downto 0);
  signal mem: ramtype;

  begin  -- architecture behavioral

      -- purpose: storing and reading data from memory
      -- inputs : raddr, waddr, din and we
      -- outputs: dout

      -- Writing data into memory
      process (clk, we, din, waddr)
        begin
          if clk'event and clk = '1' then
            if we = '1' then
              mem(conv_integer(waddr)) <= din;
            end if;
            dout <= mem(conv_integer(raddr)); -- word aligned
          end if;
      end process;

      -- Reading data from memory


end behavioral;

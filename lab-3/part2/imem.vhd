library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

library work;
use work.common.all;

entity imem is
    port(   
        addr : in std_logic_vector(3 downto 0);
        dout : out word);
end imem;

architecture behavioral of imem is
type rom_arr is array(0 to 15) of word;

constant mem:rom_arr:=
    ( 
x"12300013", -- 13003012              addi    x0,x0,0x123
x"12300013", -- 13003012              addi    x0,x0,0x123
x"12300013", -- 13003012              addi    x0,x0,0x123     # should be zero
x"00100093", -- 93001000              addi    x1,x0,1         # 1=0+1
x"00102133", -- 33211000              slt     x2,x0,x1        # 1=0<1
x"00210133", -- 33012100              add     x2,x2,x2        # 2=1+1
x"002101B3", -- B3012100              add     x3,x2,x2        # 4=2+2
x"401181B3", -- B3811140              sub     x3,x3,x1        # 3=4-1
x"00109233", -- 33921000              sll     x4,x1,x1        # 2=1<<1
x"00121233", -- 33121200              sll     x4,x4,x1        # 4=2<<1
x"00000013", -- 13000000 		nop
x"00000013", -- 13000000 		nop
x"00000013", -- 13000000 		nop
x"00000013", -- 13000000 		nop
x"00000013", -- 13000000 		nop
x"00000013"); -- 13000000 		nop

begin
	dout<=mem(conv_integer(addr));
end behavioral;

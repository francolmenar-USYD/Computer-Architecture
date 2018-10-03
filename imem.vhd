library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

library work;
use work.common.all;

entity imem is
    port(
        addr : in std_logic_vector(5 downto 0);
        dout : out word);
end  imem;

architecture behavioral of imem is
type rom_arr is array(0 to 54) of word;

constant mem:rom_arr:=
    (

    --                              main:
    --
    x"FF010113", --  0 0000 130101FF 		1111 1111 0000 0001 0000 00010 0010011    addi	   sp,sp,-16
    x"06400513", --  1 0004 13054006 		0000011 00100 00000 000  01010 0010011    li	     a0,100
    x"00112623", --  2 0008 23261100 		0000000 00001 00010 010  01100 0100011    sw	     ra,12(sp)
    x"010000EF", --  3 000c EF000001 		0 0000001000 0 00000000  00001 1101111    jal	     nprimes
    x"00C12083", --  4 0014 8320C100 		0000 0000 1100 0001 0010 0000 1000 0011   lw	     ra,12(sp)
    x"00000513", --  5 0018 13050000 		0000 0000 0000 0000 0000 0101 0001 0011   li	     a0,0
    x"01010113", --  6 001c 13010101 		0000 0001 0000 0001 0000 0001 0001 0011   addi	   sp,sp,16
    x"00008067", --  7 0020 67800000 		0000 0000 0000 0000 1000 00000  1100111   jr	     ra

     --              	            nprimes:
     --
    x"00300793", --  8 0024 93073000 		  0000 0000 0011 0000 0000 01111  0010011   li	    a5,3
    x"00050613", --  9 0028 13060500 		  000000000000 01010 000 01100    0010011   mv	    a2,a0
    x"08A7FC63", -- 10 002c 63FCA708 		  0 000100 01010 01111 111 1100 0 1100011   bleu	  a0,a5,.L2
    x"000005B7", -- 11 0030 B7050000 		  00000000000000000000      01011 0110111   lui	    a1,%hi(testarray+8)
    x"00000337", -- 12 0034 37030000      00000000000000000000      00110 0110111   lui	    t1,%hi(testarray)
    x"00858513", -- 13 0038 13858500 		  000000001000    01011 000 01010 0010011   addi    a0,a1,%lo(testarray+8)
    x"00200693", -- 14 003c 93062000 		  0000 0000 0010 0000 0000 0110 1001 0011   li	    a3,2
    x"00858593", -- 15 0040 93858500 		  0000 0000 1000 01011 000 01011  0010011   addi	  a1,a1,%lo(testarray+8)
    x"00030313", -- 16 0044 13030300 		  0000 0000 0000 0011 0000 0011 0001 0011   addi	  t1,t1,%lo(testarray)
    x"00100893", -- 17 0048 93081000 		  0000 0000 0001 0000 0000 1000 1001 0011   li	    a7,1
    x"0140006F", -- 18 004c 6F004001 		  0000 0001 0100 0000 0000 0000 0 1101111   j	      .L5
    --                               .L3:
    x"00168693", -- 19 0050 93861600 		  0000 0000 0001 0110 1000 0110 1001 0011   addi	  a3,a3,1
    x"02D687B3", -- 20 0054 B387D602 		  0000 0010 1101 0110 1000 0111 1011 0011   mul	    a5,a3,a3
    x"00458593", -- 21 0058 93854500 		  0000 0000 0100 0101 1000 0101 1001 0011   addi	  a1,a1,4
    x"04F66063", -- 22 005c 6360F604 		  0 0000100 1111 01100 110 0000 0 1100011   bgtu	  a5,a2,.L7
    --              	               .L5:
    x"0005A783", -- 23 0060 83A70500 		  0000 0000 0000 0101 1010 0111 1 0000011   lw	    a5,0(a1)
    x"FE0796E3", -- 24 0064 E39607FE 		  1 111111 00000 01111 001 0110 1 1100011   bnez	  a5,.L3
    x"00169713", -- 25 0068 13971600 		  0000 0000 0001 0110 1001 0111 0 0010011   slli	  a4,a3,1
    x"FEE662E3", -- 26 006c E362E6FE 		  1 111111 01110 01100 110 0010 1 1100011   bltu	  a2,a4,.L3
    x"00369793", -- 27 0070 93973600 		  0000 0000 0011 0110 1001 0111 1 0010011   slli	  a5,a3,3
    x"00269813", -- 28 0074 13982600 		  0000 0000 0010 0110 1001 1000 0 0010011   slli	  a6,a3,2
    x"006787B3", -- 29 0078 B3876700 		  0000 0000 0110 0111 1000 0111 1 0110011   add	    a5,a5,t1
    --              	               .L4:
    x"0117A023", -- 30 007c 23A01701 		  0000 0001 0001 0111 1010 0000 0 0100011   sw	    a7,0(a5)
    x"00D70733", -- 31 0080 3307D700 		  0000 0000 1101 0111 0000 0111 0011 0011   add	    a4,a4,a3
    x"010787B3", -- 32 0084 B3870701 		  0000 0001 0000 0111 1000 0111 1011 0011   add	    a5,a5,a6
    x"FEE67AE3", -- 33 0088 E37AE6FE 		  1 111111 01110 01100 111 1010 1 1100011   bgeu	  a2,a4,.L4
    x"00168693", -- 34 008c 93861600 		  0000 0000 0001 0110 1000 0110 1001 0011   addi	  a3,a3,1
    x"02D687B3", -- 35 0090 B387D602 		  0000 0010 1101 0110 1000 0111 1011 0011   mul	    a5,a3,a3
    x"00458593", -- 36 0094 93854500 		  0000 0000 0100 0101 1000 0101 1001 0011   addi	  a1,a1,4
    x"FCF674E3", -- 37 0098 E374F6FC 		  1 111110 01111 01100 111 0100 1 1100011   bleu	  a5,a2,.L5
    --                               .L7:
    x"00050713", -- 38 009c 13070500 		  0000 0000 0000 0101 0000 0111 0001 0011   mv	    a4,a0
    x"00200693", -- 39 00a0 93062000 		  0000 0000 0010 0000 0000 01101  0010011   li	    a3,2
    x"00000513", -- 40 00a4 13050000 		  0000 0000 0000 0000 0000 0101 0001 0011   li	    a0,0
    --                               .L6:
    x"00072783", -- 41 00a8 83270700 		  0000 0000 0000 0111 0010 0111 1000 0011   lw	    a5,0(a4)
    x"00168693", -- 42 00ac 93861600 		  0000 0000 0001 0110 1000 0110 1001 0011   addi	  a3,a3,1
    x"00470713", -- 43 00b0 13074700 		  0000 0000 0100 0111 0000 0111 0001 0011   addi	  a4,a4,4
    x"0017B793", -- 44 00b4 93B71700 		  0000 0000 0001 0111 1011 0111 1 0010011   seqz	  a5,a5
    x"00F50533", -- 45 00b8 3305F500 		  0000 0000 1111 0101 0000 0101 0011 0011   add	    a0,a0,a5
    x"FED676E3", -- 46 00bc E376D6FE 		  1 111111 01101 01100 111 0110 1 1100011  	bleu    a3,a2,.L6
    x"00008067", -- 47 00c0 67800000 		  0000 0000 0000 0000 1000 0000 0 1100111   jr      ra
    --                               .L2:
    x"00100793", -- 48 00c4 93071000 		  0000 0000 0001 0000 0000 0111 1001 0011   li	    a5,1
    x"00A7E663", -- 49 00c8 63E6A700 		  0 000000 01010 01111 110 0110 0 1100011   bgtu	  a0,a5,.L16
    x"00000513", -- 50 00cc 13050000 		  0000 0000 0000 0000 0000 0101 0001 0011   li	    a0,0
    x"00008067", -- 51 00d0 67800000 		  000000000000 00001 000 00000    1100111   jr      ra
    --
    --               	               .L16:
    x"00000737", -- 52 00d4 37070000 	    0000 0000 0000 0000 0000 0111 0 0110111  lui	a4,%hi(testarray+8)
    x"00870513", -- 53 00d8 13058700 		  0000 0000 1000 0111 0000 0101 0001 0011  addi	a0,a4,%lo(testarray+8)
    x"FC1FF06F");-- 54 00dc 6FF01FFC 		  1 1111100 000 1 11111111 00000  1101111   j	.L7
    --

                          -- .text:00000000000000c4 .L2
                          -- .text:0000000000000060 .L5
                          -- .text:000000000000009c .L7
                          -- .text:0000000000000050 .L3
                          -- .text:000000000000007c .L4
                          -- .text:00000000000000a8 .L6
                          -- .text:00000000000000d4 .L16

begin
	dout<=mem(conv_integer(addr));
end behavioral;

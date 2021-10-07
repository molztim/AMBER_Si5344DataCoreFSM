library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use std.textio.all;
use ieee.std_logic_textio.all;
use ieee.numeric_std.all;


entity textio_testbeed is
  Port ( 
    --CLK100MHZ : IN     STD_LOGIC;                    --system clock
    --sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
    --scl       : INOUT  STD_LOGIC;                   --serial clock output of i2c bus
    
    sw          : IN STD_LOGIC
    --led         : OUT std_logic := '0';
    --led2        : OUT std_logic
    
    --io0         : IN std_logic; 
    --io1         : IN std_logic 
  );
  
  
end textio_testbeed;

architecture Behavioral of textio_testbeed is

    constant filename   :string :="C:\Users\Tim\Desktop\Bachelorarbeit\Arty S7 FPGA\VIVALDO_ProjectFolder\i2cTestProjekt\25mhziicpinraus.txt";

    type DataArray is array(0 to 486*2) of std_logic_vector(15 DOWNTO 0);
    
    
    
--********************************* Convert string of var. size to std_logic_vector *************************************************    
    
    impure function tohex(rope : in string) return std_logic_vector is
        variable hexbits    : std_logic_vector(rope'length*4 - 1 DOWNTO 0); 
        variable escaperope : string(rope'length DOWNTO 1) := rope;  
        --variable i          : INTEGER := rope'length;
    begin
        for i in rope'length downto 1 loop
            hexbits(i*4-1 DOWNTO (i-1)*4) := std_logic_vector(to_unsigned(character'pos(escaperope(i)), 4));
        end loop;      
        
        return hexbits;         
    end function;
    
    
    
    impure function readmyfile(path: in string) return DataArray is
    
        File textfile : text;
        variable fileline:   line;
        variable data : DataArray;
        variable page   : std_logic_vector(15 DOWNTO 0);
        variable trash : string(2 DOWNTO 1);
        variable trash2 : string(3 DOWNTO 1);
        variable i     : Integer := 2;
        --variable j     : Integer := 0;
        
        --variable allstring  :   string(11 DOWNTO 1);


        
        begin
        
        data(0) := x"0109"; 
        data(1) :=x"4301";  --These are the orders to listen on 3.3V
        
        file_open(textfile, path, read_mode);
        
        while (not endfile(textfile)) loop 
            readline(textfile,fileline);
            --next when fileline(1) /= '0';   
            
            page(15 DOWNTO 8) :=  x"01";   
            read(fileline, trash);
            
            next when trash /= "0x";
            
            hread(fileline, page(7 DOWNTO 0));
            
            IF page = data(i-2) THEN
                hread(fileline, data(i)(15 DOWNTO 8));
                read(fileline, trash2);
                hread(fileline, data(i)(7 DOWNTO 0));
                i := i+1;
            ELSE
                data(i) := page;              
                hread(fileline, data(i+1)(15 DOWNTO 8));
                read(fileline, trash2);
                hread(fileline, data(i+1)(7 DOWNTO 0));
                i := i+2;
            END IF;
                    
                    
            --read(fileline,allstring);
           --page(15 DOWNTO 8) := x"01";
           --page(7 DOWNTO 0) := tohex(allstring(9 DOWNTO 8));
           --data(i) := page;
           --data(i+1)(15 DOWNTO 8) := tohex(allstring(7 DOWNTO 6));
           --data(i+1)(7 DOWNTO 0) := tohex(allstring(2 DOWNTO 1));
           --i := i+2;  
    
        end loop;
        
                   

        file_close(textfile);        
        return data;
        --return allstring;       
        end function; 
        
        signal Data: DataArray := readmyfile(filename);
        --signal external : string(11 DOWNTO 1) :=readmyfile(filename) ;
       -- signal sub      : string(2 DOWNTo 1)    := external(9 DOWNTO 8);
       -- signal sub2      : string(2 DOWNTo 1)    := external(7 DOWNTO 6);
       -- signal sub3      : string(2 DOWNTo 1)    := external(2 DOWNTO 1);

      --signal stringtest   : string(5 DOWNTO 1) := "abcde";
        --signal stringtest2  : string(3 DOWNTO 1) := stringtest(3 DOWNTO 1);
    
begin

process(sw)
begin

end process;
  

end Behavioral;

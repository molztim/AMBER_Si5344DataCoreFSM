library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use std.textio.all;
use ieee.std_logic_textio.all;


entity i2c_clockimport is
  Port ( fulldata : OUT DataArray;
         datalength : OUT INTEGER);
end i2c_clockimport;

--*************************************** Import Logic with TextIO **************************************************************
    constant filename   :string :="25mhziicpinraus.txt";

    type DataArray is array(0 to 486*2) of std_logic_vector(15 DOWNTO 0);
    
    
    impure function readmyfile(path: in string) return DataArray is
    
        File textfile : text;
        variable fileline:   line;
        variable data : DataArray;
        variable page   : std_logic_vector(15 DOWNTO 0);
        variable trash : string(2 DOWNTO 1);
        variable trash2 : string(3 DOWNTO 1);
        variable i     : Integer := 2;
        --variable j     : Integer := 0;


        
        begin
        
        data(0) := x"0109"; 
        data(1) :=x"4301";  --These are the orders to listen on 3.3V
        
        file_open(textfile, path, read_mode);
        
        while (not endfile(textfile)) loop 
           readline(textfile,fileline);
           next when fileline(1) /= '0';       
           
           page(15 DOWNTO 8) :=  x"01";   
           read(fileline, trash);
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
                    
        end loop;
        
        file_close(textfile);        
        return data;
               
        end function; 
        
        signal Data: DataArray := readmyfile(filename);

--******************************************************************************************************************************   


architecture Behavioral of i2c_clockimport is

begin


end Behavioral;

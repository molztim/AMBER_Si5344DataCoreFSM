library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity i2c_oszitest is
  Port ( 
    CLK100MHZ : IN     STD_LOGIC;                    --system clock
    sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
    scl       : INOUT  STD_LOGIC;                   --serial clock output of i2c bus
    
    led         : OUT std_logic
    --sw          : IN STD_LOGIC
  
  );
  
  
end i2c_oszitest;

architecture Behavioral of i2c_oszitest is

SIGNAL TRIGGER_CHANGE   :   STD_LOGIC;
SIGNAL COUNTER          :   INTEGER := 0;
SIGNAL scl_out          :   STD_LOGIC := '1'; 


begin



PROCESS(CLK100MHZ)
BEGIN

    IF rising_edge(CLK100MHZ) THEN
        IF counter = 10_000_000 THEN
            counter <= 0;
            led <= '1';
            scl_out <= NOT scl_out;
        ELSE
            counter <= counter +1;
        END IF;       
    END IF;
    
    IF falling_edge(CLK100MHZ) THEN
        TRIGGER_CHANGE<= '0';
    END IF;
END PROCESS;

scl <= '0' WHEN (scl_out = '0' ) ELSE 'Z';
sda <= '0' WHEN (scl_out = '0' ) ELSE 'Z';

end Behavioral;

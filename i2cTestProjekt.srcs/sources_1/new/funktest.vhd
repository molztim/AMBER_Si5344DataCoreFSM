library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


entity funktest is
    Port ( 
    INT_OUT :   OUT   POSITIVE;
    TRIGGER :   IN STD_LOGIC
    );
end funktest;

architecture Behavioral of funktest is

SIGNAL INT  : POSITIVE := 0;

begin

PROCESS(TRIGGER)
BEGIN
    IF TRIGGER = '1' THEN
    INT <= INT + 1;
    INT_OUT <= INT +1;
    END IF;
END PROCESS;
end Behavioral;

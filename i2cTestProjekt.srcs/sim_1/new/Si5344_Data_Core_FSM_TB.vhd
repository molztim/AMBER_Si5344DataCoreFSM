
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


entity Si5344_Data_Core_FSM_TB is
  Port (    
    sda       : INOUT  STD_LOGIC;                   --serial data output of i2c bus
    scl       : INOUT  STD_LOGIC                   --serial clock output of i2c bus 
    );
end Si5344_Data_Core_FSM_TB;

architecture Behavioral of Si5344_Data_Core_FSM_TB is

COMPONENT Si5344_Data_Core_FSM IS 
    GENERIC(
        clkfreq : INTEGER;                           --input clock frequency in Hz. Should come form the board
        bus_clk   : INTEGER;                              -- Bus clk freq in Hz
        adress : IN std_logic_vector(6 DOWNTO 0)     -- External adress to set
    );
    PORT ( 
        CLK100MHZ : IN     STD_LOGIC;                   --system clock
        sda       : INOUT  STD_LOGIC;                   --serial data output of i2c bus
        scl       : INOUT  STD_LOGIC;                   --serial clock output of i2c bus
        
        sw          : IN STD_LOGIC                    -- external control signal for start/stop/restart  
        
    );
END COMPONENT;

CONSTANT clkfreq : INTEGER := 100_000_000;
CONSTANT bus_clk : INTEGER := 100_000;
CONSTANT address :  std_logic_vector(6 DOWNTO 0) := "1101000";

SIGNAL dummy_clk    :   std_logic := '0'; 
SIGNAL ENA  : std_logic := '0';

begin

TB : Si5344_Data_Core_FSM
    generic map(
    clkfreq => clkfreq,
    bus_clk => bus_clk,
    adress => address
    
    )
    port map(
        CLK100MHZ => dummy_clk,
        sda => sda,
        scl => scl,
        sw => ENA
    );

process
begin

dummy_clk <= not dummy_clk;
wait for 5 ns;
dummy_clk <= NOT dummy_clk;
wait for 5 ns;

end process;

process
begin

wait for 1 us;
ENA <= '1';
wait for 600 ms;
ENA <= '0';
wait for 30 ms;
ENA <= '1';
wait for 600 ms;

end process;


end Behavioral;

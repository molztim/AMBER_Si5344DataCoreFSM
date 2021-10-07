library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


entity Si5344_Data_Core_TB is
  Port (
        --CLK100MHZ : IN     STD_LOGIC;                    --system clock  
        sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
        scl       : INOUT  STD_LOGIC;                  --serial clock output of i2c bus 
        
        out_data    : OUT std_logic_vector(7 DOWNTO 0);
        out_IO_Flag      : OUT std_logic; 
        
        out_cyclecounter : OUT INTEGER;
        out_syscounter   : OUT INTEGER;
        out_datacounter  : OUT INTEGER;          
        
        out_WAIT_FLAG    : OUT std_logic;
        out_count : OUT INTEGER
        
    );
end Si5344_Data_Core_TB;

architecture Behavioral of Si5344_Data_Core_TB is


    component Si5344_Data_Core is
      Port ( 
        CLK100MHZ : IN     STD_LOGIC;                    --system clock
        sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
        scl       : INOUT  STD_LOGIC;                  --serial clock output of i2c bus
        
        sw          : IN STD_LOGIC;                      -- external control signal for start/stop/restart
        
        out_data    : OUT std_logic_vector(7 DOWNTO 0);
        out_IO_Flag      : OUT std_logic; 
        
        out_cyclecounter : OUT INTEGER;
        out_syscounter   : OUT INTEGER;
        out_datacounter  : OUT INTEGER;          
        
        out_WAIT_FLAG    : OUT std_logic;
        out_count : OUT INTEGER
      );
    end component;
    
    SIGNAL clk : std_logic := '0';
    SIGNAL ENA  :   std_logic := '0';
    
begin

    TB_object : Si5344_Data_Core
        port map(
            CLK100MHZ => clk,
            sda => sda,
            scl => scl,
            sw => ENA,
            out_data    => out_data,
            out_IO_Flag   =>  out_IO_Flag,
            
            out_cyclecounter =>out_cyclecounter,
            out_syscounter   =>out_syscounter,
            out_datacounter    =>     out_datacounter,   
            
            out_WAIT_FLAG   =>out_WAIT_FLAG,
            out_count =>out_count
        
        );

PROCESS
BEGIN

clk <= not clk;
wait for 5 ns;
clk <= NOT clk;
wait for 5 ns;

END PROCESS;

PROCESS
BEGIN

wait for 1 us;
ENA <= '1';
wait for 1 sec;
ENA <= '0';
wait for 1 us; 
ENA <= '1';

END PROCESS;

end Behavioral;

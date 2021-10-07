library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


entity hirachy_sim is
  Port (         CLK100MHZ  : in STD_LOGIC;
                 sda        : inout std_logic;
                 scl        : INOUT  STD_LOGIC;                   --serial clock output of i2c bus
    
                sw          : IN STD_LOGIC;
                led         : OUT std_logic := '0';
                led2        : OUT std_logic :='0'
             );
end hirachy_sim;

architecture Behavioral of hirachy_sim is

COMPONENT PreData
        Port ( 
            DATA    :   OUT STD_LOGIC_VECTOR( (1*8)-1 DOWNTO 0);
            READ_IN :   IN  STD_LOGIC;    
            FLAG    :   OUT std_logic;
            WAITS   :   OUT STD_LOGIC
                   
            );
END COMPONENT;

SIGNAL SIG_OUT    : STD_LOGIC_VECTOR(7 DOWNTO 0);   
SIGNAL CTRL_LOGIC   :   STD_LOGIC := '0';
SIGNAL PREV_CTRL_LOGIC   :   STD_LOGIC := '0';
SIGNAL ENA          : STD_LOGIC := '0';

SIGNAL TRIGGER_CHANGE   :   STD_LOGIC;
SIGNAL COUNTER          :   INTEGER := 0;
SIGNAL DATA_COUNTER     :   INTEGER := 7;     

SIGNAL OUT_SYS      :   STD_LOGIC := '1';

SIGNAL END_FLAG     : STD_LOGIC;
SIGNAL WAIT_FLAG    : STD_LOGIC;


TYPE machine IS(ready, load, write); --needed states
SIGNAL state         : machine;      

begin

test : PreData PORT MAP (READ_IN => ENA, DATA => SIG_OUT,FLAG => END_FLAG, WAITS => WAIT_FLAG);

PROCESS(CLK100MHZ)
BEGIN
    PREV_CTRL_LOGIC <= CTRL_LOGIC;
    IF rising_edge(CLK100MHZ) THEN
        IF counter = 50_000 THEN
            counter <= 0;
            CTRL_LOGIC <= NOT CTRL_LOGIC;
        ELSE
            counter <= counter +1;
        END IF;       
    END IF;
END PROCESS;

PROCESS(CTRL_LOGIC)
BEGIN

IF PREV_CTRL_LOGIC = '0' AND CTRL_LOGIC = '1' THEN
    CASE state IS
        WHEN ready => 
            state <= load;
            ENA <= '1';
            
        WHEN load =>
            ENA <= '0';
            state <= write;
            OUT_SYS <= SIG_OUT(DATA_COUNTER);
            DATA_COUNTER <= DATA_COUNTER - 1;
            
        WHEN write =>
            IF DATA_COUNTER = 0 THEN
                OUT_SYS <= SIG_OUT(0);
                state <= load;
                DATA_COUNTER <= 7;
                ENA <= '1';
            ELSE    
                OUT_SYS <= SIG_OUT(DATA_COUNTER);
                DATA_COUNTER <= DATA_COUNTER - 1;
            END IF;
        WHEN OTHERS =>
            NULL;
    END CASE;
END IF;


END PROCESS;

sda <=  '0' WHEN OUT_SYS = '0' ELSE 'Z';

end Behavioral;

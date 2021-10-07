LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;


entity testbench_i2cmaster is
--  Port ( );
end testbench_i2cmaster;

architecture Behavioral of testbench_i2cmaster is
    
    CONSTANT half_period : time := 5 ns;
    
    SIGNAL SDA : STD_LOGIC;
    SIGNAL SCL : STD_LOGIC;
    SIGNAL clk       : STD_LOGIC := '0';                    --system clock
    SIGNAL reset_n   : STD_LOGIC  := '1';                    --active low reset
    SIGNAL ena        : STD_LOGIC := '1';                    --latch in command
    SIGNAL addr      : STD_LOGIC_VECTOR(6 DOWNTO 0) := "1101000"; --address of target slave
    SIGNAL rw        : STD_LOGIC := '0';                    --'0' is write, '1' is read
    SIGNAL data_wr   : STD_LOGIC_VECTOR(7 DOWNTO 0) := x"02"; --data to write to slave
    SIGNAL busy      : STD_LOGIC;                    --indicates transaction in progress
    SIGNAL data_rd   : STD_LOGIC_VECTOR(7 DOWNTO 0) := x"00"; --data read from slave
    SIGNAL ack_error_test : STD_LOGIC;                    --flag if improper acknowledge from slave
    SIGNAL led       : STD_LOGIC := '0';

    COMPONENT i2c_master IS
    port(
    clk       : IN     STD_LOGIC;                    --system clock
    reset_n   : IN     STD_LOGIC;                    --active low reset
    ena       : IN     STD_LOGIC;                    --latch in command
    addr      : IN     STD_LOGIC_VECTOR(6 DOWNTO 0); --address of target slave
    rw        : IN     STD_LOGIC;                    --'0' is write, '1' is read
    data_wr   : IN     STD_LOGIC_VECTOR(7 DOWNTO 0); --data to write to slave
    busy      : OUT    STD_LOGIC;                    --indicates transaction in progress
    data_rd   : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0); --data read from slave
    ack_error : OUT STD_LOGIC;                    --flag if improper acknowledge from slave
    sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
    scl       : INOUT  STD_LOGIC;
    led       : OUT STD_LOGIC
    );
    END COMPONENT;

begin
    
        IO : i2c_master 
        port map(
            clk => clk,
            reset_n => reset_n,
            ena => ena,
            addr => addr,
            rw => rw,
            data_wr => data_wr,
            busy => busy,
            data_rd =>data_rd,
            ack_error =>ack_error_test,
            sda => sda,
            scl => scl,
            led => led
            );
    
    
    
    clk <= not clk after half_period;

end Behavioral;

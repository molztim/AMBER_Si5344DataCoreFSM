library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use std.textio.all;
use ieee.std_logic_textio.all;

--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Disclaimer &&&&&&&&&&&&&&&&&&&&&&&&&
--
-- This Core was the first one to work. It NEEDS a switch input on port sw and the 25mhziicpinraus.txt
-- to work. otherwise this thing is fine, just not finished
--





entity I2C_TopCore is
  Port ( 
    CLK100MHZ : IN     STD_LOGIC;                    --system clock
    sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
    scl       : INOUT  STD_LOGIC;                  --serial clock output of i2c bus
    
    sw          : IN STD_LOGIC
    --led         : OUT std_logic;
    --led2        : OUT std_logic
    
    --io0         : IN std_logic; 
    --io1         : IN std_logic 
    --go            : In std_logic;
    --adress        : IN std_logic_vector(6 DOWNTO 0) := "1101000";
    --rwb           : IN std_logic := '0'
  );
end I2C_TopCore;

architecture Behavioral of I2C_TopCore is
    
--**************************************** These Signals are for top-level use. If not top level, pls activate ports ************************

    SIGNAL adress        : std_logic_vector(6 DOWNTO 0) := "1101000"; 
    SIGNAL rwb           : std_logic := '0'; 
    --SIGNAL sw               : std_logic := '1';
    

--*************************************** Import Logic with TextIO **************************************************************
    constant filename   :string :="C:\Users\Tim\Desktop\Bachelorarbeit\Arty S7 FPGA\VIVALDO_ProjectFolder\i2cTestProjekt\25mhziicpinraus.txt";

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
        variable c      : INTEGER := 2;
        variable page_tracker : std_logic_vector(15 DOWNTO 0) :=x"0109";       -- A simple var to track the currentpage. Thx @Ca


        
        begin
        
        data(0) := x"0109"; 
        data(1) :=x"4301";  --These are the orders to listen on 3.3V
        data(2 TO 486*2) := (others => x"0000");
        
        file_open(textfile, path, read_mode);
        
        while (not endfile(textfile)) loop 
           readline(textfile,fileline);   
           
           page(15 DOWNTO 8) :=  x"01";   
           read(fileline, trash);
           
           next when trash /= "0x";
           
           hread(fileline, page(7 DOWNTO 0));
           
           IF page = page_tracker THEN             -- data(i-2) 
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
                page_tracker := page;
           END IF;
                    
        end loop;
        
        file_close(textfile);  
          
        return data;
               
        end function; 
        
        signal Data: DataArray := readmyfile(filename);

--******************************************************************************************************************************   
        
        CONSTANT clkfreq : INTEGER :=100_000_000; --The clock frequency in Hz of the used core
        CONSTANT datalength : INTEGER := 2;

               
        type TestArray is array(0 to 2) of std_logic_vector(15 DOWNTO 0);
        --SIGNAL test_array : TestArray :=  (x"0111",x"1c1d",x"11aa"); 
        SIGNAL data_byte  :   std_logic_vector(7 DOWNTO 0) := x"01";
        
               
        --SIGNAL sda_buffer   :   std_logic;
        --SIGNAL scl_buffer   :   std_logic;
        --SIGNAL scl_trigger  : std_logic;
        
        SIGNAL IO_Flag      : std_logic := '0';
        SIGNAL IO_reset     : std_logic := '1';
        SIGNAL IO_busy      : std_logic;
        SIGNAL IO_read      : std_logic_vector(7 DOWNTO 0);
        SIGNAL IO_ack_error : std_logic;
        
        SIGNAL cyclecounter : INTEGER RANGE 0 TO 9 + datalength * 9 := 0;
        SIGNAL syscounter   : INTEGER RANGE 0 To datalength + 1 := 0;
        SIGNAL datacounter  : INTEGER RANGE 0 TO Data'length := 0;
        
        SIGNAL WAIT_FLAG    : std_logic := '0';        
        
        SIGNAL max_long : INTEGER := 350*10e5;        --350ms between main body and preamble
        SIGNAL max_short : INTEGER := 150000;       --1.50ms between transmissions
        SIGNAL max_counter    :   INTEGER := max_long;
        SIGNAL count : INTEGER RANGE 0 TO 350*10e5  := 0;     --classic counter for wait
        
        
        
        
--************************* Timer as replacement of clocking on SCL *******************************************************      

        SIGNAL timing_counter   : INTEGER := 0;
        SIGNAL SCL_counts       : INTEGER := 1000; --SCL freq. / CLK freq. 100khz/100Mhz
        SIGNAL start_counts     : INTEGER := 3000;
        SIGNAL counter_clk      : std_logic := '0';

        SIGNAL RUN_FLAG         : std_logic := '0';
        --SIGNAL DEBUG_FLAG       : std_logic := '0';


--********************************************** Import component ****************************************************  
        COMPONENT i2c_master IS
            port(
            clk       : IN     STD_LOGIC;                    --system clock
            reset_n   : IN     STD_LOGIC := '1';                    --active low reset
            ena       : IN     STD_LOGIC;                    --latch in command
            addr      : IN     STD_LOGIC_VECTOR(6 DOWNTO 0); --address of target slave
            rw        : IN     STD_LOGIC;                    --'0' is write, '1' is read
            data_wr   : IN     STD_LOGIC_VECTOR(7 DOWNTO 0); --data to write to slave
            busy      : OUT    STD_LOGIC;                    --indicates transaction in progress
            data_rd   : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0); --data read from slave
            ack_error : BUFFER STD_LOGIC;                    --flag if improper acknowledge from slave
            sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
            scl       : INOUT  STD_LOGIC
            
            );
            END COMPONENT;
         

begin
    --scl <= scl_buffer;
    --scl_trigger <= '0' when scl_buffer = '0' else '1';
    
    IO : i2c_master 
        port map(
            clk => CLK100MHZ,
            reset_n => IO_reset,
            ena => IO_Flag,
            addr => adress,
            rw => rwb,
            data_wr => data_byte,
            busy => IO_busy,
            data_rd =>IO_read,
            ack_error =>IO_ack_error,
            sda => sda,
            scl => scl);


PROCESS(CLK100MHZ, sw, datacounter)              --Main working logic

begin

IF sw = '0' THEN                -- Hold system
       IO_Flag          <= '0';
       IO_reset         <= '0';
       --counter_clk      <= '0';
       --timing_counter   <= 0;
       --syscounter       <= 0;
       --datacounter      <= 0;
       --cyclecounter     <= 0;
       --data_byte        <= Data(0)(15 DOWNTO 8);

ELSIF rising_edge(CLK100MHZ) AND datacounter < 972   THEN        --Main clock logic
       IO_reset         <= '1';
    
    --DEBUG_FLAG <= scl_trigger XOR counter_clk;
      
    IF WAIT_FLAG = '0' THEN  
    
        IF cyclecounter = 5  AND syscounter = datalength THEN
            IO_Flag <= '0';
            
        ELSIF datacounter =  Data'length OR data(datacounter) = x"0000" THEN
            IO_Flag <= '0';
           
        ELSIF cyclecounter = 0 THEN
            IO_Flag <= '1';
        ELSIF cyclecounter = 9 AND syscounter = datalength THEN                                                   
                --WAIT_FLAG <= '1';
                IF datacounter = 7 THEN
                    max_counter <= max_long;
                 ELSE
                    max_counter <= max_short;
                 END IF; 
        END IF;
        
        -- This is the clock part that will replace the scl as a clock
        
        IF timing_counter = 1499 AND RUN_FLAG = '0' THEN
            counter_clk <= NOT counter_clk;
            timing_counter <= 0;
            RUN_FLAG <= '1';
        ELSIF timing_counter = 500 AND RUN_FLAG = '1' AND counter_clk = '1' THEN        --scl_counts /2
            counter_clk <= NOT counter_clk;
            timing_counter <= 0;
        ELSIF timing_counter = 500-1 AND RUN_FLAG = '1' AND counter_clk = '0' THEN
            counter_clk <= NOT counter_clk;
            timing_counter <= 0;
            
            IF cyclecounter = 9 and syscounter = datalength THEN
                WAIT_FLAG <= '1';
                RUN_FLAG <= '0';
            END IF;   
                    
        ELSE
            timing_counter <= timing_counter + 1;   
        END IF;
                
        
        
    ELSE
        IO_Flag <= '0';
        IF count = max_counter THEN
            WAIT_FLAG <= '0';
            count <= 0;
            --led <= '1';
        else
            count <= count + 1;
        END IF;
        
    
    END IF;
    
END IF;

END PROCESS;




PROCESS(counter_clk,sw)
begin
    IF(rising_edge(counter_clk) AND sw = '1' AND datacounter < 972) THEN
        
        
        IF cyclecounter = 5 AND syscounter /= 0 THEN
            IF syscounter = datalength THEN
              
                
                IF datacounter /= (Data'length -1) THEN
                    data_byte <= Data(datacounter+1)((datalength - 0)* 8 -1 DOWNTO (datalength - 0 - 1)*8);             
                ELSE
                    NULL;
                END IF;
                
                datacounter <= datacounter +1; 
                                
                
            ELSE
                data_byte <= Data(datacounter)((datalength - syscounter)* 8 -1 DOWNTO (datalength - syscounter - 1)*8);    
            END IF;   
        END IF;
        
        
        IF cyclecounter = 9 THEN        
            IF syscounter = datalength THEN
                syscounter <= 0;
                cyclecounter <= 0;
                --WAIT_FLAG <= '1';
            ELSE
                syscounter <= syscounter + 1;
                cyclecounter <= 1;
            END IF;            
        ELSE
            cyclecounter <= cyclecounter + 1;
        END IF;
        
        
    END IF;
END PROCESS;


end Behavioral;

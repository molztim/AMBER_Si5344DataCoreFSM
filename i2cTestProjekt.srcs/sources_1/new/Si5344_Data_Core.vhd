library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use std.textio.all;
use ieee.std_logic_textio.all;


entity Si5344_Data_Core is
  Port ( 
    CLK100MHZ : IN     STD_LOGIC;                    --system clock
    sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
    scl       : INOUT  STD_LOGIC;                  --serial clock output of i2c bus
    
    sw          : IN STD_LOGIC;                      -- external control signal for start/stop/restart
   
   
-- Testbench stuff    
    out_data    : OUT std_logic_vector(7 DOWNTO 0);
    out_IO_Flag      : OUT std_logic; 
    
    out_cyclecounter : OUT INTEGER;
    out_syscounter   : OUT INTEGER;
    out_datacounter  : OUT INTEGER;          
    
    out_WAIT_FLAG    : OUT std_logic;
    out_count : OUT INTEGER
    
  );
end Si5344_Data_Core;

architecture Behavioral of Si5344_Data_Core is
    
--**************************************** These Signals are for top-level use. If not top level, pls activate ports ************************

    SIGNAL adress        : std_logic_vector(6 DOWNTO 0) := "1101000";       -- standard address of EVB. All 3 pins have to be GND
    SIGNAL rwb           : std_logic := '0';                                -- R/W bit. 0 = write    

--*************************************** Import Logic with TextIO **************************************************************
    constant filename   :string :="130-150-carlosV1-Registers.txt";         -- Filename of the file with pages, registers and values. Pls place in src folder for relative pathes to work

    type DataArray is array(0 to 486*2) of std_logic_vector(15 DOWNTO 0);   -- The might Data Array. 2*486 because its possible to get for each register/value a new page
    
    
    impure function readmyfile(path: in string) return DataArray is         -- Function to import data
    
        File textfile : text;                                               -- File Object for TextIO
        variable fileline:   line;                                          -- Line Object for TextIO
        variable data : DataArray;                                          -- The local data Array to work with
        variable page   : std_logic_vector(15 DOWNTO 0);                    -- The page vector to store the page change command. Also used to check if a page change is neccessary
        variable trash : string(2 DOWNTO 1);                                -- Trash variable. Because hread has to read bit by bit, some bits have to be trashed
        variable trash2 : string(3 DOWNTO 1);                               -- 2nd Trash string
        variable i     : Integer := 2;                                      -- A counter to keep track of which data it is
        variable page_tracker : std_logic_vector(15 DOWNTO 0) :=x"0109";    -- A simple var to track the last page in order to compare it. Thx @Carlos


        
        begin
        
        data(0) := x"0109";                                                 -- The first command is set. Page 09 Reg 43 value 01 to set pins to 3.3V 
        data(1) :=x"4301";                  
        data(2 TO 486*2) := (others => x"0000");                            -- Rest is filled with x0000. If the codes later comes across x0000, it knows, its at the end of data, since afterwards is everything x0000
        
        file_open(textfile, path, read_mode);                               -- Open File
        
        while (not endfile(textfile)) loop                                  -- Start Looping through the file lines
           readline(textfile,fileline);                                     -- Read one line
           
           page(15 DOWNTO 8) :=  x"01";                                     -- Reset page to x01XX
           read(fileline, trash);                                           -- Read the first two characters of the line
           
           next when trash /= "0x";                                         -- If character 1 and 2 of the line is not 0x, than its a comment or something else. So just ignore
           
           hread(fileline, page(7 DOWNTO 0));                               -- Read in the characters 3 and 4 as hexadecimal in the page, because there is the bits for page. Its guaranteed they are hex, otherwise the command before would had terminated everything    
           
           IF page = page_tracker THEN                                      -- If the current page is equal to the last page, go ahead
               hread(fileline, data(i)(15 DOWNTO 8));                       -- Read in character 5 and 6 as the first byte of data. They are the register 
               read(fileline, trash2);                                      -- Ignore 7 - 9 charatcer; there are just for format
               hread(fileline, data(i)(7 DOWNTO 0));                        -- Read char 10 and 11 in as hex for the last byte of data. They represent the value
               i := i+1;                                                    -- Iterate onces further through the data
           ELSE                                                             -- If the page is not the same as before go with    
                data(i) := page;                                            -- Save the page command in the current array elemnt
                hread(fileline, data(i+1)(15 DOWNTO 8));                    -- Save char 5 and 6 in the next array element (thats the regsiter)
                read(fileline, trash2);                                     -- Trahs 7-9
                hread(fileline, data(i+1)(7 DOWNTO 0));                     -- Save char 10 and 11 in the next array element as well (thats the value)
                i := i+2;                                                   -- Now we have wirtten 2 array elements, so we have to jump two iterations
                page_tracker := page;                                       -- Update to new page
           END IF;
                    
        end loop;
        
        file_close(textfile);                                               -- Close file
          
        return data;                                                        -- Return the Array with all the data the I2C Core needs                   
               
        end function; 
        
        signal Data: DataArray := readmyfile(filename);                     -- Initialise Data as the main data Array with the function above

--******************************************************************************************************************************   
-- In order to change the frequencies, pls change internal_clk and scl_clk to the desired interal freq and scl to the desired scl frequency

        
        CONSTANT internal_clk : INTEGER := 100_000_000;                     -- The frequency in Hz of the on board clock 
        CONSTANT scl_clk    : INTEGER := 100_000;                           -- SCL frequency in Hz                      
        CONSTANT datalength : INTEGER := 2;                                 -- How many Bytes per transmission?

        SIGNAL data_byte  :   std_logic_vector(7 DOWNTO 0) := x"01";        -- data_byte will grab the data/hexnumbers from 'Data# and transmit them to the I2C Core

        SIGNAL IO_Flag      : std_logic := '0';                             -- For enabeling and disabeling the IO Component
        SIGNAL IO_reset     : std_logic := '1';                             -- To Reset/Hold the IO Component. Used for restarts
        SIGNAL IO_busy      : std_logic;                                    -- A not used busy flag from the IO Component. Could have used it, worked out without it
        SIGNAL IO_read      : std_logic_vector(7 DOWNTO 0);                 -- The byte if there was something to read from IO. But this functionality was not used
        SIGNAL IO_ack_error : std_logic;                                    -- Bit for an ACK error. Also not used, but could to restart the sentence
        
        SIGNAL cyclecounter : INTEGER RANGE 0 TO 9 + datalength * 9 := 0;   -- Counts the amount of cycles/SCL raising edges per byte
        SIGNAL syscounter   : INTEGER RANGE 0 To datalength + 1 := 0;       -- Counts the amount of bytes + address send per sentences
        SIGNAL datacounter  : INTEGER RANGE 0 TO Data'length := 0;          -- Counts the amount of transmissions done
            
        SIGNAL WAIT_FLAG    : std_logic := '0';                             -- Flag to wait between the transmissions/sentences    
        
        CONSTANT max_long : INTEGER := 350e5;                             -- 350ms between main body and preamble
        CONSTANT max_short : INTEGER :=  15000;                             -- 1.50ms between transmissions
        SIGNAL max_counter : INTEGER := max_long;                           -- Variable maximum of counter
        SIGNAL count : INTEGER RANGE 0 TO max_long  := 0;                   -- classic counter for wait
        
        
        
        
--************************* Timer as replacement of clocking on SCL *******************************************************      
-- IMPORTANT: the development board had 100mhz clk and 100khz on SCL. Pls adjust 'half_period' accordung to your specs.


        SIGNAL timing_counter   : INTEGER := 0;                             -- The counter for the clock that runs parralell to the SCL and counts the time in order to guess when which action was taken
        CONSTANT half_period    : INTEGER := internal_clk/scl_clk / 2;       -- The amount of cycles necessary to count a half period of the SCL. for 100khz on scl and 100mhz on clk its 500. Change THIS to adjust to different clock frequencies!!!
        SIGNAL counter_clk      : std_logic := '0';                         -- logical Output of the clock
        SIGNAL RUN_FLAG         : std_logic := '0';                         -- Flag to monitor if the start signal is done and the counter clock has to start

--********************************************** Import component ****************************************************  
        COMPONENT i2c_master IS                         
            port(
            clk       : IN     STD_LOGIC;                                   --system clock
            reset_n   : IN     STD_LOGIC := '1';                            --active low reset
            ena       : IN     STD_LOGIC;                                   --latch in command
            addr      : IN     STD_LOGIC_VECTOR(6 DOWNTO 0);                --address of target slave
            rw        : IN     STD_LOGIC;                                   --'0' is write, '1' is read
            data_wr   : IN     STD_LOGIC_VECTOR(7 DOWNTO 0);                --data to write to slave
            busy      : OUT    STD_LOGIC;                                   --indicates transaction in progress
            data_rd   : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0);                --data read from slave
            ack_error : BUFFER STD_LOGIC;                                   --flag if improper acknowledge from slave
            sda       : INOUT  STD_LOGIC;                                   --serial data output of i2c bus
            scl       : INOUT  STD_LOGIC
            
            );
            END COMPONENT;
         

begin
    
    IO : i2c_master                                                         -- The component of the transmitting core
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


PROCESS(CLK100MHZ, sw, datacounter)                             -- 1st working logic. Everything has to be triggered on 100MHZ CLK!

begin

IF sw = '0' THEN                                                 -- Hold and reset the system
       IO_Flag          <= '0';
       IO_reset         <= '0';
       counter_clk      <= '0';
       timing_counter   <= 0;


ELSIF rising_edge(CLK100MHZ) AND datacounter < 972   THEN        -- Main clock logic. runs on the 100MHZ clock and as long as the data counter doesnt go byond the limit of the Data Array
       IO_reset         <= '1';
    
-- This is for general prupose controll of the I2C Transmission Core and for setting the number for the wait counter
      
    IF WAIT_FLAG = '0' THEN                                     -- If not in the wait state
    
        IF cyclecounter = 5  AND syscounter = datalength THEN   -- When 5 cycles are done and address + datalenght transmissions ahve been done: tell IO to stop working
            IO_Flag <= '0';
            
        ELSIF datacounter =  Data'length OR data(datacounter) = x"0000" THEN    -- When datacounter = leght of data or the 0x0000 starts: Also stop transmitting
            IO_Flag <= '0';
           
        ELSIF cyclecounter = 0 THEN                             -- Start transmitting when cycle  = 0
            IO_Flag <= '1';
        ELSIF cyclecounter = 9 AND syscounter = datalength THEN       -- When last cycle and address + datalength bits have bee send: pause                                            
                IF datacounter = 7 THEN                               -- When the first 7 transmssions are done: Get ready for the long 300ms pause
                    max_counter <= max_long;                            
                 ELSE                                                 -- if not: ready  up for aregular pause  
                    max_counter <= max_short;
                 END IF; 
        END IF;
        
-- This is the clock part that will replace the scl as a clock to trigger the counter of the cycle in order to tell the 2nd part that a cycle on the scl has passed
        
        IF timing_counter = (half_period* 3 - 1) AND RUN_FLAG = '0' THEN                        -- A buffer for the time the clock needs to not work on the START Signal
            counter_clk <= NOT counter_clk;                                                     -- Set next logic on counter_clk
            timing_counter <= 0;                                                                -- start new counting
            RUN_FLAG <= '1';                                                                    -- Exit this start setup
        ELSIF timing_counter = half_period AND RUN_FLAG = '1' AND counter_clk = '1' THEN        -- Regular counter to count when the SCL has done half a cycle. 1st half
            counter_clk <= NOT counter_clk;                                                     -- Set next logic
            timing_counter <= 0;                                                                -- Reset counter
        ELSIF timing_counter = half_period-1 AND RUN_FLAG = '1' AND counter_clk = '0' THEN      -- Regualr counter for 2nd half of scl cycle. Build for 100khz
            counter_clk <= NOT counter_clk;
            timing_counter <= 0;
            
            IF cyclecounter = 9 and syscounter = datalength THEN                        -- When 9 sclcycles are done and address + datalenght worth of transmssion: start pause
                WAIT_FLAG <= '1';                                                       -- Wait flag to waiting
                RUN_FLAG <= '0';                                                        -- Reset Run flag, so on next transmssion the code knows he has to buffer the start signal
            END IF;   
                    
        ELSE
            timing_counter <= timing_counter + 1;                                       -- Or if not, just count one up
        END IF;
                
-- The wait counter when to wait between the transmissions        
        
    ELSE                                                                                -- Or if its time to wait:
        IO_Flag <= '0';                                                                 -- Deactivate the OI Component to stop all transmission
        IF count = max_counter THEN                                                     -- Simple counter when to stop waiting
            WAIT_FLAG <= '0';                                                           -- Stop by dactivating the Wait flag
            count <= 0;
        else
            count <= count + 1;                                                         -- Else just count up
        END IF;
        
    
    END IF;


-- Just test bench stuff
    out_data  <= data_byte;
    out_IO_Flag     <= IO_Flag;
    
    out_cyclecounter <= cyclecounter;
    out_syscounter  <= syscounter;
    out_datacounter <= datacounter;          
    
    out_WAIT_FLAG    <=WAIT_Flag;
    out_count   <= count;
    
END IF;

END PROCESS;




PROCESS(counter_clk,sw)                                                             -- Here are all processes that have to run on the counter_clk aka when the SCL has done a cycle
begin
    IF sw = '0' THEN                                                                -- Reset for all signals inside this process when external says hold     
        cyclecounter <= 0;
        data_byte        <= x"01"; 
        datacounter      <= 0;
        syscounter       <= 0;
        
    ELSIF(rising_edge(counter_clk) AND sw = '1' AND datacounter < 972) THEN         -- On counter_clk trigger and if external says ok and when not outside the max data length of Data
        
        
        IF cyclecounter = 5 AND syscounter /= 0 THEN                                -- Start the data import earlier on 5th cycle due to parrallel working 
            IF syscounter = datalength THEN                                         -- If address and bytes as many as datalength has been send
                              
                IF datacounter /= (Data'length -1) THEN                             -- And datacounter hasnt reached the last array element/elements of Data
                    data_byte <= Data(datacounter+1)((datalength - 0)* 8 -1 DOWNTO (datalength - 0 - 1)*8);       -- Then import the data from the next array elemnt        
                ELSE
                    NULL;                                                           -- If datacounter reached the last array element, just stop importing, there will be nothing, just index out of range errors
                END IF;
                
                datacounter <= datacounter +1;                                     -- Anyway increment datacounter
                      
                                
           ELSE
                data_byte <= Data(datacounter)((datalength - syscounter)* 8 -1 DOWNTO (datalength - syscounter - 1)*8);    -- If nothing special: Import next two bytes from array element of Data on 5th cycle
            END IF;   
        END IF;
        
 -- This code is to control cycecounter, syscounter and datacounter. There are incremented/reset according to there discription 
        
        IF cyclecounter = 9 THEN                                    -- If there ahs been 9 cycles increase the oterh counters
            IF syscounter = datalength THEN                         -- If there is also address + datalength worth of bytes
                syscounter <= 0;                                    -- Reset syscounter to count again from address and bytes
                cyclecounter <= 0;                                  -- And reset cyclecounter
            ELSE
                syscounter <= syscounter + 1;                       -- Else: increase syscounter; We havent reached the last byte jet
                cyclecounter <= 1;                                  -- And reset cyclecounter to 1. This may look asymmetric but it has to be due to parallel processing
            END IF;            
        ELSE
            cyclecounter <= cyclecounter + 1;                       -- And if nothing special happens just increment cyclecounter
        END IF;
        
        
    END IF;
END PROCESS;


end Behavioral;                                                     -- Finish

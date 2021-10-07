------------------------------------------------------------------------------------
-- Designed by Tim Molzberger at Albert-Ludwigs University of Freiburg
-- Filename: Si5344_Data_Core_FSM
-- Dependencies: I2C_master_original.vhd or I2C_master.vhd from Scott Larson
-- Design Software:  VIVADO 2020.2
-- Contact: tim.molzberger@gmail.com
-- For more information, please review the connected bachelor thesis
--
-- HDL code is povided as it is. No gurantees.
-------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use std.textio.all;
use ieee.std_logic_textio.all;



entity Si5344_Data_Core_FSM is
    GENERIC(
        clkfreq : INTEGER := 100_000_000;                           --input clock frequency in Hz. Should come form the board
        bus_clk   : INTEGER := 100_000;                              -- Bus clk freq in Hz
        adress : IN std_logic_vector(6 DOWNTO 0) := "1101000"       -- Slave adress to set
    );
    Port ( 
        CLK100MHZ : IN      STD_LOGIC;                   --system clock
        sda       : INOUT   STD_LOGIC;                   --serial data output of i2c bus
        scl       : INOUT   STD_LOGIC;                   --serial clock output of i2c bus
        
        sw        : IN      STD_LOGIC;                  -- external control signal for start/stop/restart  
        ack_error : BUFFER  STD_LOGIC                   --port for acknowledgment error
    );
end Si5344_Data_Core_FSM;

architecture Behavioral of Si5344_Data_Core_FSM is

--**************************************** These Signals are for top-level use. If not top level, pls activate ports ************************

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

--********************************************************************************************

        SIGNAL data_byte  :   std_logic_vector(7 DOWNTO 0) := x"01";        -- data_byte will grab the data/hexnumbers from 'Data# and transmit them to the I2C Core

        SIGNAL IO_Flag      : std_logic := '0';                             -- For enabeling and disabeling the IO Component
        SIGNAL IO_reset     : std_logic := '1';                             -- To Reset/Hold the IO Component. Used for restarts
        SIGNAL IO_busy      : std_logic;                                    -- A not used busy flag from the IO Component. Could have used it, worked out without it
        SIGNAL IO_read      : std_logic_vector(7 DOWNTO 0);                 -- The byte if there was something to read from IO. But this functionality was not used
 


--********************************************************************************************

        COMPONENT i2c_master IS   
            generic(
                input_clk : INTEGER;
                bus_clk    : INTEGER);                      
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
            
            
--*******************************************************************************************************++

TYPE machine IS (ready, writeONE, writeTWO ,pause,long_pause,stop); -- FSM. 
-- Ready: get ready to work
-- writeONE: write state for address and 1st byte
-- wrtieTWO: state for write 2nd byte
-- pause: state for short waiting btween two sentences
-- long_pause: pause th 350ms
-- stop: end transmission

SIGNAL state : machine;

SIGNAL counter      : INTEGER := 0;                                 -- Counter to time the data import for 2nd byte
SIGNAL datacounter  : INTEGER RANGE 0 TO Data'length := 0;          -- Counts the amount of transmissions done
SIGNAL IO_busy_prev : std_logic;                                    -- Is sued with IO_busy to trigger on busy_flag from Digikey core
SIGNAL counter_flag : std_logic := '0';                             -- Sets the counter  'counter' for 2nd byte data import to idle if 0 
SIGNAL waitcounter  : INTEGER := 0;                                 -- Counter to wait 

begin

IO : i2c_master                                                         -- The component of the transmitting core
        generic map(
        input_clk => clkfreq,
        bus_clk => bus_clk
        )
        port map(
            clk => CLK100MHZ,
            reset_n => IO_reset,
            ena => IO_Flag,
            addr => adress,
            rw => rwb,
            data_wr => data_byte,
            busy => IO_busy,
            data_rd =>IO_read,
            ack_error =>ack_error,
            sda => sda,
            scl => scl);


process(CLK100MHZ)
begin

IF sw = '0' THEN                            -- Reset everything
    state <= ready;
    IO_Flag          <= '0';
    IO_reset         <= '0';
    data_byte        <= x"01"; 
    datacounter     <= 0;
    counter         <= 0;
    waitcounter    <= 0;
    
ELSIF rising_edge(CLK100MHZ) THEN           -- Main process on raising edge on clk
    CASE state IS                           -- FSM
        WHEN ready =>
            IO_Flag <= '1';
            IO_reset <= '1';
            IF IO_busy = '1' THEN
                state <= writeONE;
            END IF;
        
        WHEN writeONE =>          
            
            IF counter = clkfreq/bus_clk * 17 THEN              -- Dynamicly calculated time when to import the data for the 2nd write operation
                data_byte <= Data(datacounter)(7 DOWNTO 0);     -- If the time has come: import the 2nd byte from the 2-byte array element
                datacounter <= datacounter + 1;                 -- Increase data counter so the next array element will be imported later
                counter <= 0;                                   -- Reset counter
                counter_flag <= '1';                            -- Hold counter for the rest of the time
            ELSIF counter_flag = '0' THEN                       -- Else: Just count
                counter <= counter + 1;
            END IF;
            
            
            IF IO_busy = '0' AND IO_busy_prev = '1' THEN        -- On a falling edge of busy flag:
                state <= writeTWO;                              -- Change to writeTWO since busy flag will fall with the end of the 1st byte ACK signal
            END IF;
           
            IO_busy_prev <= IO_busy;                            -- And also update IO_busy_prev always to register the falling and rising edge
        
        WHEN writeTWO =>                                        -- When to write 2d byte
            
            IF IO_busy = '1' AND IO_busy_prev = '0' THEN        -- On a rising edge (this happens at the time the 2nd byte is fully transmitted)
                IO_Flag <= '0';                                 -- Stop the DigiKey Core on continuing after this transmission
                IO_reset <= '1';                                -- But dont activate the reset.
            ELSIF IO_busy = '0' AND IO_busy_prev = '1' THEN     -- On falling edge
                IF Data(datacounter) = x"0000" OR datacounter = 486*2+1 THEN    -- If the empty data with just hex "0000" or the end of the data array is reached: STOP the transmission
                    state <= stop;
                ELSE
                    IF datacounter = 7 THEN                     -- Else: Start the long pause after 7 transmissions
                        state <= long_pause;                
                    ELSE                                        -- Or just do a regular pause
                        state <= pause;
                    END IF; 
                    data_byte <= Data(datacounter)(15 DOWNTO 8);    -- And preload net data byte 
                END IF;
 
                
                counter_flag <= '0';                            -- Reset counter flag, because after pause/stop the state is writeONE again and needs the counter
            END IF;
            
            IO_busy_prev <= IO_busy;                            -- And ofc. update the prev. signal of busy_flag
        
        WHEN pause =>                                           -- When in a regular pause
            IF waitcounter = clkfreq/bus_clk * 15 THEN          -- Wait 1.5ms
                IO_Flag <= '1';                                 -- When done start the transmission core
                IO_reset <= '1';                                -- Hold up the reset so nothing gets reset
                waitcounter <= 0;                               -- Reset Counter
                state <= writeONE;                              -- Start writing again
            ELSE
                waitcounter <= waitcounter + 1;                 -- Else just count up
            END IF;
        
        WHEN long_pause =>                                      -- On a long pause
            IF waitcounter = clkfreq/bus_clk * 350e2 THEN       -- Wait the 350ms (dynamic calculations)
                IO_Flag <= '1';                                 -- Activate Core to write
                IO_reset <= '1';
                waitcounter <= 0;                               -- reset counter
                state <= writeONE;                              -- Get into writing Mode for the first byte
            ELSE
                waitcounter <= waitcounter + 1;                 -- othereise: continue counting
            END IF;
        WHEN stop =>                                            -- When stop (end of transmission)
            IO_Flag <= '0';                                     -- Deactivate writing Core
            
        WHEN OTHERS =>                                          -- Savety option NULL
            NULL;
    END CASE;

END IF;

end process;


end Behavioral;

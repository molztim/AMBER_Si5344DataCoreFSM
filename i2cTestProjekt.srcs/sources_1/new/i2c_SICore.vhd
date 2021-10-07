--------------------------------------------------------------------------------
--
--   FileName:         i2c_master.vhd
--   Dependencies:     none
--   Design Software:  Quartus II 64-bit Version 13.1 Build 162 SJ Full Version
--
--   HDL CODE IS PROVIDED "AS IS."  DIGI-KEY EXPRESSLY DISCLAIMS ANY
--   WARRANTY OF ANY KIND, WHETHER EXPRESS OR IMPLIED, INCLUDING BUT NOT
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
--   PARTICULAR PURPOSE, OR NON-INFRINGEMENT. IN NO EVENT SHALL DIGI-KEY
--   BE LIABLE FOR ANY INCIDENTAL, SPECIAL, INDIRECT OR CONSEQUENTIAL
--   DAMAGES, LOST PROFITS OR LOST DATA, HARM TO YOUR EQUIPMENT, COST OF
--   PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR SERVICES, ANY CLAIMS
--   BY THIRD PARTIES (INCLUDING BUT NOT LIMITED TO ANY DEFENSE THEREOF),
--   ANY CLAIMS FOR INDEMNITY OR CONTRIBUTION, OR OTHER SIMILAR COSTS.
--
--   Version History
--   Version 1.0 11/01/2012 Scott Larson
--     Initial Public Release
--   Version 2.0 06/20/2014 Scott Larson
--     Added ability to interface with different slaves in the same transaction
--     Corrected ack_error bug where ack_error went 'Z' instead of '1' on error
--     Corrected timing of when ack_error signal clears
--   Version 2.1 10/21/2014 Scott Larson
--     Replaced gated clock with clock enable
--     Adjusted timing of SCL during start and stop conditions
--   Version 2.2 02/05/2015 Scott Larson
--     Corrected small SDA glitch introduced in version 2.1
-- 
--------------------------------------------------------------------------------
--  
--  This code was altered by Tim Molzberger. As statet above, the source code is open source. 
--  Also thx for the good code. The befor mentioned rights will also apply to my edits.
--
--  For help or support, please wirte to tim.molzberger@gmail.com
--
--
--
--
--

LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;

ENTITY i2c_master IS  
    GENERIC(
    input_clk : INTEGER := 100_000_000;
    bus_clk     : INTEGER := 40_000_000;
    len         : positive := 1;            -- How many bytes do I get per message from the PreData Core?
    comm_len    : positive := 2             --How many bytes per Communication cycle?
    );                   
  PORT(
    CLK100MHZ : IN     STD_LOGIC;                    --system clock
    sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
    scl       : INOUT  STD_LOGIC;                   --serial clock output of i2c bus
    
    sw          : IN STD_LOGIC;
    led         : OUT std_logic := '0';
    led2        : OUT std_logic
    
    --io0         : IN std_logic; 
    --io1         : IN std_logic 
    );
END i2c_master;

ARCHITECTURE logic OF i2c_master IS

    COMPONENT PreData
    PORT(DATA    :   OUT STD_LOGIC_VECTOR( (1*8)-1 DOWNTO 0);
         READ_IN :   IN  STD_LOGIC;
         FLAG    :   OUT std_logic;
         WAITS   :   OUT STD_LOGIC
         );
    END COMPONENT;

  CONSTANT in_clk   : INTEGER := 100_000_000;     --input clock speed from user logic in Hz
  CONSTANT b_clk    : INTEGER :=  2;      --speed the i2c bus (scl) will run at in Hz
  
  CONSTANT divider  :  INTEGER := (in_clk/b_clk)/4; --number of clocks in 1/4 cycle of scl
  TYPE machine IS(ready, start, command, slv_ack1, wr, rd, slv_ack2, mstr_ack, stop, delay); --needed states
  SIGNAL state         : machine;                        --state machine
  SIGNAL data_clk      : STD_LOGIC;                      --data clock for sda
  SIGNAL data_clk_prev : STD_LOGIC;                      --data clock during previous system clock
  SIGNAL scl_clk       : STD_LOGIC;                      --constantly running internal scl
  SIGNAL scl_ena       : STD_LOGIC := '0';               --enables internal scl to output
  SIGNAL sda_int       : STD_LOGIC := '1';               --internal sda
  SIGNAL sda_ena_n     : STD_LOGIC;                      --enables internal sda to output
  SIGNAL addr_rw       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --latched in address and read/write
  SIGNAL data_tx       : STD_LOGIC_VECTOR(7 DOWNTO 0) := "01010101";   --latched in data to write to slave
  SIGNAL data_rx       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --data received from slave
  SIGNAL bit_cnt       : INTEGER RANGE 0 TO 7 := 7;      --tracks bit number in transaction
  SIGNAL stretch       : STD_LOGIC := '0';               --identifies if slave is stretching scl

-- All the ports that have no pin on the bords -> SIGNALs

    SIGNAL reset_n   :      STD_LOGIC := '1';                    --active low reset
    SIGNAL ena       :      STD_LOGIC := '1';                    --latch in command         -USUALLY 0, CARE WITH CAUTION
    SIGNAL addr      :      STD_LOGIC_VECTOR(6 DOWNTO 0) := "1101000"; --address of target slave --original: 1101000 -- Hex Sachen: x"68"
    SIGNAL rw        :      STD_LOGIC := '0';                    --'0' is write, '1' is read
    
    SIGNAL busy      :      STD_LOGIC;                    --indicates transaction in progress
    SIGNAL data_rd   :      STD_LOGIC_VECTOR(7 DOWNTO 0); --data read from slave
    SIGNAL ack_error :      STD_LOGIC;                    --flag if improper acknowledge from slave
    
    SIGNAL sw_controler : STD_LOGIC := '0';
    
    SIGNAL data_wr   :      STD_LOGIC_VECTOR(23 DOWNTO 0):="000000010000100100011000"; --data to write to slave, HARD CODED
    SIGNAL data_len  :      INTEGER RANGE 0 to (len-1) := (len-1);      -- Lenght of signal in bytes in Computer Terms: 1. byte has number 0

    
    
-- For talking to the PreData Core    
    SIGNAL data_ext  :      STD_LOGIC_VECTOR(7 DOWNTO 0) :="01111110";  -- dynamic data Input
    SIGNAL import_trig:     STD_LOGIC := '0';               -- trigger to set the core in action; is NOT needed to load one 
    SIGNAL END_FLAG  :      STD_LOGIC := '0';               -- Flag for finished operation, given from outside
    SIGNAL END_FLAG_INT :   STD_LOGIC := '0';               -- Flag for the cycle after END_FLAG has benn triggered
    
    SIGNAL data_ext2    :   STD_LOGIC_VECTOR(7 DOWNTO 0);
    
    
-- The 300ms waiting    
    SIGNAL WAIT_FLAG    :   STD_LOGIC;               -- FLag for the 300ms waiting periode
    SIGNAL WAIT_SET     : STD_LOGIC := '0';
    SIGNAL WAIT_COUNTER :   INTEGER RANGE 0 TO 300*10e5 := 0;
    SIGNAL WAIT_HOLD    :   STD_LOGIC := '0';
    SIGNAL WAIT_TIME    :   INTEGER := 300*100;        -- Time in ms * scl/1000!
    
-- The loop for all comm. cycles
    SIGNAL cycle        : INTEGER RANGE 0 TO comm_len := comm_len-1;
    SIGNAL cycle_down   : INTEGER := 2;                                 --cycle_down ist die anzahl an ausgeetzten Zyklen, MINIMUM 2 wegen der END FLAG
    SIGNAL cycle_timeout: INTEGER RANGE 0 TO 6 := cycle_down;
    
--led stuff
    SIGNAL led_signal   : STD_LOGIC := '0';
    SIGNAL led_signal2   : STD_LOGIC := '0';
    
--SDA and SCL input buffer
    SIGNAL sda_buffer   :   STD_LOGIC;
    SIGNAL scl_buffer   :   STD_LOGIC;

BEGIN

    import_logic    :   PreData PORT MAP (DATA => data_ext2, READ_IN => import_trig, FLAG => END_FLAG, WAITS => WAIT_FLAG);
    

    
  --generate the timing for the bus clock (scl_clk) and the data clock (data_clk)
  PROCESS(CLK100MHZ, reset_n)
    VARIABLE count  :  INTEGER RANGE 0 TO divider*4 := 0;  --timing for clock generation
  BEGIN
    IF(reset_n = '0') THEN                --reset asserted
      stretch <= '0';
      count := 0;
    ELSIF(CLK100MHZ'EVENT AND CLK100MHZ = '1') THEN   
      data_clk_prev <= data_clk;          --store previous value of data clock
      IF(count = divider*4-1) THEN        --end of timing cycle
        count := 0;                       --reset timer
      ELSIF(stretch = '0') THEN           --clock stretching from slave not detected
        count := count + 1;               --continue clock generation timing
      END IF;
      CASE count IS
        WHEN 0 TO divider - 1 =>            --first 1/4 cycle of clocking
          scl_clk <= '0';
          data_clk <= '0';  
          
          --led <= '1';
                
        WHEN divider TO divider*2 - 1 =>    --second 1/4 cycle of clocking
          scl_clk <= '0';
          data_clk <= '1';
         --led_signal2 <= '1';
        WHEN divider*2 TO divider*3 - 1 =>  --third 1/4 cycle of clocking
          scl_clk <= '1';                 --release scl;
          data_clk <= '1';
          --led <= '1';
        WHEN OTHERS =>                    --last 1/4 cycle of clocking
          scl_clk <= '1';
          data_clk <= '0';
          --led_signal2 <= '0';
      END CASE;
    END IF;
  END PROCESS;

  --state machine and writing to sda during scl low (data_clk rising edge)
  PROCESS(CLK100MHZ, reset_n)
  BEGIN

    IF(reset_n = '0') THEN                 --reset asserted
      state <= ready;                      --return to initial state
      busy <= '1';                         --indicate not available
      scl_ena <= '0';                      --sets scl high impedance
      sda_int <= '1';                      --sets sda high impedance
      ack_error <= '0';                    --clear acknowledge error flag
      bit_cnt <= 7;                        --restarts data bit counter
      data_rd <= "00000000";               --clear data read port
      --reset_n <= '1';                     --Continue with the operation
            
    ELSIF(CLK100MHZ'EVENT AND CLK100MHZ = '1') THEN
            

      IF(data_clk = '1' AND data_clk_prev = '0') THEN  --data clock rising edge
        CASE state IS

          
          WHEN ready =>                      --idle state  
                    
            IF(ena = '1') THEN               --transaction requested
              busy <= '1';                   --flag busy
              addr_rw <= addr & rw;          --collect requested slave address and command
              state <= start;                --go to start bit
              
              import_trig <= '1';
               led_signal2 <= '1';
            ELSE                             --remain idle
              busy <= '0';                   --unflag busy
              state <= ready;                --remain idle
            END IF;
          WHEN start =>                      --start bit of transaction
                      
           
            --import_trig <= '0';                                                        --Start loading in next
            busy <= '1';                     --resume busy if continuous mode
            sda_int <= addr_rw(bit_cnt);     --set first address bit to bus
            state <= command;                --go to command
            
            data_tx <= data_ext( 7 DOWNTO 0);
            
          WHEN command =>                    --address and command byte of transaction
            IF(bit_cnt = 0) THEN             --command transmit finished
              sda_int <= '1';                --release sda for slave acknowledge
              bit_cnt <= 7;                  --reset bit counter for "byte" states
              state <= slv_ack1;             --go to slave acknowledge (command)
              
              import_trig <= '0';
              
            ELSE                             --next clock cycle of command state
              bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
              sda_int <= addr_rw(bit_cnt-1); --write address/command bit to bus
              state <= command;              --continue with command
            END IF;
          WHEN slv_ack1 =>                   --slave acknowledge bit (command)
            IF(addr_rw(0) = '0') THEN        --write command
              sda_int <= data_tx(bit_cnt);   --write first bit of data
              state <= wr;                   --go to write byte
              
              
              import_trig <= '1';
              data_tx <= data_ext( 7 DOWNTO 0);
              
              
            ELSE                             --read command
              sda_int <= '1';                --release sda from incoming data
              state <= rd;                   --go to read byte
            END IF;
            
          WHEN wr =>                         --write byte of transaction
            busy <= '1';                     --resume busy if continuous mode
                        
            IF(bit_cnt = 0) THEN             --write byte transmit finished
              sda_int <= '1';                --release sda for slave acknowledge
              bit_cnt <= 7;                  --reset bit counter for "byte" states
              state <= slv_ack2;             --go to slave acknowledge (write)
              
            --  import_trig <= '1';              -- UPDATE import on last mile
            --  data_tx <= data_ext( 7 DOWNTO 0); 
              
            ELSE                             --next clock cycle of write state
              bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
              sda_int <= data_tx(bit_cnt-1); --write next bit to bus
              state <= wr;                   --continue writing
              import_trig <= '0';              --reset import system
            END IF;
            
          WHEN rd =>                         --read byte of transaction
            busy <= '1';                     --resume busy if continuous mode
            IF(bit_cnt = 0) THEN             --read byte receive finished
              IF(ena = '1' AND addr_rw = addr & rw) THEN  --continuing with another read at same address
                sda_int <= '0';              --acknowledge the byte has been received
              ELSE                           --stopping or continuing with a write
                sda_int <= '1';              --send a no-acknowledge (before stop or repeated start)
              END IF;
              bit_cnt <= 7;                  --reset bit counter for "byte" states
              data_rd <= data_rx;            --output received data
              state <= mstr_ack;             --go to master acknowledge
            ELSE                             --next clock cycle of read state
              bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
              state <= rd;                   --continue reading
            END IF;
          WHEN slv_ack2 =>                   --slave acknowledge bit (write)
            IF(ena = '1' AND cycle > 0) THEN               --continue transaction
              
              state <= wr;       
              import_trig <= '0';
              sda_int <= data_ext(bit_cnt);
              cycle <= cycle - 1;             
                            
              --import_trig <= '1';              -- UPDATE import on last mile
              data_tx <= data_ext( 7 DOWNTO 0);
            
            ELSE 
              cycle <= comm_len - 1;
               state <= stop;
          END IF;
          
          WHEN mstr_ack =>                   --master acknowledge bit after a read
            IF(ena = '1') THEN               --continue transaction
              busy <= '0';                   --continue is accepted and data received is available on bus
              addr_rw <= addr & rw;          --collect requested slave address and command
              --data_tx <= data_ext(7 + data_len*8 DOWNTO 0 + data_len*8);            --collect requested data to write
              IF(addr_rw = addr & rw) THEN   --continue transaction with another read
                sda_int <= '1';              --release sda from incoming data
                state <= rd;                 --go to read byte
              ELSE                           --continue transaction with a write or new slave
                state <= start;              --repeated start
              END IF;    
            ELSE                             --complete transaction
              state <= stop;                 --go to stop bit
            END IF;
            
            
          WHEN stop =>                       --stop bit of transaction
            busy <= '0';                     --unflag busy
            state <= ready;                  --go to idle state
            ena <= '0';
            
          WHEN OTHERS =>
            NULL;
        END CASE;    
      ELSIF(data_clk = '0' AND data_clk_prev = '1') THEN  --data clock falling edge
        CASE state IS
          WHEN start =>                  
            IF(scl_ena = '0') THEN                  --starting new transaction
              scl_ena <= '1';                       --enable scl output
              ack_error <= '0';                     --reset acknowledge error output
            END IF;
          WHEN slv_ack1 =>                          --receiving slave acknowledge (command)
            IF(sda_buffer /= '0' OR ack_error = '1') THEN  --no-acknowledge or previous no-acknowledge
              ack_error <= '1';                     --set error output if no-acknowledge
            END IF;
          WHEN rd =>                                --receiving slave data
            data_rx(bit_cnt) <= sda;                --receive current slave data bit
          WHEN slv_ack2 =>                          --receiving slave acknowledge (write)
            IF(sda_buffer /= '0' OR ack_error = '1') THEN  --no-acknowledge or previous no-acknowledge
              ack_error <= '1';                     --set error output if no-acknowledge
            END IF;
            
          WHEN stop =>
            scl_ena <= '0';                         --disable scl
            
          WHEN OTHERS =>
            NULL;
        END CASE;
      END IF;
      
 -- Cycle regulator. Can be attached to regular code
 
         
      IF(data_clk = '1' AND data_clk_prev = '0') THEN
        IF state = ready THEN
            --sda_int <= '1';
            IF cycle_timeout = 0 THEN
                reset_n <= '1';
                cycle_timeout <= cycle_down;
                ena <= '1';        
                
            ELSE
                cycle_timeout <= cycle_timeout - 1;
                import_trig <= '1';
            END IF;
            
            IF END_FLAG = '1' THEN
                reset_n <= '0';
               led_signal <= '1';
            END IF; 
            
            IF WAIT_FLAG = '1' AND WAIT_HOLD = '0' THEN
                state <= delay;
                WAIT_HOLD <= '1';
                
            END IF;
            
        ELSIF state = delay THEN
            IF WAIT_COUNTER < WAIT_TIME THEN
                WAIT_COUNTER <= WAIT_COUNTER + 1;
            ELSE
                state <= ready;
                
            END IF;
        END IF;
      END IF;
    END IF;
    
  END PROCESS;  

  --set sda output
  WITH state SELECT
    sda_ena_n <= data_clk_prev WHEN start,     --generate start condition
                 NOT data_clk_prev WHEN stop,  --generate stop condition
                 sda_int WHEN OTHERS;          --set to internal sda signal    
      
  --set scl and sda outputs




  scl <= '0' WHEN ((scl_ena = '1' AND scl_clk = '0'))  ELSE 'Z'; --OR WAIT_HOLD = '1'
  sda <= '0' WHEN sda_ena_n = '0'  ELSE 'Z';
  
  sda_buffer <= sda;
  scl_buffer <= scl;
  
  -- This code allows control via the switch on my board. Delet if note useful anymore
    --ena <= sw;
    
    led <= led_signal;
    led2 <= led_signal2;
    
END logic;

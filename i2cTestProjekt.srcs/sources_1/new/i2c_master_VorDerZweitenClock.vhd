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
--  Also thx for the good code. All
--
--
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
    bus_clk     : INTEGER := 40_000_000);
  PORT(
    CLK100MHZ : IN     STD_LOGIC;                    --system clock
    sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
    scl       : INOUT  STD_LOGIC;                   --serial clock output of i2c bus
    
    sw          : IN STD_LOGIC;
    led         : OUT std_logic;
    led2        : OUT std_logic;
    led3        : OUT STD_LOGIC;
    led4        : OUT STD_LOGIC := '0';
    
    io0         : INOUT std_logic; 
    io1         : INOUT std_logic 
    );
END i2c_master;

ARCHITECTURE logic OF i2c_master IS

    SIGNAL led_sig  : STD_LOGIC := '0';
    SIGNAL led_sig2  : STD_LOGIC := '0';
    SIGNAL feel     : STD_LOGIC :='0';

  CONSTANT in_clk   : INTEGER := 100_000_000;     --input clock speed from user logic in Hz
  CONSTANT b_clk    : INTEGER :=  1;      --speed the i2c bus (scl) will run at in Hz
  
  CONSTANT divider  :  INTEGER := (in_clk/b_clk)/4; --number of clocks in 1/4 cycle of scl
  TYPE machine IS(ready, start, command, slv_ack1, wr, rd, slv_ack2, mstr_ack, stop); --needed states
  SIGNAL state         : machine;                        --state machine
  SIGNAL data_clk      : STD_LOGIC;                      --data clock for sda
  SIGNAL data_clk_prev : STD_LOGIC;                      --data clock during previous system clock
  SIGNAL scl_clk       : STD_LOGIC;                      --constantly running internal scl
  SIGNAL scl_ena       : STD_LOGIC := '0';               --enables internal scl to output
  SIGNAL sda_int       : STD_LOGIC := '1';               --internal sda
  SIGNAL sda_ena_n     : STD_LOGIC;                      --enables internal sda to output
  SIGNAL addr_rw       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --latched in address and read/write
  SIGNAL data_tx       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --latched in data to write to slave
  SIGNAL data_rx       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --data received from slave
  SIGNAL bit_cnt       : INTEGER RANGE 0 TO 7 := 7;      --tracks bit number in transaction
  SIGNAL stretch       : STD_LOGIC := '0';               --identifies if slave is stretching scl

-- All the ports that have no pin on the bords -> SIGNALs

    SIGNAL reset_n   :      STD_LOGIC := '1';                    --active low reset
    SIGNAL ena       :      STD_LOGIC := '0';                    --latch in command
    SIGNAL addr      :      STD_LOGIC_VECTOR(6 DOWNTO 0) := "1100110"; --address of target slave
    SIGNAL rw        :      STD_LOGIC := '0';                    --'0' is write, '1' is read
    SIGNAL data_wr   :      STD_LOGIC_VECTOR(7 DOWNTO 0) := "01110101"; --data to write to slave
    SIGNAL busy      :      STD_LOGIC;                    --indicates transaction in progress
    SIGNAL data_rd   :      STD_LOGIC_VECTOR(7 DOWNTO 0); --data read from slave
    SIGNAL ack_error :      STD_LOGIC;                    --flag if improper acknowledge from slave
    
-- SIGNALS and CONSTANTS for Slave. To be copied into the slave afterwards
-- ALL FOR SLAVE ONLY

    SIGNAL sl_reset         : STD_LOGIC:= '1';  
    TYPE sl_machine IS(init, start_stop, adress, ack1, ack2, mstr_ack2, stop, rcv, send,nextcom);
    SIGNAL sl_state         : sl_machine;                       
    SIGNAL sl_data_clk      : STD_LOGIC;                     
    SIGNAL sl_data_clk_prev : STD_LOGIC;    
    SIGNAL sl_data_re       : STD_LOGIC_VECTOR(7 DOWNTO 0);            
    SIGNAL sl_busy          : STD_LOGIC;    
    SIGNAL sl_ack_error     : STD_LOGIC; 
    SIGNAL sl_bit_cnt       : INTEGER RANGE 0 TO 7 := 7;
    SIGNAL sl_adress        : STD_LOGIC_VECTOR(6 DOWNTO 0) := "1100110"; 
    SIGNAL sl_adress_rcv    : STD_LOGIC_VECTOR(7 DOWNTO 0);
    
    SIGNAL sl_scl           : STD_LOGIC;
    SIGNAL sl_scl_prev      : STD_LOGIC;
    SIGNAL sl_scl_out       : STD_LOGIC := '1';
    SIGNAL sl_sda           : STD_LOGIC;
    SIGNAL sl_sda_prev      : STD_LOGIC;
    SIGNAL sl_sda_out       : STD_LOGIC := '1';
    SIGNAL sl_ack_bo        : STD_LOGIC := '0';
    
    SIGNAL sl_LED_test      : STD_LOGIC := '0';
    
   
BEGIN
    
  --generate the timing for the bus clock (scl_clk) and the data clock (data_clk)
  PROCESS(CLK100MHZ, reset_n)
    VARIABLE count  :  INTEGER RANGE 0 TO divider*4;  --timing for clock generation
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
        WHEN divider TO divider*2 - 1 =>    --second 1/4 cycle of clocking
          scl_clk <= '0';
          data_clk <= '1';
        WHEN divider*2 TO divider*3 - 1 =>  --third 1/4 cycle of clocking
          scl_clk <= '1';                 --release scl
          IF(scl = '0') THEN              --detect if slave is stretching clock
            stretch <= '1';
          ELSE
            stretch <= '0';
          END IF;
          data_clk <= '1';
        WHEN OTHERS =>                    --last 1/4 cycle of clocking
          scl_clk <= '1';
          data_clk <= '0';
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
    ELSIF(CLK100MHZ'EVENT AND CLK100MHZ = '1') THEN    
      IF(data_clk = '1' AND data_clk_prev = '0') THEN  --data clock rising edge
        CASE state IS
          WHEN ready =>                      --idle state  
                    
            IF(ena = '1') THEN               --transaction requested
              busy <= '1';                   --flag busy
              addr_rw <= addr & rw;          --collect requested slave address and command
              data_tx <= data_wr;            --collect requested data to write
              state <= start;                --go to start bit
            ELSE                             --remain idle
              busy <= '0';                   --unflag busy
              state <= ready;                --remain idle
            END IF;
          WHEN start =>                      --start bit of transaction
            busy <= '1';                     --resume busy if continuous mode
            sda_int <= addr_rw(bit_cnt);     --set first address bit to bus
            state <= command;                --go to command
          WHEN command =>                    --address and command byte of transaction
            IF(bit_cnt = 0) THEN             --command transmit finished
              sda_int <= '1';                --release sda for slave acknowledge
              bit_cnt <= 7;                  --reset bit counter for "byte" states
              state <= slv_ack1;             --go to slave acknowledge (command)
            ELSE                             --next clock cycle of command state
              bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
              sda_int <= addr_rw(bit_cnt-1); --write address/command bit to bus
              state <= command;              --continue with command
            END IF;
          WHEN slv_ack1 =>                   --slave acknowledge bit (command)
            IF(addr_rw(0) = '0') THEN        --write command
              sda_int <= data_tx(bit_cnt);   --write first bit of data
              state <= wr;                   --go to write byte
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
            ELSE                             --next clock cycle of write state
              bit_cnt <= bit_cnt - 1;        --keep track of transaction bits
              sda_int <= data_tx(bit_cnt-1); --write next bit to bus
              state <= wr;                   --continue writing
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
            IF(ena = '1') THEN               --continue transaction
              busy <= '0';                   --continue is accepted
              addr_rw <= addr & rw;          --collect requested slave address and command
              data_tx <= data_wr;            --collect requested data to write
              IF(addr_rw = addr & rw) THEN   --continue transaction with another write
                sda_int <= data_wr(bit_cnt); --write first bit of data
                state <= wr;                 --go to write byte
              ELSE                           --continue transaction with a read or new slave
                state <= start;              --go to repeated start
              END IF;
            ELSE                             --complete transaction
              state <= stop;                 --go to stop bit
            END IF;
          WHEN mstr_ack =>                   --master acknowledge bit after a read
            IF(ena = '1') THEN               --continue transaction
              busy <= '0';                   --continue is accepted and data received is available on bus
              addr_rw <= addr & rw;          --collect requested slave address and command
              data_tx <= data_wr;            --collect requested data to write
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
        END CASE;    
      ELSIF(data_clk = '0' AND data_clk_prev = '1') THEN  --data clock falling edge
        CASE state IS
          WHEN start =>                  
            IF(scl_ena = '0') THEN                  --starting new transaction
              scl_ena <= '1';                       --enable scl output
              ack_error <= '0';                     --reset acknowledge error output
            END IF;
          WHEN slv_ack1 =>                          --receiving slave acknowledge (command)
            IF(sda /= '0' OR ack_error = '1') THEN  --no-acknowledge or previous no-acknowledge
              ack_error <= '1';                     --set error output if no-acknowledge
            END IF;
          WHEN rd =>                                --receiving slave data
            data_rx(bit_cnt) <= sda;                --receive current slave data bit
          WHEN slv_ack2 =>                          --receiving slave acknowledge (write)
            IF(sda /= '0' OR ack_error = '1') THEN  --no-acknowledge or previous no-acknowledge
              ack_error <= '1';                     --set error output if no-acknowledge
            END IF;
          WHEN stop =>
            scl_ena <= '0';                         --disable scl
          WHEN OTHERS =>
            NULL;
        END CASE;
      END IF;
    END IF;
  END PROCESS;  

  --set sda output
  WITH state SELECT
    sda_ena_n <= data_clk_prev WHEN start,     --generate start condition
                 NOT data_clk_prev WHEN stop,  --generate stop condition
                 sda_int WHEN OTHERS;          --set to internal sda signal    
      
  --set scl and sda outputs
  scl <= '0' WHEN (scl_ena = '1' AND scl_clk = '0') ELSE 'Z';
  sda <= '0' WHEN sda_ena_n = '0' ELSE 'Z';
  
ena <= sw;
--reset_n <= not sw;
  
process(CLK100MHZ)
VARIABLE count  :  INTEGER RANGE 0 TO 100_000_000;

begin

    if rising_edge(CLK100MHZ) then
        if count = (100_000_000/2 - 1) then
            led_sig <= NOT led_sig;
            count := 0;
        else
            count := count + 1;      
        end if;
    end if;
end process;

led <= led_sig;

process(feel)
begin
if feel = '1' then
    led_sig2 <= '1';
end if;
end process;








--
--  Original Slave Architektur
--  Gemacht für den master von Digikey
--  Alles selbst
--
-- State Maschine: init, runup, adress, ack1, wr, rd, ack2, mstr_ack, stop
-- 





-- Diese Prevoiuse Geschichte, die ich auf der SCL brauche
PROCESS(CLK100MHZ)
BEGIN
    IF(CLK100MHZ'EVENT AND CLK100MHZ = '1')THEN     --Trigger jedes mal, wenn die Interen 100MHZ Clk steigt
        sl_scl_prev <= sl_scl;
        sl_sda_prev <= sl_sda;
        
        IF io0 = '0' THEN       -- Convertiere: SDA Input zu logischen Werten
            sl_sda <= '0';
        ELSE
            sl_sda <= '1';
        END IF;
        
        IF io1 = '0' THEN       -- Convertiere: SCL Input zu logischen Werten
            sl_scl <= '0';
        ELSE
            sl_scl <= '1';
            
        END IF;

END IF;
END PROCESS;

led2 <= sl_scl;
led3 <= sl_sda;
led4 <= sl_LED_test;

-- Outputs: logisch zu Z/0
io1 <= '0' WHEN sl_scl_out = '0' ELSE 'Z';
io0 <= '0' WHEN sl_sda_out = '0' ELSE 'Z';


-- Das Hauptprogramm
PROCESS(CLK100MHZ, sl_reset)
BEGIN

    IF(reset_n = '0') THEN              -- Wenn ich reset_n = 0 habe dann...   
      sl_state <= init;                 -- ... Halte das Ding im STBY Modus         
      sl_busy <= '1';                                           
      sl_ack_error <= '0';                    
      sl_bit_cnt <= 7;                        
      sl_data_re <= "00000000";            
         
         
         
    ELSIF(CLK100MHZ'EVENT AND CLK100MHZ = '1') THEN    -- Also: Wenn die interne 100MHZ Clk triggert, dann für den kram aus
      IF sl_state = init THEN                          -- Gehe zuerst aus dem STBY Modus raus
        sl_state <= start_stop;
      END IF;  
      
      IF(sl_state = start_stop) THEN
         IF(sl_scl = '1' AND sl_sda = '0' AND sl_sda_prev = '1' ) THEN  -- Wenn das kommt: START/RESTART
            sl_adress_rcv <= "00000000";
            sl_state <= adress;

         ELSIF(sl_scl = '1' AND sl_sda = '1' AND sl_sda_prev = '0' ) THEN -- Und hier: STOP. Ende der Übertragung
            sl_sda_out <= '1';                  -- Setzte beide Ausgänge auf 
            sl_scl_out <= '1';
            sl_state <= start_stop;

         END IF; 
         
      ELSIF(sl_state = ack1 AND sl_bit_cnt = 0) THEN        -- Wenn ich in ack1 bin und am Ende des Strings:
        IF sl_adress_rcv(7 DOWNTO 1) = sl_adress THEN       -- Bleibe im ack1 Modus und setzte bit_cnt zurück
           sl_bit_cnt <= 7; 
        ELSE
            sl_bit_cnt <= 7;                                -- Sollte allerdings die Adresse nciht übereinstimmen, dann gehe in start_stop zurück
            sl_state <= start_stop; 
        END IF;    
        
    ELSIF(sl_state = ack2 AND sl_bit_cnt = 0) THEN      -- Wenn er am Ende von rcv ist und in sck2 springt: resette bit_cnt und bleibe hier
       sl_bit_cnt <= 7;
       IF sl_data_re = "01111110" THEN
            sl_LED_test <= '0';     
       END IF; 

                 
        
      ELSIF(sl_scl = '1' AND sl_scl_prev = '0') THEN       -- Auf einer steigenden Flanke der SCL
      
        CASE sl_state IS 
            WHEN adress =>
            
                IF(sl_bit_cnt = 0) THEN
                    sl_adress_rcv(sl_bit_cnt) <= sl_sda;                                                       
                    sl_state <= ack1;
              
                ELSE                                     
                    sl_adress_rcv(sl_bit_cnt) <= sl_sda;
                    sl_bit_cnt <= sl_bit_cnt - 1;
                    sl_state <= adress;           
                END IF;
                
                
            WHEN ack1 =>NULL;
            
            WHEN rcv =>
                sl_LED_test <= '1';
                
                IF(sl_bit_cnt = 0) THEN
                    sl_data_re(sl_bit_cnt) <= sl_sda;                                                       
                    sl_state <= ack2;
              
                ELSE                                     
                    sl_data_re(sl_bit_cnt) <= sl_sda;
                    sl_bit_cnt <= sl_bit_cnt - 1;
                    --sl_state <= rcv;           
                END IF;
                
            WHEN ack2 => NULL;
            WHEN send => NULL;
            WHEN OTHERS => NULL;
        END CASE;
        
      ELSIF(sl_scl = '0' AND sl_scl_prev = '1') THEN    -- Auf der fallenden Flanke des SCL
        CASE sl_state IS 
            WHEN ack1 =>
                IF sl_ack_bo = '0' THEN
                    sl_sda_out <= '0';
                    sl_ack_bo <= '1';
                    
                    
                ELSIF sl_ack_bo = '1' THEN
                    sl_sda_out <= '1';
                    sl_ack_bo <= '0';
                    
                    IF sl_adress_rcv(0) = '0' THEN
                        sl_state <= rcv;
                    ELSE 
                        sl_state <= send;
                    END IF;
                END IF;
            WHEN ack2 =>
                IF sl_ack_bo = '0' THEN
                    sl_sda_out <= '0';
                    sl_ack_bo <= '1';
                    
                ELSIF sl_ack_bo = '1' THEN
                    sl_sda_out <= '1';
                    sl_ack_bo <= '0';
                    sl_state <= rcv;
                END IF;
                
            WHEN OTHERS => NULL;
        END CASE;
      END IF;
END IF;
END PROCESS;

END logic;

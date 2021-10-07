--//////////////////////////////////////////////////////////////////////////////
--Core for Reading in Data
--Importatn: Activation Singal READ_IN has to be '0' before the first Operation in order to
--Pre-load data
--
--////////////////////////////////////////////////////////////////////////////
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity PreData is
  generic   (
                data_lenght : positive := 1;
                pre_max     : positive := 4;
                regi_max    : positive := 454;
                post_max    : positive := 5
            );
      Port ( 
            DATA    :   OUT STD_LOGIC_VECTOR( (data_lenght*8)-1 DOWNTO 0);
            READ_IN :   IN  STD_LOGIC;
            FLAG    :   OUT STD_LOGIC := '0';  
            WAITS    :   OUT STD_LOGIC := '0'         
            );
end PreData;

architecture Behavioral of PreData is

--    COMPONENT funktest
--    PORT(   INT_OUT :   OUT   POSITIVE;
--            TRIGGER    : IN STD_LOGIC);
--    END COMPONENT;

--Data signals
SIGNAL  preamble        :   STD_LOGIC_VECTOR(pre_max*3*8-1 DOWNTO 0) := x"0943010B24C00B2500054001";  --3 Befehle
SIGNAL  regi            :   STD_LOGIC_VECTOR(regi_max*3*8-1 DOWNTO 0):= x"000600000700000800000B680016020017DC0018FF0019FF001AFF002B02002C00002D00002E00002F00003000003100003200003300003400003500003600003700003800003900003A00003B00003C00003D00003F0000400400410000420000430000440000450C004600004700004800004900004A00004B00004C00004D00004E00004F0000500F005100005200005300005400005500005600005700005800005900005A00005B00005C00005D00005E00005F00006000006100006200006300006400006500006600006700006800006900009200009300009500009600009800009A00009B00009D00009E0000A00000A20000A90000AA0000AB0000AC0000E50100EA0000EB0000EC0000ED0001020101120601130901143B01152801170101180901193B011A2801260101270901283B012928012B01012C09012D3B012E28013F000140000141400142FF020600020800020900020A00020B00020C00020D00020E00020F00021000021100021200021300021400021500021600021700021800021900021A00021B00021C00021D00021E00021F00022000022100022200022300022400022500022600022700022800022900022A00022B00022C00022D00022E00022F0002310B02320B02330B02340B023500023600023700023880023989023A00023B00023C00023D00023E80025000025100025200025300025400025500025C00025D00025E00025F00026000026100026B35026C33026D34026E34026F45027056027142027234028A00028B00028C00028D00028E00028F00029000029100029480029600029700029900029D00029E00029F0002A90002AA0002AB0002B7FF030200030300030400030500030684030700030800030900030A00030B80030C00030D00030E00030F00031000031100031200031300031400031500031600031700031800031900031A00031B00031C00031D00031E00031F00032000032100032200032300032400032500032600032700032800032900032A00032B00032C00032D0003380003391F033B00033C00033D00033E00033F00034000034100034200034300034400034500034600034700034800034900034A00034B00034C00034D00034E00034F00035000035100035200035900035A00035B00035C00035D00035E00035F00036000048700050800050900050A00050B00050C00050D00050E00050F00051000051100051200051300051500051600051700051800051900051A00051B00051C00051D00051E00051F0005212B052A01052B01052C0F052D03052E00052F00053100053200053304053400053501053604053700053800053900053D0A053E0605890C058A00059B18059D00059E00059F0005A00005A10005A20005A600080235080305080401080500080600080700080800080900080A00080B00080C00080D00080E00080F00081000081100081200081300081400081500081600081700081800081900081A00081B00081C00081D00081E00081F00082000082100082200082300082400082500082600082700082800082900082A00082B00082C00082D00082E00082F00083000083100083200083300083400083500083600083700083800083900083A00083B00083C00083D00083E00083F00084000084100084200084300084400084500084600084700084800084900084A00084B00084C00084D00084E00084F00085000085100085200085300085400085500085600085700085800085900085A00085B00085C00085D00085E00085F00086000086100090E02094300094900094A00094E49094F02095E000A02000A03010A04010A05010A14000A1A000A20000A26000B440F0B46000B470F0B480F0B4A0E0B570E0B5801";  
--454 Befehle
SIGNAL  postamble       :   STD_LOGIC_VECTOR(post_max*3*8-1 DOWNTO 0) := x"051401001C010540000B24C30B2502";  --5 Befehle


--machine and generell processing signals
TYPE machine IS (ready, pre, reg, post, fin);
SIGNAL state            :   machine;        --machine is to controll state/data return
TYPE leaf IS (wPAGE, wPAGE2, wDATA, wDATA2);
SIGNAL branch               :   leaf;       --machine for write page or data

SIGNAL counter_pre          :   INTEGER RANGE 0 TO pre_max := pre_max;
SIGNAL counter_regi         :   INTEGER RANGE 0 TO regi_max := regi_max;
SIGNAL counter_post         :   INTEGER RANGE 0 TO post_max := post_max;
SIGNAL writer               :   BOOLEAN := FALSE;
SIGNAL reset                :   INTEGER RANGE 0 TO 2 := 2;


-- Signals for outside
SIGNAL force_pause       :  STD_LOGIC := '0'; --Signal Force 300ms pause in Top level. Requires OUT
SIGNAL finish_flag       :  STD_LOGIC := '0'; --Set Flag when finished
SIGNAL TRIGGER_CHANGE   :   STD_LOGIC :='0';        --Triggert as Laden des nächstens Bytes bzw. das vorranschreiten im state
SIGNAL DATA_INT         :   STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL WAIT_FLAG_IN     :   STD_LOGIC := '0';

SIGNAL TEST             :   STD_LOGIC;

--SIGNAL TEST_COUNTER     :   POSITIVE;


-- Functions!
FUNCTION converter (recv : STD_LOGIC_VECTOR(3*8-1 DOWNTO 0)) return STD_LOGIC_VECTOR is
BEGIN
    return  x"01" & recv(3*8-1 DOWNTO 2*8) & recv(2*8-1 DOWNTO 8) & recv(7 DOWNTO 0); --Adds the register shift function
    -- Order : x01 - new page - register - data 
END FUNCTION;

begin

--funk : funktest PORT MAP (INT_OUT => TEST_COUNTER, TRIGGER => TRIGGER_CHANGE);


PROCESS(READ_IN,reset,counter_pre,preamble)
BEGIN

IF reset /= 0 THEN
    reset <= reset - 1;
    
    IF reset = 2 THEN
        DATA_INT <= converter(preamble((counter_pre)*3*8-1 DOWNTO (counter_pre-1)*3*8 ));
        counter_pre <= counter_pre - 1;
        state <= pre;
        writer <= TRUE;
        branch <= wPAGE;
    END IF;
END IF;

IF rising_edge(READ_IN) THEN
    
    IF writer = FALSE THEN
    CASE state IS
        WHEN ready =>
            NULL;       
        WHEN pre =>
            IF counter_pre = 0 THEN
                state <= reg;
                counter_pre <= pre_max;
                
                DATA_INT <= converter(regi((counter_regi)*3*8-1 DOWNTO (counter_regi-1)*3*8 ));
                counter_regi <= counter_regi -1;

                writer <= TRUE;
                
                
            ELSE
                DATA_INT <= converter(preamble((counter_pre)*3*8-1 DOWNTO (counter_pre-1)*3*8 ));
                counter_pre <= counter_pre - 1;
                
                IF counter_pre = 1 THEN
                WAIT_FLAG_IN <= '1';
                END IF;
                
                writer <= TRUE;
            END IF;

        WHEN reg =>
            IF counter_regi = 0 THEN
                state <= post;
                counter_regi <= regi_max;
                
                DATA_INT <= converter(postamble((counter_post)*3*8-1 DOWNTO (counter_post-1)*3*8 ));
                counter_post <= counter_post - 1;
                 writer <= TRUE;
            ELSE
                counter_regi <= counter_regi -1;
                DATA_INT <= converter(regi((counter_regi)*3*8-1 DOWNTO (counter_regi-1)*3*8 ));
                writer <= TRUE;
                
                WAITS <= '0';

            END IF;
            
        WHEN post =>
            IF counter_post = 0 THEN
               counter_post <= post_max;
                state <= fin;

            ELSE
                counter_post <= counter_post - 1;
                DATA_INT <= converter(postamble((counter_post)*3*8-1 DOWNTO (counter_post-1)*3*8 ));
                writer <= TRUE;
            END IF; 
            
        WHEN FIN =>
            FLAG <= '1';
            
        WHEN OTHERS =>
            NULL;
    END CASE;
    END IF;
    
    IF branch = wPAGE AND writer = TRUE THEN
        DATA <= DATA_INT(31 DOWNTO 24);
        branch <= wPAGE2;
        
        
       
        
    ELSIF branch = wPAGE2 AND writer = TRUE THEN
        DATA <= DATA_INT(23 DOWNTO 16);
        branch <= wDATA;
        
        

    ELSIF branch = wDATA AND writer = TRUE THEN
        DATA <= DATA_INT(15 DOWNTO 8);
        branch <= wDATA2;
        writer <= FALSE;

        
    ELSIF branch = wDATA2 AND writer = FALSE THEN
        DATA <= DATA_INT(7 DOWNTO 0);
        branch <= wPAGE;
       
       IF WAIT_FLAG_IN = '1' THEN
            WAITS <= '1';
            WAIT_FLAG_IN <= '0';
        END IF; 
       
        
    END IF;
    
END IF;
    
END PROCESS;

PROCESS(READ_IN)
BEGIN

    IF rising_edge(READ_IN) THEN
        TRIGGER_CHANGE <= '1';
    END IF;
    
    IF falling_edge(READ_IN) THEN
        TRIGGER_CHANGE<= '0';
    END IF;

END PROCESS;

end Behavioral;

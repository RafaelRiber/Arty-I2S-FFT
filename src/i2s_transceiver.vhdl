LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY i2s_transceiver IS
  GENERIC(
    MCLK_SCLK_ratio  : integer := 8;  --number of MCLKxCI periods per SCLKxCO period
    SCLK_LRCLK_ratio : integer := 64;  --number of SCLKxCO periods per word select period
    d_width          : integer := 24);  --data width
  PORT(
    RSTxRI : IN std_logic;              --asynchronous active high RSTxRI
    CLKxCI : IN std_logic;              -- Clock In (Defines MCLK)

    -- DAC IO
    MCLK_DACxCO  : OUT std_logic;
    LRCLK_DACxCO : OUT std_logic;
    SCLK_DACxCO  : OUT std_logic;
    DAC_SDxDO    : OUT std_logic;       --serial data transmit

    -- ADC IO
    MCLK_ADCxCO  : OUT std_logic;
    LRCLK_ADCxCO : OUT std_logic;
    SCLK_ADCxCO  : OUT std_logic;
    ADC_SDxDO     : IN  std_logic;       --serial data receive

    -- Data IO
    l_data_DAC : IN  std_logic_vector(d_width-1 DOWNTO 0);  --left channel data to transmit
    r_data_DAC : IN  std_logic_vector(d_width-1 DOWNTO 0);  --right channel data to transmit
    
    l_data_ADC : OUT std_logic_vector(d_width-1 DOWNTO 0);  --left channel data received
    r_data_ADC : OUT std_logic_vector(d_width-1 DOWNTO 0));  --right channel data received
END i2s_transceiver;

ARCHITECTURE logic OF i2s_transceiver IS

  SIGNAL SCLK_int      : std_logic := '0';  --internal serial clock signal
  SIGNAL LRCLK_int     : std_logic := '0';  --internal word select signal
  SIGNAL l_data_ADC_int : std_logic_vector(d_width-1 DOWNTO 0);  --internal left channel rx data buffer
  SIGNAL r_data_ADC_int : std_logic_vector(d_width-1 DOWNTO 0);  --internal right channel rx data buffer
  SIGNAL l_data_DAC_int : std_logic_vector(d_width-1 DOWNTO 0);  --internal left channel tx data buffer
  SIGNAL r_data_DAC_int : std_logic_vector(d_width-1 DOWNTO 0);  --internal right channel tx data buffer

BEGIN

  PROCESS(CLKxCI, RSTxRI)
    VARIABLE SCLK_cnt  : integer := 0;  --counter of master clocks during half period of serial clock
    VARIABLE LRCLK_cnt : integer := 0;  --counter of serial clock toggles during half period of word select
  BEGIN

    IF(RSTxRI = '1') THEN               --asynchronous RSTxRI
      SCLK_cnt      := 0;               --clear MCLKxCI/SCLKxCO counter
      LRCLK_cnt     := 0;               --clear SCLKxCO/LRCLKxCO counter
      SCLK_int      <= '0';             --clear serial clock signal
      LRCLK_int     <= '0';             --clear word select signal
      l_data_ADC_int <= (OTHERS => '0');  --clear internal left channel rx data buffer
      r_data_ADC_int <= (OTHERS => '0');  --clear internal right channel rx data buffer
      l_data_DAC_int <= (OTHERS => '0');  --clear internal left channel tx data buffer
      r_data_DAC_int <= (OTHERS => '0');  --clear internal right channel tx data buffer
      DAC_SDxDO      <= '0';             --clear serial data transmit output
      l_data_ADC     <= (OTHERS => '0');  --clear left channel received data output
      r_data_ADC     <= (OTHERS => '0');  --clear right channel received data output
    ELSIF(CLKxCI'event AND CLKxCI = '1') THEN  --master clock rising edge
      IF(SCLK_cnt < MCLK_SCLK_ratio/2-1) THEN  --less than half period of SCLKxCO
        SCLK_cnt := SCLK_cnt + 1;       --increment MCLKxCI/SCLKxCO counter
      ELSE                              --half period of SCLKxCO
        SCLK_cnt := 0;                  --RSTxRI MCLKxCI/SCLKxCO counter
        SCLK_int <= NOT SCLK_int;       --toggle serial clock
        IF(LRCLK_cnt < SCLK_LRCLK_ratio-1) THEN  --less than half period of LRCLKxCO
          LRCLK_cnt := LRCLK_cnt + 1;   --increment SCLKxCO/LRCLKxCO counter
          IF(SCLK_int = '0' AND LRCLK_cnt > 1 AND LRCLK_cnt < d_width*2+2) THEN  --rising edge of SCLKxCO during data word
            IF(LRCLK_int = '1') THEN    --right channel
              r_data_ADC_int <= r_data_ADC_int(d_width-2 DOWNTO 0) & ADC_SDxDO;  --shift data bit into right channel rx data buffer
            ELSE                        --left channel
              l_data_ADC_int <= l_data_ADC_int(d_width-2 DOWNTO 0) & ADC_SDxDO;  --shift data bit into left channel rx data buffer
            END IF;
          END IF;
          IF(SCLK_int = '1' AND LRCLK_cnt < d_width*2+3) THEN  --falling edge of SCLKxCO during data word
            IF(LRCLK_int = '1') THEN    --right channel
              DAC_SDxDO      <= r_data_DAC_int(d_width-1);  --transmit serial data bit 
              r_data_DAC_int <= r_data_DAC_int(d_width-2 DOWNTO 0) & '0';  --shift data of right channel tx data buffer
            ELSE                        --left channel
              DAC_SDxDO      <= l_data_DAC_int(d_width-1);  --transmit serial data bit
              l_data_DAC_int <= l_data_DAC_int(d_width-2 DOWNTO 0) & '0';  --shift data of left channel tx data buffer
            END IF;
          END IF;
        ELSE                            --half period of LRCLKxCO
          LRCLK_cnt     := 0;           --RSTxRI SCLKxCO/LRCLKxCO counter
          LRCLK_int     <= NOT LRCLK_int;  --toggle word select
          r_data_ADC     <= r_data_ADC_int;  --output right channel received data
          l_data_ADC     <= l_data_ADC_int;  --output left channel received data
          r_data_DAC_int <= r_data_DAC;  --latch in right channel data to transmit
          l_data_DAC_int <= l_data_DAC;  --latch in left channel data to transmit
        END IF;
      END IF;
    END IF;
  END PROCESS;

  MCLK_DACxCO <= CLKxCI;
  LRCLK_DACxCO <= LRCLK_int;
  SCLK_DACxCO <= SCLK_int;

  MCLK_ADCxCO <= CLKxCI;
  LRCLK_ADCxCO <= LRCLK_int;
  SCLK_ADCxCO <= SCLK_int;
  
END logic;


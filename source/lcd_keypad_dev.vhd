--------------------------------------------------------------------------------
-- Filename     : lcd_keypad_dev.vhd
-- Author       : Chris Lloyd
-- Date Created : 2019-03-04
-- Last Revised : 2019-04-06
-- Project      : lcd_keypad_dev (Main Entity)
-- Description  : Entity to test the functionality of a keypad and lcd.
--------------------------------------------------------------------------------
-- Todos:
-- 1. Solve all CDL=>
-- 2. Rename external signals to all caps with I_ and O_ and IO_ as needed
--
-- Done:
--
--------------------------------------------------------------------------------

----------------
--  Packages  --
----------------
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;
package lcd_keypad_dev_util is
  type t_lcd_display_data is array (31 downto 0) of std_logic_vector(7 downto 0);
end package lcd_keypad_dev_util;


-----------------
--  Libraries  --
-----------------
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library work;
  use work.lcd_keypad_dev_util.all;

--------------
--  Entity  --
--------------
entity lcd_keypad_dev is
port
(
  -- Clocks
  clk_in_100mhz          : in    std_logic;

  -- Keypad Inputs (rows)
  keypad_row_1           : in    std_logic;
  keypad_row_2           : in    std_logic;
  keypad_row_3           : in    std_logic;
  keypad_row_4           : in    std_logic;

  -- Keypad Outputs (cols)
  keypad_col_1           : out   std_logic;
  keypad_col_2           : out   std_logic;
  keypad_col_3           : out   std_logic;
  keypad_col_4           : out   std_logic;

  -- LCD Control Lines
  lcd_enable             : out   std_logic;
  lcd_data_high_ctrl_low : out   std_logic;
  lcd_rd_high_wr_low     : out   std_logic;

  -- LCD Data Lines (bidirectional)
  lcd_data               : inout std_logic_vector(7 downto 0);

  -- MAX10 Dev Board Input DIP Switches
  dip_sw_1               : in    std_logic;
  dip_sw_2               : in    std_logic;
  dip_sw_3               : in    std_logic;
  dip_sw_4               : in    std_logic;
  dip_sw_5               : in    std_logic;

  -- MAX10 Dev Board Output LEDs (active low)
  led_1                  : out   std_logic;
  led_2                  : out   std_logic;
  led_3                  : out   std_logic;
  led_4                  : out   std_logic;
  led_5                  : out   std_logic
);
end entity lcd_keypad_dev;


--------------------------------
--  Architecture Declaration  --
--------------------------------
architecture rtl of lcd_keypad_dev is
  -------------
  -- SIGNALS --
  -------------
  -- Global reset counter and enable line
  signal s_reset_fw_cntr    : unsigned(23 downto 0);
  signal s_system_rst       : std_logic;

  -- 4 bit binary representation of keypad state
  signal s_keypad_binary    : std_logic_vector(3 downto 0);

  signal s_lcd_display_data : t_lcd_display_data;
  signal s_lcd_trigger      : std_logic;
  signal s_lcd_busy         : std_logic;
  signal s_do_write_once    : std_logic;

  ----------------
  -- COMPONENTS --
  ----------------
  component hex_keypad_driver
  port(
    -- Clocks & Resets
    I_CLK_100MHZ    : in  std_logic;
    I_SYSTEM_RST    : in  std_logic;

    -- Keypad Inputs (rows)
    I_KEYPAD_ROW_1  : in  std_logic;
    I_KEYPAD_ROW_2  : in  std_logic;
    I_KEYPAD_ROW_3  : in  std_logic;
    I_KEYPAD_ROW_4  : in  std_logic;

    -- Keypad Outputs (cols)
    O_KEYPAD_COL_1  : out std_logic;
    O_KEYPAD_COL_2  : out std_logic;
    O_KEYPAD_COL_3  : out std_logic;
    O_KEYPAD_COL_4  : out std_logic;

    -- Output binary representation of keypad state
    O_KEYPAD_BINARY : out std_logic_vector(3 downto 0)
  );
  end component;

  component lcd_driver
  port(
    -- Clocks & Resets
    I_CLK_100MHZ              : in    std_logic;
    I_SYSTEM_RST              : in    std_logic;

    -- User signals
    I_LCD_DISPLAY_DATA        : in    t_lcd_display_data;
    I_LCD_TRIGGER             : in    std_logic;
    O_LCD_BUSY                : out   std_logic;

    -- Pass through external signals
    O_LCD_ENABLE              : out   std_logic;
    O_LCD_DATA_HIGH_CTRL_LOW  : out   std_logic;
    O_LCD_RD_HIGH_WR_LOW      : out   std_logic;
    IO_LCD_DATA               : inout std_logic_vector(7 downto 0)
  );
  end component;

begin
  ------------------------------------------------------------------------------
  -- Process Name     : SYSTEM_RST_OUTPUT
  -- Sensitivity List : clk_in_100mhz : 100 MHz global clock
  -- Useful Outputs   : s_system_rst  : Global Reset line
  --                                    (active high reset logic)
  -- Description      : System FW Reset Output logic (active high reset logic).
  --                    Should only require a few clock ticks to reset at most,
  --                    but holding design in reset for 50ms to be conservative
  --                    since we are in no hurry to startup.
  --                    Note: 50 ms time based on other lcd example code.
  --    `               CDL=> Check this value later.
  ------------------------------------------------------------------------------
  SYSTEM_RST_OUTPUT: process (clk_in_100mhz)
    -- Count duration of 50ms => (50,000us * 100MHz clk) = 5,000,000 counts
    -- = 0x‭4C_4B40‬
    constant C_50MS_DURATION : unsigned(23 downto 0) := x"4C_4B40";

  begin
    if (rising_edge(clk_in_100mhz)) then
      if (s_reset_fw_cntr = C_50MS_DURATION) then
        s_reset_fw_cntr   <= s_reset_fw_cntr;
        s_system_rst      <= '0';
      else
        s_reset_fw_cntr   <= s_reset_fw_cntr + 1;
        s_system_rst      <= '1';
      end if;
    end if;
  end process SYSTEM_RST_OUTPUT;
  ------------------------------------------------------------------------------

  -- Entity to control the powering and reading of the keypad rows and columns.
  -- Outputs the current binary number of keypad (0-15)
  HEX_KEYPAD_DRIVER_0: hex_keypad_driver
  port map (
    -- Clocks & Resets
    I_CLK_100MHZ    => clk_in_100mhz,
    I_SYSTEM_RST    => s_system_rst,

    -- Keypad Inputs (rows)
    I_KEYPAD_ROW_1  => keypad_row_1,
    I_KEYPAD_ROW_2  => keypad_row_2,
    I_KEYPAD_ROW_3  => keypad_row_3,
    I_KEYPAD_ROW_4  => keypad_row_4,

    -- Keypad Outputs (cols)
    O_KEYPAD_COL_1  => keypad_col_1,
    O_KEYPAD_COL_2  => keypad_col_2,
    O_KEYPAD_COL_3  => keypad_col_3,
    O_KEYPAD_COL_4  => keypad_col_4,

    -- Output binary representation of keypad state
    O_KEYPAD_BINARY => s_keypad_binary
  );
  ------------------------------------------------------------------------------

  ------------------------------------------------------------------------------
  -- Process Name     : SET_LED_OUTPUTS
  -- Sensitivity List : clk_in_100mhz : 100 MHz global clock
  --                    s_system_rst  : Global Reset line
  -- Useful Outputs   : led_(1-4)     : Output LEDs (active low)
  -- Description      : Test process to set the value of s_keypad_binary to the
  --                    on board LEDs.
  ------------------------------------------------------------------------------
  SET_LED_OUTPUTS: process (clk_in_100mhz, s_system_rst)
  begin
    if (s_system_rst = '1') then
      led_1 <= '1';
      led_2 <= '1';
      led_3 <= '1';
      led_4 <= '1';
      led_5 <= '1';  -- Unused in current design set to high to turn led off

    elsif (rising_edge(clk_in_100mhz)) then
      led_1 <= not s_keypad_binary(0);
      led_2 <= not s_keypad_binary(1);
      led_3 <= not s_keypad_binary(2);
      led_4 <= not s_keypad_binary(3);
      led_5 <= '1';  -- Unused in current design set to high to turn led off
    end if;
  end process SET_LED_OUTPUTS;
  ------------------------------------------------------------------------------

  -- Entity to control an lcd device based on a entire screen block of data.
  LCD_DRIVER_0: lcd_driver
  port map (
    -- Clocks & Resets
    I_CLK_100MHZ             => clk_in_100mhz,
    I_SYSTEM_RST             => s_system_rst,

    -- User signals
    I_LCD_DISPLAY_DATA       => s_lcd_display_data,
    I_LCD_TRIGGER            => s_lcd_trigger,
    O_LCD_BUSY               => s_lcd_busy,

    -- Pass through external signals
    O_LCD_ENABLE             => lcd_enable,
    O_LCD_DATA_HIGH_CTRL_LOW => lcd_data_high_ctrl_low,
    O_LCD_RD_HIGH_WR_LOW     => lcd_rd_high_wr_low,
    IO_LCD_DATA              => lcd_data
  );
  ------------------------------------------------------------------------------

  ------------------------------------------------------------------------------
  -- Process Name     : LCD_TEST
  -- Sensitivity List : clk_in_100mhz : 100 MHz global clock
  --                    s_system_rst  : Global Reset line
  -- Useful Outputs   : s_lcd_display_data : Data to lower level process
  --                  : s_lcd_trigger      : Trigger to indicate start of operation
  -- Description      : Test process to drive lower level lcd driver.
  ------------------------------------------------------------------------------
  LCD_TEST: process (clk_in_100mhz, s_system_rst)
  begin
    if (s_system_rst = '1') then
      s_lcd_display_data   <= (others => (others => ('0')));
      s_lcd_trigger        <= '0';
      s_do_write_once      <= '0';

    elsif (rising_edge(clk_in_100mhz)) then
      if (s_lcd_busy = '0' and s_do_write_once = '0') then
        s_lcd_display_data <= (others => (x"42"));  -- CDL=> ascii "A"
        s_lcd_trigger      <= '1';
        s_do_write_once    <= '1';
      else
        s_lcd_display_data <= s_lcd_display_data;
        s_lcd_trigger      <= '0';
        s_do_write_once    <= s_do_write_once;
      end if;
    end if;
  end process LCD_TEST;
  ------------------------------------------------------------------------------
end architecture rtl;






-- Testing (CDL=> Remove later)
  -- Testing logic to probe inputs either high or low depending on the
  -- state of dip_sw_1 CDL=> Remove later, used only for testing
  --DRIVE_ALL_HIGH: process (clk_in_100mhz, dip_sw_1)
  --begin
  --  if (dip_sw_1 = '1') then
  --    keypad_col_1 <= '1';
  --    keypad_col_2 <= '1';
  --    keypad_col_3 <= '1';
  --    keypad_col_4 <= '1';
  --    keypad_row_1 <= '1';
  --    keypad_row_2 <= '1';
  --    keypad_row_3 <= '1';
  --    keypad_row_4 <= '1';
  --    lcd_data_high_ctrl_low <= '1';
  --    lcd_enable <= '1';
  --    lcd_rd_high_wr_low <= '1';
  --    lcd_data <= (others => '1');
  --    led_1 <= '0';
  --    led_2 <= '0';
  --    led_3 <= '0';
  --    led_4 <= '0';
  --    led_5 <= '0';
  --  else
  --    keypad_col_1 <= '0';
  --    keypad_col_2 <= '0';
  --    keypad_col_3 <= '0';
  --    keypad_col_4 <= '0';
  --    keypad_row_1 <= '0';
  --    keypad_row_2 <= '0';
  --    keypad_row_3 <= '0';
  --    keypad_row_4 <= '0';
  --    lcd_data_high_ctrl_low <= '0';
  --    lcd_enable <= '0';
  --    lcd_rd_high_wr_low <= '0';
  --    lcd_data <= (others => '0');
  --    led_1 <= '1';
  --    led_2 <= '1';
  --    led_3 <= '1';
  --    led_4 <= '1';
  --    led_5 <= '1';
  --  end if;
  --end process DRIVE_ALL_HIGH;

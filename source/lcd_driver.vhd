--------------------------------------------------------------------------------
-- Filename     : lcd_driver
-- Author       : Chris Lloyd
-- Date Created : 2019-02-28
-- Last Revised : 2019-04-06
-- Project      : lcd_keypad_dev
-- Description  : Driver to control a 16X2 LCD unit (ADM1602K-NSW_FBS/3.3V)
--------------------------------------------------------------------------------
-- Todos:
-- 1. Solve all CDL=>
-- 2. Add process descriptions
-- 3. Ensure indentation and spacing is clean and consistent.
--
-- Done:
--
--------------------------------------------------------------------------------

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
entity lcd_driver is
port
(
  -- Clocks & Resets
  I_CLK_100MHZ             : in    std_logic;
  I_SYSTEM_RST             : in    std_logic;

  -- User signals
  I_LCD_DISPLAY_DATA       : in    t_lcd_display_data;
  I_LCD_TRIGGER            : in    std_logic;
  O_LCD_BUSY               : out   std_logic;

  -- Pass through external signals
  O_LCD_ENABLE             : out   std_logic;
  O_LCD_DATA_HIGH_CTRL_LOW : out   std_logic;
  O_LCD_RD_HIGH_WR_LOW     : out   std_logic;
  IO_LCD_DATA              : inout std_logic_vector(7 downto 0)
);
end entity lcd_driver;

--------------------------------
--  Architecture Declaration  --
--------------------------------
architecture rtl of lcd_driver is

  -------------
  -- SIGNALS --
  -------------
  signal s_lcd_state_cntr         : unsigned(7 downto 0);

  signal s_lcd_write_index_cntr   : unsigned(4 downto 0);  -- CDL=> What's this?

  type t_LCD_STATE is (
                       -- Configuration States
                       INIT_FUNC_SET_STATE,
                       INIT_CLR_DISP_STATE,
                       INIT_RET_HOME_STATE,
                       INIT_ENTRY_MODE_STATE,
                       INIT_DISP_ON_OFFS_STATE,

                       -- Normal Operation States
                       READY_STATE,
                       WRITE_ADDR_STATE,
                       WRITE_DATA_STATE,
                       CHECK_BUSY_STATE
                    );
  signal s_lcd_state_max_count    : unsigned(7 downto 0);
  signal s_lcd_next_action        : t_LCD_STATE;
  signal s_lcd_current_state      : t_LCD_STATE;

  signal s_lcd_write_byte         : std_logic_vector(7 downto 0);
  signal s_lcd_write_en           : std_logic;
  signal s_lcd_read_en            : std_logic;

  signal s_lcd_read_byte          : std_logic_vector(7 downto 0);
  signal s_lcd_data_high_ctrl_low : std_logic;
  signal s_lcd_enable             : std_logic;
  signal s_lcd_rd_high_wr_low     : std_logic;
  signal s_lcd_data_in            : std_logic_vector(7 downto 0);
  signal s_lcd_data_out           : std_logic_vector(7 downto 0);

-- CDL=> Deal with when to start / rollover logic
begin
  LCD_STATE_CNTR: process (I_CLK_100MHZ, I_SYSTEM_RST)
  begin
    if (I_SYSTEM_RST = '1') then
      s_lcd_state_cntr    <= (others => '0');

    elsif (rising_edge(I_CLK_100MHZ)) then
      if (s_lcd_state_cntr = s_lcd_state_max_count) then  -- Rollover case  -- CDL=> Initial value for s_lcd_state_max_count?
        s_lcd_state_cntr  <= (others => '0');
      else                                                -- Increment case
        s_lcd_state_cntr  <= s_lcd_state_cntr + 1;
      end if;
    end if;
  end process LCD_STATE_CNTR;
  ------------------------------------------------------------------------------

  LCD_ADDR_INDEX_CNTR: process (I_CLK_100MHZ, I_SYSTEM_RST)
  begin
    if (I_SYSTEM_RST = '1') then
      s_lcd_write_index_cntr    <= (others => '0');

    elsif (rising_edge(I_CLK_100MHZ)) then
      if    (s_lcd_write_index_cntr = ('1' & x"F")) then        -- Rollover case -- CDL=> Here
        s_lcd_write_index_cntr  <= (others => '0');

      elsif (     (s_lcd_current_state = WRITE_DATA_STATE) 
              and (s_lcd_state_cntr = s_lcd_state_max_count) ) then
        s_lcd_write_index_cntr  <= s_lcd_write_index_cntr + 1;  -- Increment case

      else
        s_lcd_write_index_cntr  <= s_lcd_write_index_cntr;
      end if;
    end if;
  end process LCD_ADDR_INDEX_CNTR;
  ------------------------------------------------------------------------------

  LCD_STATE_MACHINE: process (I_CLK_100MHZ, I_SYSTEM_RST)
  begin
    if (I_SYSTEM_RST = '1') then
      s_lcd_state_max_count <= x"00";
      s_lcd_next_action <= INIT_FUNC_SET_STATE;
      s_lcd_current_state <= CHECK_BUSY_STATE;

    elsif (rising_edge(I_CLK_100MHZ)) then
      case s_lcd_current_state is

        ------------------------------------------------------------------------
        -- Configuration States
        ------------------------------------------------------------------------
        when INIT_FUNC_SET_STATE =>
          s_lcd_state_max_count <= x"64";  -- 1 uS
          s_lcd_next_action <= INIT_CLR_DISP_STATE;

          if (s_lcd_state_cntr = s_lcd_state_max_count) then
            s_lcd_current_state <= CHECK_BUSY_STATE;
          else
            s_lcd_current_state <= s_lcd_current_state;
          end if;

        when INIT_CLR_DISP_STATE =>
          s_lcd_state_max_count <= x"64";  -- 1 uS
          s_lcd_next_action <= INIT_RET_HOME_STATE;

          if (s_lcd_state_cntr = s_lcd_state_max_count) then
            s_lcd_current_state <= CHECK_BUSY_STATE;
          else
            s_lcd_current_state <= s_lcd_current_state;
          end if;

        when INIT_RET_HOME_STATE =>
          s_lcd_state_max_count <= x"64";  -- 1 uS
          s_lcd_next_action <= INIT_ENTRY_MODE_STATE;

          if (s_lcd_state_cntr = s_lcd_state_max_count) then
            s_lcd_current_state <= CHECK_BUSY_STATE;
          else
            s_lcd_current_state <= s_lcd_current_state;
          end if;

        when INIT_ENTRY_MODE_STATE =>
          s_lcd_state_max_count <= x"64";  -- 1 uS
          s_lcd_next_action <= INIT_DISP_ON_OFFS_STATE;

          if (s_lcd_state_cntr = s_lcd_state_max_count) then
            s_lcd_current_state <= CHECK_BUSY_STATE;
          else
            s_lcd_current_state <= s_lcd_current_state;
          end if;

        when INIT_DISP_ON_OFFS_STATE =>
          s_lcd_state_max_count <= x"64";  -- 1 uS
          s_lcd_next_action <= READY_STATE;

          if (s_lcd_state_cntr = s_lcd_state_max_count) then
            s_lcd_current_state <= CHECK_BUSY_STATE;
          else
            s_lcd_current_state <= s_lcd_current_state;
          end if;

        ------------------------------------------------------------------------
        -- Normal Operation States
        ------------------------------------------------------------------------
        when READY_STATE =>
          s_lcd_next_action <= WRITE_ADDR_STATE;

          if (I_LCD_TRIGGER = '1') then
            s_lcd_state_max_count <= x"FF";  -- CDL=> Here
            s_lcd_current_state <= s_lcd_next_action;
          else
            s_lcd_state_max_count <= x"00";  -- CDL=> Here
            s_lcd_current_state <= s_lcd_current_state;
          end if;

        when WRITE_ADDR_STATE =>
          s_lcd_state_max_count <= x"64";  -- 1 uS
          s_lcd_next_action <= WRITE_DATA_STATE;

          if (s_lcd_state_cntr = s_lcd_state_max_count) then
            s_lcd_current_state <= CHECK_BUSY_STATE;
          else
            s_lcd_current_state <= s_lcd_current_state;
          end if;

        when WRITE_DATA_STATE =>
          s_lcd_state_max_count <= x"64";  -- 1 uS

          -- CDL=> OBOB?
          if (s_lcd_write_index_cntr = ('0' & x"F")) then
            s_lcd_next_action <= WRITE_ADDR_STATE;
          elsif (s_lcd_write_index_cntr = ('1' & x"F")) then
            s_lcd_next_action <= READY_STATE;
          else
            s_lcd_next_action <= WRITE_DATA_STATE;
          end if;

          if (s_lcd_state_cntr = s_lcd_state_max_count) then
            s_lcd_current_state <= CHECK_BUSY_STATE;
          else
            s_lcd_current_state <= s_lcd_current_state;
          end if;

        when CHECK_BUSY_STATE =>
          s_lcd_state_max_count <= x"64";  -- 1 uS
          s_lcd_next_action <= s_lcd_next_action;

          if (s_lcd_state_cntr = s_lcd_state_max_count) then
            if (s_lcd_read_byte(7) = '1') then  -- Still Busy
              s_lcd_current_state <= CHECK_BUSY_STATE;
            else
              s_lcd_current_state <= s_lcd_next_action;
            end if;
          else
            s_lcd_current_state <= s_lcd_current_state;
          end if;

        -- Error condition, should never occur
        when others =>
          s_lcd_state_max_count <= x"00";
          s_lcd_next_action <= INIT_FUNC_SET_STATE;
          s_lcd_current_state <= CHECK_BUSY_STATE;
        end case;
    end if;
  end process LCD_STATE_MACHINE;
  ------------------------------------------------------------------------------

  LCD_CONTROL: process (I_CLK_100MHZ, I_SYSTEM_RST)
    constant C_INIT_FUNC_SET_FLAG     : std_logic_vector(7 downto 0) := x"38";
    constant C_INIT_CLR_DISP_FLAG     : std_logic_vector(7 downto 0) := x"01";
    constant C_INIT_RET_HOME_FLAG     : std_logic_vector(7 downto 0) := x"02";
    constant C_INIT_ENTRY_MODE_FLAG   : std_logic_vector(7 downto 0) := x"06";
    constant C_INIT_DISP_ON_OFFS_FLAG : std_logic_vector(7 downto 0) := x"0C";
    constant C_LINE_1_ADDR_FLAG       : std_logic_vector(7 downto 0) := x"00"; -- CDL=> Here
    constant C_LINE_2_ADDR_FLAG       : std_logic_vector(7 downto 0) := x"40"; -- CDL=> Here
  begin
    if (I_SYSTEM_RST = '1') then
      O_LCD_BUSY <= '1';
      s_lcd_write_byte <= (others=> '0');
      s_lcd_write_en   <= '0';  -- CDL=> To-Do
      s_lcd_read_en    <= '0';  -- CDL=> To-Do
      s_lcd_data_high_ctrl_low <= '0';  -- CDL=> To-Do

    elsif (rising_edge(I_CLK_100MHZ)) then
      -- External busy state
      if (s_lcd_current_state = READY_STATE) then
        O_LCD_BUSY <= '0';
      else
        O_LCD_BUSY <= '1';
      end if;

      case (s_lcd_current_state) is
        when INIT_FUNC_SET_STATE =>
          s_lcd_write_byte         <= C_INIT_FUNC_SET_FLAG;
          s_lcd_write_en           <= '1';
          s_lcd_read_en            <= '0';
          s_lcd_data_high_ctrl_low <= '0';

        when INIT_CLR_DISP_STATE =>
          s_lcd_write_byte         <= C_INIT_CLR_DISP_FLAG;
          s_lcd_write_en           <= '1';
          s_lcd_read_en            <= '0';
          s_lcd_data_high_ctrl_low <= '0';

        when INIT_RET_HOME_STATE =>
          s_lcd_write_byte         <= C_INIT_RET_HOME_FLAG;
          s_lcd_write_en           <= '1';
          s_lcd_read_en            <= '0';
          s_lcd_data_high_ctrl_low <= '0';

        when INIT_ENTRY_MODE_STATE =>
          s_lcd_write_byte         <= C_INIT_ENTRY_MODE_FLAG;
          s_lcd_write_en           <= '1';
          s_lcd_read_en            <= '0';
          s_lcd_data_high_ctrl_low <= '0';

        when INIT_DISP_ON_OFFS_STATE =>
          s_lcd_write_byte         <= C_INIT_DISP_ON_OFFS_FLAG;
          s_lcd_write_en           <= '1';
          s_lcd_read_en            <= '0';
          s_lcd_data_high_ctrl_low <= '0';

        when READY_STATE =>
          s_lcd_write_byte         <= (others=> '0');
          s_lcd_write_en           <= '0';
          s_lcd_read_en            <= '0';
          s_lcd_data_high_ctrl_low <= '0';

        when WRITE_ADDR_STATE =>

          -- CDL=> OBOB?
          if (s_lcd_write_index_cntr = ('0' & x"0")) then
            s_lcd_write_byte       <= C_LINE_1_ADDR_FLAG;
          else  -- CDL=> Should be elsif?
            s_lcd_write_byte       <= C_LINE_2_ADDR_FLAG;
          end if;
          s_lcd_write_en           <= '1';
          s_lcd_read_en            <= '0';
          s_lcd_data_high_ctrl_low <= '0';

        when WRITE_DATA_STATE =>
          s_lcd_write_byte         <= I_LCD_DISPLAY_DATA(to_integer(s_lcd_write_index_cntr));-- CDL=> Here;
          s_lcd_write_en           <= '1';
          s_lcd_read_en            <= '0';
          s_lcd_data_high_ctrl_low <= '1';

        when CHECK_BUSY_STATE =>
          s_lcd_write_byte         <= (others=> '0');
          s_lcd_write_en           <= '0';
          s_lcd_read_en            <= '1';
          s_lcd_data_high_ctrl_low <= '0';

        when others =>
          s_lcd_write_byte         <= (others=> '0');
          s_lcd_write_en           <= '0';
          s_lcd_read_en            <= '0';
          s_lcd_data_high_ctrl_low <= '0';
      end case;
    end if;
  end process LCD_CONTROL;
  ------------------------------------------------------------------------------

  LCD_RD_WR_DATA: process (I_CLK_100MHZ, I_SYSTEM_RST)
  begin
    if (I_SYSTEM_RST = '1') then
      s_lcd_rd_high_wr_low       <= '0';
      s_lcd_data_out             <= (others=> '0');
      s_lcd_enable               <= '0';
      s_lcd_read_byte            <= (others=> '0');

    elsif (rising_edge(I_CLK_100MHZ)) then
      if (s_lcd_write_en = '1') then
        case (to_integer(s_lcd_state_cntr)) is
          when 5 =>
            s_lcd_rd_high_wr_low <= '0';
            s_lcd_data_out       <= s_lcd_write_byte;
            s_lcd_enable         <= '0';
          when 35 =>
            s_lcd_rd_high_wr_low <= s_lcd_rd_high_wr_low;
            s_lcd_data_out       <= s_lcd_data_out;
            s_lcd_enable         <= '1';
          when 75 =>
            s_lcd_rd_high_wr_low <= s_lcd_rd_high_wr_low;
            s_lcd_data_out       <= s_lcd_data_out;
            s_lcd_enable         <= '0';
          --when 95 =>  -- CDL=> Remove later
          when others =>
            s_lcd_rd_high_wr_low <= s_lcd_rd_high_wr_low;
            s_lcd_data_out       <= s_lcd_data_out;
            s_lcd_enable         <= '0';
        end case;

        s_lcd_read_byte          <= s_lcd_read_byte;
      elsif (s_lcd_read_en = '1') then
        case (to_integer(s_lcd_state_cntr)) is
          when 5 =>
            s_lcd_rd_high_wr_low <= '1';
            s_lcd_enable         <= '0';
            s_lcd_read_byte      <= s_lcd_read_byte;
          when 35 =>
            s_lcd_rd_high_wr_low <= s_lcd_rd_high_wr_low;
            s_lcd_enable         <= '1';
            s_lcd_read_byte      <= s_lcd_read_byte;
          when 75 =>
            s_lcd_rd_high_wr_low <= s_lcd_rd_high_wr_low;
            s_lcd_enable         <= '0';
            s_lcd_read_byte      <= s_lcd_data_in;  -- CDL=> Here?
          --when 95 =>  -- CDL=> Remove later
          when others =>
            s_lcd_rd_high_wr_low <= s_lcd_rd_high_wr_low;
            s_lcd_enable         <= '0';
            s_lcd_read_byte      <= s_lcd_read_byte;
        end case;
        s_lcd_data_out           <= s_lcd_data_out;
      else
        s_lcd_rd_high_wr_low     <= '0';
        s_lcd_data_out           <= s_lcd_data_out;
        s_lcd_enable             <= '0';
        s_lcd_read_byte          <= s_lcd_read_byte;
      end if;
    end if;
  end process LCD_RD_WR_DATA;
  ------------------------------------------------------------------------------

  O_LCD_DATA_HIGH_CTRL_LOW <= s_lcd_data_high_ctrl_low;
  O_LCD_RD_HIGH_WR_LOW     <= s_lcd_rd_high_wr_low;
  O_LCD_ENABLE             <= s_lcd_enable;

  IO_LCD_DATA <= s_lcd_data_out when s_lcd_write_en = '1' else (others => 'Z');
  s_lcd_data_in <= IO_LCD_DATA; -- when s_lcd_write_en = '0' else (others => '0'); -- CDL=> Remove later

end architecture rtl;

-- CDL=> Testing
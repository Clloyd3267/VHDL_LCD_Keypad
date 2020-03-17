--------------------------------------------------------------------------------
-- Filename     : hex_keypad_driver.vhd
-- Author       : Chris Lloyd
-- Date Created : 2019-02-26
-- Last Revised : 2019-02-28
-- Project      : lcd_keypad_dev
-- Description  : Driver code to return a binary representation of the output
--                of a 16 button keypad (Sparkfun DD-14881)
--------------------------------------------------------------------------------
-- Todos:
--
-- Done:
-- 1. Solve all CDL=>
-- 2. Add process descriptions
-- 3. Ensure indentation and spacing is clean and consistent.
--
--------------------------------------------------------------------------------

-----------------
--  Libraries  --
-----------------
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

--------------
--  Entity  --
--------------
entity hex_keypad_driver is
port
(
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

  -- 4 bit binary representation of keypad state (output of entity)
  O_KEYPAD_BINARY : out std_logic_vector(3 downto 0)
);
end entity hex_keypad_driver;


--------------------------------
--  Architecture Declaration  --
--------------------------------
architecture rtl of hex_keypad_driver is

  -------------
  -- SIGNALS --
  -------------

  -- Keypad counter and enable to account for delay time
  signal s_keypad_enable      : std_logic;
  signal s_keypad_enable_cntr : unsigned(6 downto 0);

  -- State related signals
  type t_STATE is (IDLE_STATE,
                   COL1_POWER_STATE, COL1_READ_STATE,
                   COL2_POWER_STATE, COL2_READ_STATE,
                   COL3_POWER_STATE, COL3_READ_STATE,
                   COL4_POWER_STATE, COL4_READ_STATE);
  signal s_keypad_state       : t_STATE;

  -- Signals to allow current state of columns to be read as well as written to
  signal s_keypad_col_1       : std_logic;
  signal s_keypad_col_2       : std_logic;
  signal s_keypad_col_3       : std_logic;
  signal s_keypad_col_4       : std_logic;

  -- 4 bit binary representation of keypad state (output of entity)
  signal s_keypad_binary      : std_logic_vector(3 downto 0);

begin
  ------------------------------------------------------------------------------
  -- Process Name     : KEYPAD_EN_CNTR
  -- Sensitivity List : I_CLK_100MHZ    : 100 MHz global clock
  --                    I_SYSTEM_RST    : Global Reset line
  -- Useful Outputs   : s_keypad_enable : Enable line to allow state to change
  --                    in KEYPAD_STATE_MACHINE process
  --                    (active high enable logic)
  -- Description      : Counter to delay the powering of the columns to negate
  --                    the delay of the Hardware. Every 1111111b (127) clock
  --                    ticks (1.27 us), s_keypad_enable gets driven high to
  --                    allow for state change in KEYPAD_STATE_MACHINE process.
  ------------------------------------------------------------------------------
  KEYPAD_EN_CNTR: process (I_CLK_100MHZ, I_SYSTEM_RST)
  begin
    if (I_SYSTEM_RST = '1') then
      s_keypad_enable_cntr  <= (others => '0');
      s_keypad_enable       <= '0';

    elsif (rising_edge(I_CLK_100MHZ)) then
      s_keypad_enable_cntr  <= s_keypad_enable_cntr + 1;

      if (s_keypad_enable_cntr = "1111111") then  -- Max count 127 (1.27 us)
        s_keypad_enable     <= '1';
      else
        s_keypad_enable     <= '0';
      end if;
    end if;
  end process KEYPAD_EN_CNTR;
  ------------------------------------------------------------------------------

  ------------------------------------------------------------------------------
  -- Process Name     : KEYPAD_STATE_MACHINE
  -- Sensitivity List : I_CLK_100MHZ    : 100 MHz global clock
  --                    I_SYSTEM_RST    : Global Reset line
  -- Useful Outputs   : s_keypad_state  : Current state of keypad state machine.
  --                                      Used to control read and write of row
  --                                      and cols in KEYPAD_TO_BINARY process.
  -- Description      : State machine to control different states for power and
  --                    and read of rows and cols. Always a power state then a
  --                    read state.
  ------------------------------------------------------------------------------
  KEYPAD_STATE_MACHINE: process (I_CLK_100MHZ, I_SYSTEM_RST)
  begin
    if (I_SYSTEM_RST = '1') then  -- Upon reset, set the state to IDLE_STATE
      s_keypad_state          <= IDLE_STATE;

    elsif (rising_edge(I_CLK_100MHZ)) then  -- CDL=> Why be explicit about clock
      if (s_keypad_enable = '1') then
        case s_keypad_state is
          when IDLE_STATE =>
              s_keypad_state  <= COL2_POWER_STATE; -- CDL=> COL1_POWER_STATE?

          when COL1_POWER_STATE =>
              s_keypad_state  <= COL1_READ_STATE;
          when COL1_READ_STATE =>
              s_keypad_state  <= COL2_POWER_STATE;

          when COL2_POWER_STATE =>
              s_keypad_state  <= COL2_READ_STATE;
          when COL2_READ_STATE =>
              s_keypad_state  <= COL3_POWER_STATE;

          when COL3_POWER_STATE =>
              s_keypad_state  <= COL3_READ_STATE;
          when COL3_READ_STATE =>
              s_keypad_state  <= COL4_POWER_STATE;

          when COL4_POWER_STATE =>
              s_keypad_state  <= COL4_READ_STATE;
          when COL4_READ_STATE =>
              s_keypad_state  <= COL1_POWER_STATE;

          -- Error condition, should never occur
          when others =>
            s_keypad_state    <= IDLE_STATE;
        end case;
      else
        s_keypad_state        <= s_keypad_state;
      end if;
    end if;
  end process KEYPAD_STATE_MACHINE;
  ------------------------------------------------------------------------------

  ------------------------------------------------------------------------------
  -- Process Name     : KEYPAD_TO_BINARY
  -- Sensitivity List : I_CLK_100MHZ    : 100 MHz global clock
  --                    I_SYSTEM_RST    : Global Reset line
  -- Useful Outputs   : s_keypad_binary : 4 bit binary representation of keypad
  --                                      state (output of entity).
  -- Description      : Entity to control the powering and reading of the
  --                    keypad rows and columns based on the current s_keypad_state.
  --                    Outputs the current binary number of keypad (0-15)
  ------------------------------------------------------------------------------
  KEYPAD_TO_BINARY: process (I_CLK_100MHZ, I_SYSTEM_RST)
  begin
    if (I_SYSTEM_RST = '1') then
      s_keypad_col_1      <= '0';
      s_keypad_col_2      <= '0';
      s_keypad_col_3      <= '0';
      s_keypad_col_4      <= '0';
      s_keypad_binary     <= (others => '0');

    elsif ((rising_edge(I_CLK_100MHZ))) then

      -- Power the Column 1
      if (s_keypad_state = COL1_POWER_STATE) then
        s_keypad_col_1    <= '1';
        s_keypad_col_2    <= '0';
        s_keypad_col_3    <= '0';
        s_keypad_col_4    <= '0';

      -- Power the Column 2
      elsif (s_keypad_state = COL2_POWER_STATE) then
        s_keypad_col_1    <= '0';
        s_keypad_col_2    <= '1';
        s_keypad_col_3    <= '0';
        s_keypad_col_4    <= '0';

      -- Power the Column 3
      elsif (s_keypad_state = COL3_POWER_STATE) then
        s_keypad_col_1    <= '0';
        s_keypad_col_2    <= '0';
        s_keypad_col_3    <= '1';
        s_keypad_col_4    <= '0';

      -- Power the Column 4
      elsif (s_keypad_state = COL4_POWER_STATE) then
        s_keypad_col_1    <= '0';
        s_keypad_col_2    <= '0';
        s_keypad_col_3    <= '0';
        s_keypad_col_4    <= '1';

      else
        s_keypad_col_1    <= s_keypad_col_1;
        s_keypad_col_2    <= s_keypad_col_2;
        s_keypad_col_3    <= s_keypad_col_3;
        s_keypad_col_4    <= s_keypad_col_4;
      end if;

      -- Col 1
      if (s_keypad_state = COL1_READ_STATE) then
        if    (I_KEYPAD_ROW_1 = '1') then
          s_keypad_binary <= "0001";
        elsif (I_KEYPAD_ROW_2 = '1') then
          s_keypad_binary <= "0100";
        elsif (I_KEYPAD_ROW_3 = '1') then
          s_keypad_binary <= "0111";
        elsif (I_KEYPAD_ROW_4 = '1') then
          s_keypad_binary <= "1110";
        else
          s_keypad_binary <= s_keypad_binary;
        end if;

      -- Col 2
      elsif (s_keypad_state = COL2_READ_STATE) then
        if    (I_KEYPAD_ROW_1 = '1') then
          s_keypad_binary <= "0010";
        elsif (I_KEYPAD_ROW_2 = '1') then
          s_keypad_binary <= "0101";
        elsif (I_KEYPAD_ROW_3 = '1') then
          s_keypad_binary <= "1000";
        elsif (I_KEYPAD_ROW_4 = '1') then
          s_keypad_binary <= "0000";
        else
          s_keypad_binary <= s_keypad_binary;
        end if;

      -- Col 3
      elsif (s_keypad_state = COL3_READ_STATE) then
        if    (I_KEYPAD_ROW_1 = '1') then
          s_keypad_binary <= "0011";
        elsif (I_KEYPAD_ROW_2 = '1') then
          s_keypad_binary <= "0110";
        elsif (I_KEYPAD_ROW_3 = '1') then
          s_keypad_binary <= "1001";
        elsif (I_KEYPAD_ROW_4 = '1') then
          s_keypad_binary <= "1111";
        else
          s_keypad_binary <= s_keypad_binary;
        end if;

      -- Col 4
      elsif (s_keypad_state = COL4_READ_STATE) then
        if    (I_KEYPAD_ROW_1 = '1') then
          s_keypad_binary <= "1010";
        elsif (I_KEYPAD_ROW_2 = '1') then
          s_keypad_binary <= "1011";
        elsif (I_KEYPAD_ROW_3 = '1') then
          s_keypad_binary <= "1100";
        elsif (I_KEYPAD_ROW_4 = '1') then
          s_keypad_binary <= "1101";
        else
          s_keypad_binary <= s_keypad_binary;
        end if;

      else
        s_keypad_binary   <= s_keypad_binary;
      end if;
    end if;
  end process KEYPAD_TO_BINARY;
  O_KEYPAD_COL_1          <= s_keypad_col_1;
  O_KEYPAD_COL_2          <= s_keypad_col_2;
  O_KEYPAD_COL_3          <= s_keypad_col_3;
  O_KEYPAD_COL_4          <= s_keypad_col_4;
  O_KEYPAD_BINARY         <= s_keypad_binary;
  ------------------------------------------------------------------------------
end architecture rtl;

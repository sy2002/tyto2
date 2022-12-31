--------------------------------------------------------------------------------
-- hdmi_tpg_mega65r3.vhd                                                      --
-- Board specific top level wrapper for the hdmi_tpg design.                  --
--------------------------------------------------------------------------------
-- (C) Copyright 2022 Adam Barnes <ambarnes@gmail.com>                        --
-- This file is part of The Tyto Project. The Tyto Project is free software:  --
-- you can redistribute it and/or modify it under the terms of the GNU Lesser --
-- General Public License as published by the Free Software Foundation,       --
-- either version 3 of the License, or (at your option) any later version.    --
-- The Tyto Project is distributed in the hope that it will be useful, but    --
-- WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public     --
-- License for more details. You should have received a copy of the GNU       --
-- Lesser General Public License along with The Tyto Project. If not, see     --
-- https://www.gnu.org/licenses/.                                             --
--------------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;

library work;
  use work.hdmi_tpg_pkg.all;
  use work.sync_reg_pkg.all;

entity hdmi_tpg_mega65r3 is
  port (

    clk_in                  : in    std_logic;                      -- clock in (100MHz)

    max10_clk               : inout std_logic;                      -- MAX10 CPLD
    max10_tx                : in    std_logic;
    max10_rx                : out   std_logic;

    uart_rx                 : in    std_logic;                      -- debug UART
    uart_tx                 : out   std_logic;

    kb_io0                  : out   std_logic;                      -- keyboard
    kb_io1                  : out   std_logic;
    kb_io2                  : in    std_logic;
    kb_jtagen               : out   std_logic;
    kb_tck                  : out   std_logic;
    kb_tms                  : out   std_logic;
    kb_tdi                  : out   std_logic;
    kb_tdo                  : in    std_logic;

    jsa_left_n              : in    std_logic;                      -- joysticks/paddles
    jsa_right_n             : in    std_logic;
    jsa_up_n                : in    std_logic;
    jsa_down_n              : in    std_logic;
    jsa_fire_n              : in    std_logic;
    jsb_left_n              : in    std_logic;
    jsb_right_n             : in    std_logic;
    jsb_up_n                : in    std_logic;
    jsb_down_n              : in    std_logic;
    jsb_fire_n              : in    std_logic;
    paddle                  : in    std_logic_vector(3 downto 0);
    paddle_drain            : out   std_logic;

    i2c_sda                 : inout std_logic;                      -- on-board I2C bus
    i2c_scl                 : inout std_logic;

    grove_sda               : inout std_logic;                      -- Grove connector
    grove_scl               : inout std_logic;

    sd_cd_n                 : in    std_logic;                      -- SD/MMC card
    sd_wp_n                 : in    std_logic;
    sd_ss_n                 : out   std_logic;
    sd_clk                  : out   std_logic;
    sd_mosi                 : out   std_logic;
    sd_miso                 : in    std_logic;

    sd2_cd_n                : in    std_logic;                      -- micro SD card
    sd2_ss_n                : out   std_logic;
    sd2_clk                 : out   std_logic;
    sd2_mosi                : out   std_logic;
    sd2_miso                : in    std_logic;

    vga_vdac_clk            : out   std_logic;                      -- VGA VDAC
    vga_vdac_sync_n         : out   std_logic;
    vga_vdac_blank_n        : out   std_logic;
    vga_vsync               : out   std_logic;                      -- VGA out
    vga_hsync               : out   std_logic;
    vga_r                   : out   std_logic_vector (7 downto 0);
    vga_g                   : out   std_logic_vector (7 downto 0);
    vga_b                   : out   std_logic_vector (7 downto 0);

    hdmi_clk_p              : out   std_logic;                      -- HDMI out
    hdmi_clk_n              : out   std_logic;
    hdmi_data_p             : out   std_logic_vector(2 downto 0);
    hdmi_data_n             : out   std_logic_vector(2 downto 0);
    hdmi_ct_hpd             : out   std_logic;
    hdmi_hpd                : inout std_logic;
    hdmi_ls_oe              : out   std_logic;
    hdmi_cec                : inout std_logic;
    hdmi_scl                : inout std_logic;
    hdmi_sda                : inout std_logic;

    pwm_l                   : out   std_logic;                      -- PWM audio out
    pwm_r                   : out   std_logic;

    i2s_sd_n                : out   std_logic;                      -- I2S audio out
    i2s_mclk                : out   std_logic;
    i2s_bclk                : out   std_logic;
    i2s_sync                : out   std_logic;
    i2s_sdata               : out   std_logic;

    hr_rst_n                : out   std_logic;                      -- HyperRAM
    hr_clk_p                : out   std_logic;
    hr_cs_n                 : out   std_logic;
    hr_rwds                 : inout std_logic;
    hr_d                    : inout std_logic_vector(7 downto 0);

--  hrx_rst_n               : out   std_logic;                      -- optional extended HyperRAM on PMOD
--  hrx_clk_p               : out   std_logic;
--  hrx_cs_n                : out   std_logic;
--  hrx_rwds                : inout std_logic;
--  hrx_d                   : inout std_logic_vector(7 downto 0);

    fdd_density             : out   std_logic;                      -- FDD
    fdd_motora              : out   std_logic;
    fdd_motorb              : out   std_logic;
    fdd_selecta             : out   std_logic;
    fdd_selectb             : out   std_logic;
    fdd_stepdir             : out   std_logic;
    fdd_step                : out   std_logic;
    fdd_wdata               : out   std_logic;
    fdd_wgate               : out   std_logic;
    fdd_side1               : out   std_logic;
    fdd_index               : in    std_logic;
    fdd_track0              : in    std_logic;
    fdd_writeprotect        : in    std_logic;
    fdd_rdata               : in    std_logic;
    fdd_diskchanged         : in    std_logic;

    iec_rst_n               : out   std_logic;                      -- CBM-488/IEC serial port
    iec_atn_n               : out   std_logic;
    iec_srq_n_en            : out   std_logic;
    iec_srq_n_o             : out   std_logic;
    iec_srq_n_i             : in    std_logic;
    iec_clk_en              : out   std_logic;
    iec_clk_o               : out   std_logic;
    iec_clk_i               : in    std_logic;
    iec_data_en             : out   std_logic;
    iec_data_o              : out   std_logic;
    iec_data_i              : in    std_logic;

    eth_rst_n               : out   std_logic;                      -- ethernet PHY (RMII)
    eth_clk                 : out   std_logic;
    eth_txen                : out   std_logic;
    eth_txd                 : out   std_logic_vector(1 downto 0);
    eth_rxdv                : in    std_logic;
    eth_rxd                 : in    std_logic_vector(1 downto 0);
    eth_rxer                : in    std_logic;
    eth_mdc                 : out   std_logic;
    eth_mdio                : inout std_logic;
    eth_led_n               : inout std_logic_vector(1 to 1);

    cart_ctrl_oe_n          : out   std_logic;                      -- C64 cartridge
    cart_ctrl_dir           : out   std_logic;
    cart_addr_oe_n          : out   std_logic;
    cart_laddr_dir          : out   std_logic;
    cart_haddr_dir          : out   std_logic;
    cart_data_oe_n          : out   std_logic;
    cart_data_dir           : out   std_logic;
    cart_phi2               : out   std_logic;
    cart_dotclk             : out   std_logic;
    cart_rst_n              : in    std_logic;
    cart_nmi_n              : in    std_logic;
    cart_irq_n              : in    std_logic;
    cart_dma_n              : in    std_logic;
    cart_exrom_n            : inout std_logic;
    cart_ba                 : inout std_logic;
    cart_r_w                : inout std_logic;
    cart_roml_n             : inout std_logic;
    cart_romh_n             : inout std_logic;
    cart_game_n             : inout std_logic;
    cart_io1_n              : inout std_logic;
    cart_io2_n              : inout std_logic;
    cart_a                  : inout std_logic_vector(15 downto 0);
    cart_d                  : inout std_logic_vector(7 downto 0);

    p1lo                    : inout std_logic_vector(3 downto 0);   -- PMODs
    p1hi                    : inout std_logic_vector(3 downto 0);
    p2lo                    : inout std_logic_vector(3 downto 0);
    p2hi                    : inout std_logic_vector(3 downto 0);

    qspi_cs_n               : out   std_logic;                      -- QSPI flash
    qspi_d                  : inout std_logic_vector(3 downto 0);

    tp                      : inout std_logic_vector(8 downto 1)    -- testpoints

  );
end entity hdmi_tpg_mega65r3;

architecture synth of hdmi_tpg_mega65r3 is

  constant MEGA65_CLOCK_FREQ_HZ : natural := 100_000_000;

  signal mode_step : std_logic;
  signal mode      : std_logic_vector(3 downto 0);
  signal dvi       : std_logic;
  signal heartbeat : std_logic_vector(3 downto 0);
  signal status    : std_logic_vector(1 downto 0);
  
  -- MEGA65 specific signals
  signal floppyled    : std_logic;
  signal key_return_n : std_logic;
  
  signal vga_clk      : std_logic;
  signal vga_vs       : std_logic;
  signal vga_hs       : std_logic;
  signal vga_de       : std_logic;
  signal vga_red      : std_logic_vector(7 downto 0);
  signal vga_green    : std_logic_vector(7 downto 0);
  signal vga_blue     : std_logic_vector(7 downto 0);  

begin

  mode_step <= not key_return_n;
  dvi       <= '0';
  floppyled <= heartbeat(0) and status(0) and status(1);

  MAIN: component hdmi_tpg
    generic map (
      fclk       => Real(MEGA65_CLOCK_FREQ_HZ / 1_000_000)
    )
    port map (
      rst        => not jsb_fire_n,
      clk        => clk_in,
      mode_step  => mode_step,
      mode       => mode,
      dvi        => dvi,
      heartbeat  => heartbeat,
      status     => status,
      
      -- HDMI out
      hdmi_clk_p => hdmi_clk_p,
      hdmi_clk_n => hdmi_clk_n,
      hdmi_d_p   => hdmi_data_p,
      hdmi_d_n   => hdmi_data_n,
      
      -- VGA out
      vga_clk_o  => vga_clk,
      vga_vs_o   => vga_vs,
      vga_hs_o   => vga_hs,
      vga_de_o   => vga_de,
      vga_r_o    => vga_red,
      vga_g_o    => vga_green,
      vga_b_o    => vga_blue
    );

   M65_KEYB: entity work.keyboard
    generic map (
      CLOCK_FREQ_HZ  => MEGA65_CLOCK_FREQ_HZ
    )
    port map (
      cpuclock    => clk_in,
      flopled     => floppyled,
      powerled    => '1',
      kio8        => kb_io0,        -- clock to keyboard
      kio9        => kb_io1,        -- data output to keyboard
      kio10       => kb_io2,        -- data input from keyboard
      delete_out  => open,
      return_out  => key_return_n,
      fastkey_out => open          
    );
    
  -- Make the MEGA65 VDAC (ADV7125BCPZ170) output the image:
  --
  -- Excerpts taken from the data sheet Rev D, page 8, table 6:
  --    "sync":  If sync information is not required on the green channel, the SYNC input should be tied to Logic 0.
  --    "blank": A Logic 0 on this control input drives the analog outputs [...] to the blanking level.
  -- So as we do not do any "sync on green", we set sync to 0 and since we do not want to use the VDAC to
  -- blank the screen (i.e. set the analog R, G and B to 0), we hard-wire blank to 1.
  vga_vdac_sync_n   <= '0';
  vga_vdac_blank_n  <= '1';
  vga_vdac_clk      <= not vga_clk; -- phase shifted so that the VDAC can sample the stabilized signals
  
  -- The MEGA65 VDAC (ADV7125BCPZ170) does not like non-zero color values outside the visible window.
  -- This is why we explicitly set R, G, B to zero outside of "data enable".
  vga_vsync   <= vga_vs;
  vga_hsync   <= vga_hs;
  vga_data_enable : process(all)
  begin
     if vga_de = '1' then
        vga_r <= vga_red;
        vga_g <= vga_green;
        vga_b <= vga_blue;
     else
        vga_r <= (others => '0');
        vga_g <= (others => '0');
        vga_b <= (others => '0');
     end if;
  end process vga_data_enable;      
    
  -- safe states for unused I/Os
  max10_clk      <= 'Z'; -- assumed
  max10_rx       <= '1';
  uart_tx        <= '1';
--kb_io0         <= '0'; -- assumed
--kb_io1         <= '0'; -- assumed
  kb_jtagen      <= '0'; -- assumed
  paddle_drain   <= '0';
  i2c_sda        <= 'Z';
  i2c_scl        <= 'Z';
  grove_sda      <= 'Z';
  grove_scl      <= 'Z';
  sd_ss_n        <= '1';
  sd_clk         <= '0';
  sd_mosi        <= '0';
  sd2_ss_n       <= '1';
  sd2_clk        <= '0';
  sd2_mosi       <= '0';
--vga_vdac_clk   <= '0';
--vga_vdac_sync_n    <= '0';
--vga_vdac_blank_n   <= '1';
--vga_vsync      <= '0';
--vga_hsync      <= '0';
--vga_r          <= (others => '0');
--vga_g          <= (others => '0');
--vga_b          <= (others => '0');
--hdmi_clk_p     <= (others => '0');
--hdmi_clk_n     <= (others => '1');
--hdmi_data_p    <= (others => '0');
--hdmi_data_n    <= (others => '1');
  hdmi_ct_hpd    <= '1';
  hdmi_hpd       <= 'Z';
  hdmi_ls_oe     <= '1';
  hdmi_cec       <= 'Z';
  hdmi_scl       <= 'Z';
  hdmi_sda       <= 'Z';
  pwm_l          <= '0';
  pwm_r          <= '0';
  i2s_sd_n       <= '0';
  i2s_mclk       <= '0';
  i2s_bclk       <= '1';
  i2s_sync       <= '0';
  i2s_sdata      <= '0';
  hr_rst_n       <= '0';
  hr_clk_p       <= '0';
  hr_cs_n        <= '1';
  hr_rwds        <= 'Z';
  hr_d           <= (others => 'Z');
  fdd_density    <= '1';
  fdd_motora     <= '1';
  fdd_motorb     <= '1';
  fdd_selecta    <= '1';
  fdd_selectb    <= '1';
  fdd_stepdir    <= '1';
  fdd_step       <= '1';
  fdd_wdata      <= '1';
  fdd_wgate      <= '1';
  fdd_side1      <= '1';
  iec_rst_n      <= '0';
  iec_atn_n      <= '1';
  iec_srq_n_en   <= '0';
  iec_srq_n_o    <= '0';
  iec_clk_en     <= '0';
  iec_clk_o      <= '0';
  iec_data_en    <= '0';
  iec_data_o     <= '0';
  eth_rst_n      <= '0';
  eth_clk        <= '0';
  eth_txen       <= '0';
  eth_txd        <= "00";
  eth_mdc        <= '0';
  eth_mdio       <= 'Z';
  eth_led_n      <= "0"; -- on
  cart_ctrl_oe_n <= '1';
  cart_ctrl_dir  <= '1';
  cart_addr_oe_n <= '1';
  cart_laddr_dir <= '1';
  cart_haddr_dir <= '1';
  cart_data_oe_n <= '1';
  cart_data_dir  <= '1';
  cart_phi2      <= '0';
  cart_dotclk    <= '0';
  cart_exrom_n   <= 'Z';
  cart_ba        <= 'Z';
  cart_r_w       <= 'Z';
  cart_roml_n    <= 'Z';
  cart_romh_n    <= 'Z';
  cart_game_n    <= 'Z';
  cart_io1_n     <= 'Z';
  cart_io2_n     <= 'Z';
  cart_a         <= (others => 'Z');
  cart_d         <= (others => 'Z');
  p1lo           <= (others => 'Z');
  p1hi           <= (others => 'Z');
  p2lo           <= (others => 'Z');
  p2hi           <= (others => 'Z');
  qspi_cs_n      <= '1';
  qspi_d         <= (others => '0');
  tp             <= (others => '0');

end architecture synth;

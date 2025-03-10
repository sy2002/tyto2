--------------------------------------------------------------------------------
-- hdmi_tpg_digilent_nexys_video.vhd                                          --
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

entity hdmi_tpg_digilent_nexys_video is
  port (

    -- clocks
    clki_100m     : in    std_logic;
    -- gtp_clk_p       : in    std_logic;
    -- gtp_clk_n       : in    std_logic;
    -- fmc_mgt_clk_p   : in    std_logic;
    -- fmc_mgt_clk_n   : in    std_logic;

    -- LEDs, buttons and switches
    led           : out   std_logic_vector(7 downto 0);
    btn_c         : in    std_logic;
    -- btn_d           : in    std_logic;
    -- btn_l           : in    std_logic;
    -- btn_r           : in    std_logic;
    -- btn_u           : in    std_logic;
    btn_rst_n     : in    std_logic;
    sw            : in    std_logic_vector(7 downto 0);

    -- OLED
    oled_res_n    : out   std_logic;
    oled_d_c      : out   std_logic;
    oled_sclk     : out   std_logic;
    oled_sdin     : out   std_logic;
    -- oled_vbat_dis   : out   std_logic;
    -- oled_vdd_dis    : out   std_logic;

    -- HDMI RX
    -- hdmi_rx_clk_p   : in    std_logic;
    -- hdmi_rx_clk_n   : in    std_logic;
    -- hdmi_rx_d_p     : in    std_logic_vector(0 to 2);
    -- hdmi_rx_d_n     : in    std_logic_vector(0 to 2);
    -- hdmi_rx_scl     : in    std_logic;
    -- hdmi_rx_sda     : inout std_logic;
    -- hdmi_rx_cec     : in    std_logic;
    -- hdmi_rx_hpd     : out   std_logic;
    -- hdmi_rx_txen    : out   std_logic;

    -- HDMI TX
    hdmi_tx_clk_p : out   std_logic;
    hdmi_tx_clk_n : out   std_logic;
    hdmi_tx_d_p   : out   std_logic_vector(0 to 2);
    hdmi_tx_d_n   : out   std_logic_vector(0 to 2);
    -- hdmi_tx_scl     : out   std_logic;
    -- hdmi_tx_sda     : inout std_logic;
    -- hdmi_tx_cec     : out   std_logic;
    -- hdmi_tx_hpd     : in    std_logic;

    -- DisplayPort
    -- dp_tx_p         : out   std_logic_vector(0 to 1);
    -- dp_tx_n         : out   std_logic_vector(0 to 1);
    -- dp_tx_aux_p     : inout std_logic;
    -- dp_tx_aux_n     : inout std_logic;
    -- dp_tx_aux2_p    : inout std_logic;
    -- dp_tx_aux2_n    : inout std_logic;
    -- dp_tx_hpd       : in    std_logic;

    -- audio codec
    ac_mclk       : out   std_logic;
    -- ac_lrclk        : out   std_logic;
    -- ac_bclk         : out   std_logic;
    ac_dac_sdata  : out   std_logic;
    -- ac_adc_sdata    : in    std_logic;

    -- PMODs
    -- ja              : inout std_logic_vector(7 downto 0);
    -- jb              : inout std_logic_vector(7 downto 0);
    -- jc              : inout std_logic_vector(7 downto 0);
    -- xa_p            : inout std_logic_vector(3 downto 0);
    -- xa_n            : inout std_logic_vector(3 downto 0);

    -- UART
    uart_rx_out   : out   std_logic;
    -- uart_tx_in      : in    std_logic;

    -- ethernet
    eth_rst_n     : out   std_logic;
    -- eth_txck        : out   std_logic;
    -- eth_txctl       : out   std_logic;
    -- eth_txd         : out   std_logic_vector(3 downto 0);
    -- eth_rxck        : in    std_logic;
    -- eth_rxctl       : in    std_logic;
    -- eth_rxd         : in    std_logic_vector(3 downto 0);
    -- eth_mdc         : out   std_logic;
    -- eth_mdio        : inout std_logic;
    -- eth_int_n       : in    std_logic;
    -- eth_pme_n       : in    std_logic;

    -- fan
    -- fan_pwm         : out   std_logic;

    -- FTDI
    -- ftdi_clko       : in    std_logic;
    -- ftdi_rxf_n      : in    std_logic;
    -- ftdi_txe_n      : in    std_logic;
    ftdi_rd_n     : out   std_logic;
    ftdi_wr_n     : out   std_logic;
    ftdi_siwu_n   : out   std_logic;
    ftdi_oe_n     : out   std_logic;
    -- ftdi_d          : inout std_logic_vector(7 downto 0);
    -- ftdi_spien      : out   std_logic;

    -- PS/2
    ps2_clk       : inout std_logic;
    ps2_data      : inout std_logic;

    -- QSPI
    qspi_cs_n     : out   std_logic;
    -- qspi_dq         : inout std_logic_vector(3 downto 0);

    -- SD
    -- sd_reset        : out   std_logic;
    -- sd_cclk         : out   std_logic;
    -- sd_cmd          : out   std_logic;
    -- sd_d            : inout std_logic_vector(3 downto 0);
    -- sd_cd           : in    std_logic;

    -- I2C
    -- i2c_scl         : inout std_logic;
    -- i2c_sda         : inout std_logic;

    -- VADJ
    -- set_vadj        : out   std_logic_vector(1 downto 0);
    -- vadj_en         : out   std_logic;

    -- FMC
    -- fmc_clk0_m2c_p  : in    std_logic;
    -- fmc_clk0_m2c_n  : in    std_logic;
    -- fmc_clk1_m2c_p  : in    std_logic;
    -- fmc_clk1_m2c_n  : in    std_logic;
    -- fmc_la_p        : inout std_logic_vector(33 downto 0);
    -- fmc_la_n        : inout std_logic_vector(33 downto 0);

    -- DDR3
    ddr3_reset_n  : out   std_logic
  -- ddr3_ck_p       : out   std_logic_vector(0 downto 0);
  -- ddr3_ck_n       : out   std_logic_vector(0 downto 0);
  -- ddr3_cke        : out   std_logic_vector(0 downto 0);
  -- ddr3_ras_n      : out   std_logic;
  -- ddr3_cas_n      : out   std_logic;
  -- ddr3_we_n       : out   std_logic;
  -- ddr3_odt        : out   std_logic_vector(0 downto 0);
  -- ddr3_addr       : out   std_logic_vector(14 downto 0);
  -- ddr3_ba         : out   std_logic_vector(2 downto 0);
  -- ddr3_dm         : out   std_logic_vector(1 downto 0);
  -- ddr3_dq         : inout std_logic_vector(15 downto 0);
  -- ddr3_dqs_p      : inout std_logic_vector(1 downto 0);
  -- ddr3_dqs_n      : inout std_logic_vector(1 downto 0)

  );
end entity hdmi_tpg_digilent_nexys_video;

architecture synth of hdmi_tpg_digilent_nexys_video is

  signal rst_100m  : std_logic;
  signal mode_step : std_logic;
  signal mode      : std_logic_vector(3 downto 0);
  signal dvi       : std_logic;
  signal heartbeat : std_logic_vector(3 downto 0);
  signal status    : std_logic_vector(1 downto 0);

begin

  -- user interface:
  -- button CPU_RESET = reset
  -- button BTNC = press to increment video mode (0..14 then wrap)
  -- switch SW0 = HDMI/DVI mode
  -- led LD7 = DVI mode
  -- led LD6 = audio clock MMCM lock
  -- led LD5 = pixel clock MMCM lock
  -- led LD4 = heartbeat
  -- leds LD3..LD0 = display mode (binary, 0000..1110)

  RST: component sync_reg
    generic map (
      width => 1,
      depth => 2
    )
    port map (
      clk   => clki_100m,
      d(0)  => not btn_rst_n,
      q(0)  => rst_100m
    );

  mode_step       <= btn_c;
  dvi             <= sw(0);
  led(7)          <= dvi;
  led(6)          <= status(1);
  led(5)          <= status(0);
  led(4)          <= heartbeat(0);
  led(3 downto 0) <= mode;

  MAIN: component hdmi_tpg
    generic map (
      fclk       => 100.0 -- 100MHz
    )
    port map (
      rst        => not btn_rst_n,
      clk        => clki_100m,
      mode_step  => mode_step,
      mode       => mode,
      dvi        => dvi,
      heartbeat  => heartbeat,
      status     => status,
      hdmi_clk_p => hdmi_tx_clk_p,
      hdmi_clk_n => hdmi_tx_clk_n,
      hdmi_d_p   => hdmi_tx_d_p,
      hdmi_d_n   => hdmi_tx_d_n
    );

  -- unused I/Os

  oled_res_n   <= '0';
  oled_d_c     <= '0';
  oled_sclk    <= '0';
  oled_sdin    <= '0';
  ac_mclk      <= '0';
  ac_dac_sdata <= '0';
  uart_rx_out  <= '1';
  eth_rst_n    <= '0';
  ftdi_rd_n    <= '1';
  ftdi_wr_n    <= '1';
  ftdi_siwu_n  <= '1';
  ftdi_oe_n    <= '1';
  ps2_clk      <= 'Z';
  ps2_data     <= 'Z';
  qspi_cs_n    <= '1';
  ddr3_reset_n <= '0';

end architecture synth;

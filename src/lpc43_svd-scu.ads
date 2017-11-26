--
--  Copyright (C) 2017, AdaCore
--

--  This spec has been automatically generated from LPC43xx_43Sxx.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with System;

package LPC43_SVD.SCU is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Select pin function.
   type MODE_ENUM is
     (
      --  Function 0 (default)
      Function_0_Default,
      --  Function 1
      Function_1,
      --  Function 2
      Function_2,
      --  Function 3
      Function_3,
      --  Function 4
      Function_4,
      --  Function 5
      Function_5,
      --  Function 6
      Function_6,
      --  Function 7
      Function_7)
     with Size => 3;
   for MODE_ENUM use
     (Function_0_Default => 0,
      Function_1 => 1,
      Function_2 => 2,
      Function_3 => 3,
      Function_4 => 4,
      Function_5 => 5,
      Function_6 => 6,
      Function_7 => 7);

   --  Enable pull-down resistor at pad.
   type EPD_ENUM is
     (
      --  Disable pull-down.
      Disable_Pull_Down,
      --  Enable pull-down.Enable both pull-down resistor and pull-up resistor
      --  for repeater mode.
      Enable_Pull_Down)
     with Size => 1;
   for EPD_ENUM use
     (Disable_Pull_Down => 0,
      Enable_Pull_Down => 1);

   --  Disable pull-up resistor at pad. By default, the pull-up resistor is
   --  enabled at reset.
   type EPUN_ENUM is
     (
      --  Enable pull-up. Enable both pull-down resistor and pull-up resistor
      --  for repeater mode.
      Enable_Pull_Up,
      --  Disable pull-up.
      Disable_Pull_Up)
     with Size => 1;
   for EPUN_ENUM use
     (Enable_Pull_Up => 0,
      Disable_Pull_Up => 1);

   --  Select Slew rate.
   type EHS_ENUM is
     (
      --  Slow (low noise with medium speed)
      Slow_Low_Noise_With,
      --  Fast (medium noise with fast speed)
      Fast_Medium_Noise_W)
     with Size => 1;
   for EHS_ENUM use
     (Slow_Low_Noise_With => 0,
      Fast_Medium_Noise_W => 1);

   --  Input buffer enable. The input buffer is disabled by default at reset
   --  and must be enabled for receiving.
   type EZI_ENUM is
     (
      --  Disable input buffer
      Disable_Input_Buffer,
      --  Enable input buffer
      Enable_Input_Buffer)
     with Size => 1;
   for EZI_ENUM use
     (Disable_Input_Buffer => 0,
      Enable_Input_Buffer => 1);

   --  Input glitch filter. Disable the input glitch filter for clocking
   --  signals higher than 30 MHz.
   type ZIF_ENUM is
     (
      --  Enable input glitch filter
      Enable_Input_Glitch,
      --  Disable input glitch filter
      Disable_Input_Glitch)
     with Size => 1;
   for ZIF_ENUM use
     (Enable_Input_Glitch => 0,
      Disable_Input_Glitch => 1);

   --  Select drive strength.
   type EHD_ENUM is
     (
      --  Normal-drive: 4 mA drive strength
      Normal_Drive_4_Ma_D,
      --  Medium-drive: 8 mA drive strength
      Medium_Drive_8_Ma_D,
      --  High-drive: 14 mA drive strength
      High_Drive_14_Ma_Dr,
      --  Ultra high-drive: 20 mA drive strength
      Ultra_High_Drive_20)
     with Size => 2;
   for EHD_ENUM use
     (Normal_Drive_4_Ma_D => 0,
      Medium_Drive_8_Ma_D => 1,
      High_Drive_14_Ma_Dr => 2,
      Ultra_High_Drive_20 => 3);

   type Pin_Type is (Normal_Drive, High_Drive, High_Speed);

   --  Pin configuration register
   type SFS_Register(Pin : Pin_Type := Normal_Drive) is record

      --  Select pin function.
      MODE     : MODE_ENUM := LPC43_SVD.SCU.Function_0_Default;

      --  Enable pull-down resistor at pad.
      EPD      : EPD_ENUM := LPC43_SVD.SCU.Disable_Pull_Down;

      --  Disable pull-up resistor at pad. By default, the pull-up resistor is
      --  enabled at reset.
      EPUN     : EPUN_ENUM := LPC43_SVD.SCU.Enable_Pull_Up;

      --  Input buffer enable. The input buffer is disabled by default
      --  at reset and must be enabled for receiving.
      EZI      : EZI_ENUM := LPC43_SVD.SCU.Disable_Input_Buffer;

      --  Input glitch filter. Disable the input glitch filter for
      --  clocking signals higher than 30 MHz.
      ZIF      : ZIF_ENUM := LPC43_SVD.SCU.Enable_Input_Glitch;

      --  Reserved
      RESERVED     : LPC43_SVD.UInt22 := 16#0#;

      case Pin is
         when Normal_Drive =>

            --  Reserved
            ND_EHS_RESERVED : LPC43_SVD.Bit := 16#0#;
            --  Reserved
            ND_EHD_RESERVED : LPC43_SVD.UInt2 := 16#0#;

         when High_Drive =>

            --  Select drive strength.
            EHD : EHD_ENUM := LPC43_SVD.SCU.Normal_Drive_4_Ma_D;
            --  Reserved
            HD_EHS_RESERVED : LPC43_SVD.Bit := 16#0#;

         when High_Speed =>

            --  Select Slew rate.
            EHS    : EHS_ENUM := LPC43_SVD.SCU.Slow_Low_Noise_With;
            --  Reserved
            HS_EHD_RESERVED : LPC43_SVD.UInt2 := 16#0#;

      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for SFS_Register use record
      MODE            at 0 range 0 .. 2;
      EPD             at 0 range 3 .. 3;
      EPUN            at 0 range 4 .. 4;
      ND_EHS_RESERVED at 0 range 5 .. 5;
      HD_EHS_RESERVED at 0 range 5 .. 5;
      EHS             at 0 range 5 .. 5;
      EZI             at 0 range 6 .. 6;
      ZIF             at 0 range 7 .. 7;
      ND_EHD_RESERVED at 0 range 8 .. 9;
      EHD             at 0 range 8 .. 9;
      HS_EHD_RESERVED at 0 range 8 .. 9;
      RESERVED        at 0 range 10 .. 31;
   end record;

   type SFS_Register_Access is access all SFS_Register;

   type Normal_Drive_Pins is array (Integer range <>)
     of aliased SFS_Register(Normal_Drive);

   type High_Drive_Pins is array (Integer range <>)
     of aliased SFS_Register(High_Drive);

   type High_Speed_Pins is array (Integer range <>)
     of aliased SFS_Register(High_Speed);

   --  Pin configuration register for pins P0
   type SFSP0_Registers is array (0 .. 1) of aliased SFS_Register(Normal_Drive)
     with Volatile;

   --  Pin configuration register for pins P1
   type SFSP1_Registers is record
      Pins_0_16 : Normal_Drive_Pins (0 .. 16);
      Pin17 : High_Drive_Pins (17 .. 17);
      Pins_18_20 : Normal_Drive_Pins (18 .. 20);
   end record
     with Volatile;

   --  Pin configuration register for pins P2
   type SFSP2_Registers is record
      Pins_0_2 : Normal_Drive_Pins (0 .. 2);
      Pins_3_5 : High_Drive_Pins (3 .. 5);
      Pins_6_13 : Normal_Drive_Pins (6 .. 13);
   end record
     with Volatile;

   --  Pin configuration register for pins P3
   type SFSP3_Registers is record
      Pins_0_2 : Normal_Drive_Pins (0 .. 2);
      Pin3 : High_Speed_Pins (3 .. 3);
      Pins_4_8 : Normal_Drive_Pins (4 .. 8);
   end record
     with Volatile;

   --  Pin configuration register for pins P4
   type SFSP4_Registers is array (0 .. 10) of aliased SFS_Register(Normal_Drive)
     with Volatile;

   --  Pin configuration register for pins P5
   type SFSP5_Registers is array (0 .. 7) of aliased SFS_Register(Normal_Drive)
     with Volatile;

   --  Pin configuration register for pins P6
   type SFSP6_Registers is array (0 .. 12) of aliased SFS_Register(Normal_Drive)
     with Volatile;

   --  Pin configuration register for pins P7
   type SFSP7_Registers is array (0 .. 7) of aliased SFS_Register(Normal_Drive)
     with Volatile;

   --  Pin configuration register for pins P8
   type SFSP8_Registers is record
      Pins_0_2 : High_Drive_Pins (0 .. 2);
      Pins_3_8 : Normal_Drive_Pins (3 .. 8);
   end record
     with Volatile;

   --  Pin configuration register for pins P9
   type SFSP9_Registers is array (0 .. 6) of aliased SFS_Register(Normal_Drive)
     with Volatile;

   --  Pin configuration register for pins PA
   type SFSPA_Registers is record
      Pin0 : Normal_Drive_Pins (0 .. 0);
      Pins_1_3 : High_Speed_Pins (1 .. 3);
      Pin4 : Normal_Drive_Pins (4 .. 4);
   end record
     with Volatile;

   --  Pin configuration register for pins PB
   type SFSPB_Registers is array (0 .. 6) of aliased SFS_Register(Normal_Drive)
     with Volatile;

   --  Pin configuration register for pins PC
   type SFSPC_Registers is array (0 .. 14) of aliased SFS_Register(Normal_Drive)
     with Volatile;

   --  Pin configuration register for pins PD
   type SFSPD_Registers is array (0 .. 16) of aliased SFS_Register(Normal_Drive)
     with Volatile;

   --  Pin configuration register for pins PE
   type SFSPE_Registers is array (0 .. 15) of aliased SFS_Register(Normal_Drive)
     with Volatile;

   --  Pin configuration register for pins PF
   type SFSPF_Registers is array (0 .. 11) of aliased SFS_Register(Normal_Drive)
     with Volatile;

   --  Pin configuration register for pins CLK
   type SFSCLK_Registers is array (0 .. 3) of aliased SFS_Register(High_Speed)
     with Volatile;

   --  Differential data input AIP/AIM.
   type SFSUSB_ENUM is
     (
      --  Going LOW with full speed edge rate
      Going_Low_With_Full,
      --  Going HIGH with full speed edge rate
      Going_High_With_Full)
     with Size => 1;
   for SFSUSB_ENUM use
     (Going_Low_With_Full => 0,
      Going_High_With_Full => 1);

   --  Control signal for differential input or single input.
   type SFSUSB_ENUM_1 is
     (
      --  Reserved. Do not use.
      Reserved,
      --  Single input. Enables USB1. Use with the on-chip full-speed PHY.
      Single_Input)
     with Size => 1;
   for SFSUSB_ENUM_1 use
     (Reserved => 0,
      Single_Input => 1);

   --  Enable pull-down connect.
   type SFSUSB_ENUM_2 is
     (
      --  Pull-down disconnected
      Pull_Down_Disconnect,
      --  Pull-down connected
      Pull_Down_Connected)
     with Size => 1;
   for SFSUSB_ENUM_2 use
     (Pull_Down_Disconnect => 0,
      Pull_Down_Connected => 1);

   subtype SFSUSB_RESERVED_Field is LPC43_SVD.Bit;

   --  Power mode.
   type SFSUSB_ENUM_3 is
     (
      --  Power saving mode (Suspend mode)
      Power_Saving_Mode_S,
      --  Normal mode
      Normal_Mode)
     with Size => 1;
   for SFSUSB_ENUM_3 use
     (Power_Saving_Mode_S => 0,
      Normal_Mode => 1);

   --  Enable the vbus_valid signal. This signal is monitored by the USB1
   --  block. Use this bit for software de-bouncing of the VBUS sense signal or
   --  to indicate the VBUS state to the USB1 controller when the VBUS signal
   --  is present but the USB1_VBUS function is not connected in the SFSP2_5
   --  register. The setting of this bit has no effect if the USB1_VBUS
   --  function of pin P2_5 is enabled through the SFSP2_5 register.
   type SFSUSB_ENUM_4 is
     (
      --  VBUS signal LOW or inactive
      Vbus_Signal_Low_Or_I,
      --  VBUS signal HIGH or active
      Vbus_Signal_High_Or)
     with Size => 1;
   for SFSUSB_ENUM_4 use
     (Vbus_Signal_Low_Or_I => 0,
      Vbus_Signal_High_Or => 1);

   subtype SFSUSB_RESERVED_Field_1 is LPC43_SVD.UInt26;

   --  Pin configuration register for pins USB1_DM and USB1_DP
   type SFSUSB_Register is record
      --  Differential data input AIP/AIM.
      USB_AIM    : SFSUSB_ENUM := LPC43_SVD.SCU.Going_Low_With_Full;
      --  Control signal for differential input or single input.
      USB_ESEA   : SFSUSB_ENUM_1 := LPC43_SVD.SCU.Single_Input;
      --  Enable pull-down connect.
      USB_EPD    : SFSUSB_ENUM_2 :=
                    LPC43_SVD.SCU.Pull_Down_Disconnect;
      --  Reserved
      RESERVED   : SFSUSB_RESERVED_Field := 16#0#;
      --  Power mode.
      USB_EPWR   : SFSUSB_ENUM_3 :=
                    LPC43_SVD.SCU.Power_Saving_Mode_S;
      --  Enable the vbus_valid signal. This signal is monitored by the USB1
      --  block. Use this bit for software de-bouncing of the VBUS sense signal
      --  or to indicate the VBUS state to the USB1 controller when the VBUS
      --  signal is present but the USB1_VBUS function is not connected in the
      --  SFSP2_5 register. The setting of this bit has no effect if the
      --  USB1_VBUS function of pin P2_5 is enabled through the SFSP2_5
      --  register.
      USB_VBUS   : SFSUSB_ENUM_4 :=
                    LPC43_SVD.SCU.Vbus_Signal_Low_Or_I;
      --  Reserved
      RESERVED_1 : SFSUSB_RESERVED_Field_1 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SFSUSB_Register use record
      USB_AIM    at 0 range 0 .. 0;
      USB_ESEA   at 0 range 1 .. 1;
      USB_EPD    at 0 range 2 .. 2;
      RESERVED   at 0 range 3 .. 3;
      USB_EPWR   at 0 range 4 .. 4;
      USB_VBUS   at 0 range 5 .. 5;
      RESERVED_1 at 0 range 6 .. 31;
   end record;

   --  Select input glitch filter time constant for the SCL pin.
   type SFSI2C0_ENUM is
     (
      --  50 ns glitch filter
      SFSI2C0_ENUM_50_Ns_Glitch_Filter,
      --  3 ns glitch filter
      SFSI2C0_ENUM_3_Ns_Glitch_Filter)
     with Size => 1;
   for SFSI2C0_ENUM use
     (SFSI2C0_ENUM_50_Ns_Glitch_Filter => 0,
      SFSI2C0_ENUM_3_Ns_Glitch_Filter => 1);

   subtype SFSI2C0_RESERVED_Field is LPC43_SVD.Bit;

   --  Select I2C mode for the SCL pin.
   type SFSI2C0_ENUM_1 is
     (
      --  Standard/Fast mode transmit
      Standardfast_Mode,
      --  Fast-mode Plus transmit
      Fast_Mode_Plus_Trans)
     with Size => 1;
   for SFSI2C0_ENUM_1 use
     (Standardfast_Mode => 0,
      Fast_Mode_Plus_Trans => 1);

   --  Enable the input receiver for the SCL pin. Always write a 1 to this bit
   --  when using the I2C0.
   type SFSI2C0_ENUM_2 is
     (
      --  Disabled
      Disabled,
      --  Enabled
      Enabled)
     with Size => 1;
   for SFSI2C0_ENUM_2 use
     (Disabled => 0,
      Enabled => 1);

   subtype SFSI2C0_RESERVED_Field_1 is LPC43_SVD.UInt3;

   --  Enable or disable input glitch filter for the SCL pin. The filter time
   --  constant is determined by bit EFP.
   type SFSI2C0_ENUM_3 is
     (
      --  Enable input filter
      Enable_Input_Filter,
      --  Disable input filter
      Disable_Input_Filter)
     with Size => 1;
   for SFSI2C0_ENUM_3 use
     (Enable_Input_Filter => 0,
      Disable_Input_Filter => 1);

   subtype SFSI2C0_RESERVED_Field_2 is LPC43_SVD.UInt16;

   --  Pin configuration register for I2C0-bus pins
   type SFSI2C0_Register is record
      --  Select input glitch filter time constant for the SCL pin.
      SCL_EFP    : SFSI2C0_ENUM :=
                    LPC43_SVD.SCU.SFSI2C0_ENUM_50_Ns_Glitch_Filter;
      --  Reserved. Always write a 0 to this bit.
      RESERVED   : SFSI2C0_RESERVED_Field := 16#0#;
      --  Select I2C mode for the SCL pin.
      SCL_EHD    : SFSI2C0_ENUM_1 := LPC43_SVD.SCU.Standardfast_Mode;
      --  Enable the input receiver for the SCL pin. Always write a 1 to this
      --  bit when using the I2C0.
      SCL_EZI    : SFSI2C0_ENUM_2 := LPC43_SVD.SCU.Disabled;
      --  Reserved
      RESERVED_1 : SFSI2C0_RESERVED_Field_1 := 16#0#;
      --  Enable or disable input glitch filter for the SCL pin. The filter
      --  time constant is determined by bit EFP.
      SCL_ZIF    : SFSI2C0_ENUM_3 :=
                    LPC43_SVD.SCU.Enable_Input_Filter;
      --  Select input glitch filter time constant for the SDA pin.
      SDA_EFP    : SFSI2C0_ENUM :=
                    LPC43_SVD.SCU.SFSI2C0_ENUM_50_Ns_Glitch_Filter;
      --  Reserved. Always write a 0 to this bit.
      RESERVED_2 : SFSI2C0_RESERVED_Field := 16#0#;
      --  Select I2C mode for the SDA pin.
      SDA_EHD    : SFSI2C0_ENUM_1 := LPC43_SVD.SCU.Standardfast_Mode;
      --  Enable the input receiver for the SDA pin. Always write a 1 to this
      --  bit when using the I2C0.
      SDA_EZI    : SFSI2C0_ENUM_2 := LPC43_SVD.SCU.Disabled;
      --  Reserved
      RESERVED_3 : SFSI2C0_RESERVED_Field_1 := 16#0#;
      --  Enable or disable input glitch filter for the SDA pin. The filter
      --  time constant is determined by bit SDA_EFP.
      SDA_ZIF    : SFSI2C0_ENUM_3 :=
                    LPC43_SVD.SCU.Enable_Input_Filter;
      --  Reserved
      RESERVED_4 : SFSI2C0_RESERVED_Field_2 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SFSI2C0_Register use record
      SCL_EFP    at 0 range 0 .. 0;
      RESERVED   at 0 range 1 .. 1;
      SCL_EHD    at 0 range 2 .. 2;
      SCL_EZI    at 0 range 3 .. 3;
      RESERVED_1 at 0 range 4 .. 6;
      SCL_ZIF    at 0 range 7 .. 7;
      SDA_EFP    at 0 range 8 .. 8;
      RESERVED_2 at 0 range 9 .. 9;
      SDA_EHD    at 0 range 10 .. 10;
      SDA_EZI    at 0 range 11 .. 11;
      RESERVED_3 at 0 range 12 .. 14;
      SDA_ZIF    at 0 range 15 .. 15;
      RESERVED_4 at 0 range 16 .. 31;
   end record;

   --  Select ADC0_0
   type ENAIO0_ENUM is
     (
      --  Digital function selected on pin P4_3.
      Digital_Function_Sel,
      --  Analog function ADC0_0 selected on pin P4_3
      Analog_Function_Adc0)
     with Size => 1;
   for ENAIO0_ENUM use
     (Digital_Function_Sel => 0,
      Analog_Function_Adc0 => 1);

   --  ADC0 function select register
   type ENAIO0_Register is record
      --  Select ADC0_0
      ADC0_0        : ENAIO0_ENUM :=
                       LPC43_SVD.SCU.Digital_Function_Sel;
      --  Select ADC0_1
      ADC0_1        : ENAIO0_ENUM :=
                       LPC43_SVD.SCU.Digital_Function_Sel;
      --  Select ADC0_2
      ADC0_2        : ENAIO0_ENUM :=
                       LPC43_SVD.SCU.Digital_Function_Sel;
      --  Select ADC0_3
      ADC0_3        : ENAIO0_ENUM :=
                       LPC43_SVD.SCU.Digital_Function_Sel;
      --  Select ADC0_4
      ADC0_4        : ENAIO0_ENUM :=
                       LPC43_SVD.SCU.Digital_Function_Sel;
      --  Select ADC0_5
      ADC0_5        : ENAIO0_ENUM :=
                       LPC43_SVD.SCU.Digital_Function_Sel;
      --  Select ADC0_6
      ADC0_6        : ENAIO0_ENUM :=
                       LPC43_SVD.SCU.Digital_Function_Sel;
      --  unspecified
      Reserved_7_31 : LPC43_SVD.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENAIO0_Register use record
      ADC0_0        at 0 range 0 .. 0;
      ADC0_1        at 0 range 1 .. 1;
      ADC0_2        at 0 range 2 .. 2;
      ADC0_3        at 0 range 3 .. 3;
      ADC0_4        at 0 range 4 .. 4;
      ADC0_5        at 0 range 5 .. 5;
      ADC0_6        at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  Select ADC1_0
   type ENAIO1_ENUM is
     (
      --  Digital function selected on pin PC_3.
      Digital_Function_Sel,
      --  Analog function ADC1_0 selected on pin PC_3.
      Analog_Function_Adc1)
     with Size => 1;
   for ENAIO1_ENUM use
     (Digital_Function_Sel => 0,
      Analog_Function_Adc1 => 1);

   --  ADC1 function select register
   type ENAIO1_Register is record
      --  Select ADC1_0
      ADC1_0        : ENAIO1_ENUM :=
                       LPC43_SVD.SCU.Digital_Function_Sel;
      --  Select ADC1_1
      ADC1_1        : ENAIO1_ENUM :=
                       LPC43_SVD.SCU.Digital_Function_Sel;
      --  Select ADC1_2
      ADC1_2        : ENAIO1_ENUM :=
                       LPC43_SVD.SCU.Digital_Function_Sel;
      --  Select ADC1_3
      ADC1_3        : ENAIO1_ENUM :=
                       LPC43_SVD.SCU.Digital_Function_Sel;
      --  Select ADC1_4
      ADC1_4        : ENAIO1_ENUM :=
                       LPC43_SVD.SCU.Digital_Function_Sel;
      --  Select ADC1_5
      ADC1_5        : ENAIO1_ENUM :=
                       LPC43_SVD.SCU.Digital_Function_Sel;
      --  Select ADC1_6
      ADC1_6        : ENAIO1_ENUM :=
                       LPC43_SVD.SCU.Digital_Function_Sel;
      --  Select ADC1_7.
      ADC1_7        : ENAIO1_ENUM :=
                       LPC43_SVD.SCU.Digital_Function_Sel;
      --  unspecified
      Reserved_8_31 : LPC43_SVD.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENAIO1_Register use record
      ADC1_0        at 0 range 0 .. 0;
      ADC1_1        at 0 range 1 .. 1;
      ADC1_2        at 0 range 2 .. 2;
      ADC1_3        at 0 range 3 .. 3;
      ADC1_4        at 0 range 4 .. 4;
      ADC1_5        at 0 range 5 .. 5;
      ADC1_6        at 0 range 6 .. 6;
      ADC1_7        at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  Select DAC
   type ENAIO2_ENUM is
     (
      --  Digital function selected on pin P4_4.
      Digital_Function_Sel,
      --  Analog function DAC selected on pin P4_4.
      Analog_Function_Dac)
     with Size => 1;
   for ENAIO2_ENUM use
     (Digital_Function_Sel => 0,
      Analog_Function_Dac => 1);

   --  Select band gap output. To measure the band gap, disable the pull-up on
   --  pin PF_7 and connect PF_7 to the digital pad. Do not use the digital pad
   --  nor the ADC1_7 on the board when measuring the band gap (see Section
   --  15.4.8.1).
   type ENAIO2_ENUM_1 is
     (
      --  Digital function selected on pin PF_7.
      Digital_Function_Sel,
      --  Band gap output selected for pin PF_7.
      Band_Gap_Output_Sele)
     with Size => 1;
   for ENAIO2_ENUM_1 use
     (Digital_Function_Sel => 0,
      Band_Gap_Output_Sele => 1);

   --  Analog function select register
   type ENAIO2_Register is record
      --  Select DAC
      DAC           : ENAIO2_ENUM :=
                       LPC43_SVD.SCU.Digital_Function_Sel;
      --  unspecified
      Reserved_1_3  : LPC43_SVD.UInt3 := 16#0#;
      --  Select band gap output. To measure the band gap, disable the pull-up
      --  on pin PF_7 and connect PF_7 to the digital pad. Do not use the
      --  digital pad nor the ADC1_7 on the board when measuring the band gap
      --  (see Section 15.4.8.1).
      BG            : ENAIO2_ENUM_1 :=
                       LPC43_SVD.SCU.Digital_Function_Sel;
      --  unspecified
      Reserved_5_31 : LPC43_SVD.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for ENAIO2_Register use record
      DAC           at 0 range 0 .. 0;
      Reserved_1_3  at 0 range 1 .. 3;
      BG            at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype EMCDELAYCLK_CLK_DELAY_Field is LPC43_SVD.UInt16;
   subtype EMCDELAYCLK_RESERVED_Field is LPC43_SVD.UInt16;

   --  EMC clock delay register
   type EMCDELAYCLK_Register is record
      --  EMC_CLKn SDRAM clock output delay. 0x0 = no delay 0x1111
      --  approximately 0.5 ns delay 0x2222 approximately 1.0 ns delay 0x3333
      --  approximately 1.5 ns delay 0x4444 approximately 2.0 ns delay 0x5555
      --  approximately 2.5 ns delay 0x6666 approximately 3.0 ns delay 0x7777
      --  approximately 3.5 ns delay
      CLK_DELAY : EMCDELAYCLK_CLK_DELAY_Field := 16#0#;
      --  Reserved. Do not write ones to reserved register bits.
      RESERVED  : EMCDELAYCLK_RESERVED_Field := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for EMCDELAYCLK_Register use record
      CLK_DELAY at 0 range 0 .. 15;
      RESERVED  at 0 range 16 .. 31;
   end record;

   subtype SDDELAY_SAMPLE_DELAY_Field is LPC43_SVD.UInt4;
   subtype SDDELAY_RESERVED_Field is LPC43_SVD.UInt4;
   subtype SDDELAY_DRV_DELAY_Field is LPC43_SVD.UInt4;
   subtype SDDELAY_RESERVED_Field_1 is LPC43_SVD.UInt20;

   --  SD/MMC sample and drive delay register
   type SDDELAY_Register is record
      --  SD/MMC sample delay. The delay value is SAMPLE_DELAY x 0.5 ns.
      SAMPLE_DELAY : SDDELAY_SAMPLE_DELAY_Field := 16#0#;
      --  Reserved. Do not write ones to reserved register bits.
      RESERVED     : SDDELAY_RESERVED_Field := 16#0#;
      --  SD/MMC drive delay. The delay value is DRV_DELAY x 0.5 ns. The values
      --  DRV_DELAY = 0 and DRV_DELAY = 1 are not allowed.
      DRV_DELAY    : SDDELAY_DRV_DELAY_Field := 16#0#;
      --  Reserved. Do not write ones to reserved register bits.
      RESERVED_1   : SDDELAY_RESERVED_Field_1 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for SDDELAY_Register use record
      SAMPLE_DELAY at 0 range 0 .. 3;
      RESERVED     at 0 range 4 .. 7;
      DRV_DELAY    at 0 range 8 .. 11;
      RESERVED_1   at 0 range 12 .. 31;
   end record;

   subtype PINTSEL0_INTPIN0_Field is LPC43_SVD.UInt5;

   --  Pin interrupt 0: Select the port for the pin number to be selected in
   --  the INTPIN0 bits of this register.
   type PINTSEL0_ENUM is
     (
      --  GPIO Port 0
      Gpio_Port_0,
      --  GPIO Port 1
      Gpio_Port_1,
      --  GPIO Port 2
      Gpio_Port_2,
      --  GPIO Port 3
      Gpio_Port_3,
      --  GPIO Port 4
      Gpio_Port_4,
      --  GPIO Port 5
      Gpio_Port_5,
      --  GPIO Port 6
      Gpio_Port_6,
      --  GPIO Port 7
      Gpio_Port_7)
     with Size => 3;
   for PINTSEL0_ENUM use
     (Gpio_Port_0 => 0,
      Gpio_Port_1 => 1,
      Gpio_Port_2 => 2,
      Gpio_Port_3 => 3,
      Gpio_Port_4 => 4,
      Gpio_Port_5 => 5,
      Gpio_Port_6 => 6,
      Gpio_Port_7 => 7);

   subtype PINTSEL0_INTPIN1_Field is LPC43_SVD.UInt5;
   subtype PINTSEL0_INTPIN2_Field is LPC43_SVD.UInt5;
   subtype PINTSEL0_INTPIN3_Field is LPC43_SVD.UInt5;

   --  Pin interrupt select register for pin interrupts 0 to 3.
   type PINTSEL0_Register is record
      --  Pint interrupt 0: Select the pin number within the GPIO port selected
      --  by the PORTSEL0 bit in this register.
      INTPIN0  : PINTSEL0_INTPIN0_Field := 16#0#;
      --  Pin interrupt 0: Select the port for the pin number to be selected in
      --  the INTPIN0 bits of this register.
      PORTSEL0 : PINTSEL0_ENUM := LPC43_SVD.SCU.Gpio_Port_0;
      --  Pint interrupt 1: Select the pin number within the GPIO port selected
      --  by the PORTSEL1 bit in this register.
      INTPIN1  : PINTSEL0_INTPIN1_Field := 16#0#;
      --  Pin interrupt 1: Select the port for the pin number to be selected in
      --  the INTPIN1 bits of this register.
      PORTSEL1 : PINTSEL0_ENUM := LPC43_SVD.SCU.Gpio_Port_0;
      --  Pint interrupt 2: Select the pin number within the GPIO port selected
      --  by the PORTSEL2 bit in this register.
      INTPIN2  : PINTSEL0_INTPIN2_Field := 16#0#;
      --  Pin interrupt 2: Select the port for the pin number to be selected in
      --  the INTPIN2 bits of this register.
      PORTSEL2 : PINTSEL0_ENUM := LPC43_SVD.SCU.Gpio_Port_0;
      --  Pint interrupt 3: Select the pin number within the GPIO port selected
      --  by the PORTSEL3 bit in this register.
      INTPIN3  : PINTSEL0_INTPIN3_Field := 16#0#;
      --  Pin interrupt 3: Select the port for the pin number to be selected in
      --  the INTPIN3 bits of this register.
      PORTSEL3 : PINTSEL0_ENUM := LPC43_SVD.SCU.Gpio_Port_0;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PINTSEL0_Register use record
      INTPIN0  at 0 range 0 .. 4;
      PORTSEL0 at 0 range 5 .. 7;
      INTPIN1  at 0 range 8 .. 12;
      PORTSEL1 at 0 range 13 .. 15;
      INTPIN2  at 0 range 16 .. 20;
      PORTSEL2 at 0 range 21 .. 23;
      INTPIN3  at 0 range 24 .. 28;
      PORTSEL3 at 0 range 29 .. 31;
   end record;

   subtype PINTSEL1_INTPIN4_Field is LPC43_SVD.UInt5;

   --  Pin interrupt 4: Select the port for the pin number to be selected in
   --  the INTPIN4 bits of this register.
   type PINTSEL1_ENUM is
     (
      --  GPIO Port 0
      Gpio_Port_0,
      --  GPIO Port 1
      Gpio_Port_1,
      --  GPIO Port 2
      Gpio_Port_2,
      --  GPIO Port 3
      Gpio_Port_3,
      --  GPIO Port 4
      Gpio_Port_4,
      --  GPIO Port 5
      Gpio_Port_5,
      --  GPIO Port 6
      Gpio_Port_6,
      --  GPIO Port 7
      Gpio_Port_7)
     with Size => 3;
   for PINTSEL1_ENUM use
     (Gpio_Port_0 => 0,
      Gpio_Port_1 => 1,
      Gpio_Port_2 => 2,
      Gpio_Port_3 => 3,
      Gpio_Port_4 => 4,
      Gpio_Port_5 => 5,
      Gpio_Port_6 => 6,
      Gpio_Port_7 => 7);

   subtype PINTSEL1_INTPIN5_Field is LPC43_SVD.UInt5;
   subtype PINTSEL1_INTPIN6_Field is LPC43_SVD.UInt5;
   subtype PINTSEL1_INTPIN7_Field is LPC43_SVD.UInt5;

   --  Pin interrupt select register for pin interrupts 4 to 7.
   type PINTSEL1_Register is record
      --  Pint interrupt 4: Select the pin number within the GPIO port selected
      --  by the PORTSEL4 bit in this register.
      INTPIN4  : PINTSEL1_INTPIN4_Field := 16#0#;
      --  Pin interrupt 4: Select the port for the pin number to be selected in
      --  the INTPIN4 bits of this register.
      PORTSEL4 : PINTSEL1_ENUM := LPC43_SVD.SCU.Gpio_Port_0;
      --  Pint interrupt 5: Select the pin number within the GPIO port selected
      --  by the PORTSEL5 bit in this register.
      INTPIN5  : PINTSEL1_INTPIN5_Field := 16#0#;
      --  Pin interrupt 5: Select the port for the pin number to be selected in
      --  the INTPIN5 bits of this register.
      PORTSEL5 : PINTSEL1_ENUM := LPC43_SVD.SCU.Gpio_Port_0;
      --  Pint interrupt 6: Select the pin number within the GPIO port selected
      --  by the PORTSEL6 bit in this register.
      INTPIN6  : PINTSEL1_INTPIN6_Field := 16#0#;
      --  Pin interrupt 6: Select the port for the pin number to be selected in
      --  the INTPIN6 bits of this register.
      PORTSEL6 : PINTSEL1_ENUM := LPC43_SVD.SCU.Gpio_Port_0;
      --  Pint interrupt 7: Select the pin number within the GPIO port selected
      --  by the PORTSEL7 bit in this register.
      INTPIN7  : PINTSEL1_INTPIN7_Field := 16#0#;
      --  Pin interrupt 7: Select the port for the pin number to be selected in
      --  the INTPIN7 bits of this register.
      PORTSEL7 : PINTSEL1_ENUM := LPC43_SVD.SCU.Gpio_Port_0;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PINTSEL1_Register use record
      INTPIN4  at 0 range 0 .. 4;
      PORTSEL4 at 0 range 5 .. 7;
      INTPIN5  at 0 range 8 .. 12;
      PORTSEL5 at 0 range 13 .. 15;
      INTPIN6  at 0 range 16 .. 20;
      PORTSEL6 at 0 range 21 .. 23;
      INTPIN7  at 0 range 24 .. 28;
      PORTSEL7 at 0 range 29 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  System Control Unit (SCU) I/O configuration
   type SCU_Peripheral is record
      --  Pin configuration register for pins P0
      SFSP0       : aliased SFSP0_Registers;
      --  Pin configuration register for pins P1
      SFSP1       : aliased SFSP1_Registers;
      --  Pin configuration register for pins P2
      SFSP2       : aliased SFSP2_Registers;
      --  Pin configuration register for pins P3
      SFSP3       : aliased SFSP3_Registers;
      --  Pin configuration register for pins P4
      SFSP4       : aliased SFSP4_Registers;
      --  Pin configuration register for pins P5
      SFSP5       : aliased SFSP5_Registers;
      --  Pin configuration register for pins P6
      SFSP6       : aliased SFSP6_Registers;
      --  Pin configuration register for pins P7
      SFSP7       : aliased SFSP7_Registers;
      --  Pin configuration register for pins P8
      SFSP8       : aliased SFSP8_Registers;
      --  Pin configuration register for pins P9
      SFSP9       : aliased SFSP9_Registers;
      --  Pin configuration register for pins PA
      SFSPA       : aliased SFSPA_Registers;
      --  Pin configuration register for pins PB
      SFSPB       : aliased SFSPB_Registers;
      --  Pin configuration register for pins PC
      SFSPC       : aliased SFSPC_Registers;
      --  Pin configuration register for pins PD
      SFSPD       : aliased SFSPD_Registers;
      --  Pin configuration register for pins PE
      SFSPE       : aliased SFSPE_Registers;
      --  Pin configuration register for pins PF
      SFSPF       : aliased SFSPF_Registers;
      --  Pin configuration register for pins CLK
      SFSCLK      : aliased SFSCLK_Registers;
      --  Pin configuration register for pins USB1_DM and USB1_DP
      SFSUSB      : aliased SFSUSB_Register;
      --  Pin configuration register for I2C0-bus pins
      SFSI2C0     : aliased SFSI2C0_Register;
      --  ADC0 function select register
      ENAIO0      : aliased ENAIO0_Register;
      --  ADC1 function select register
      ENAIO1      : aliased ENAIO1_Register;
      --  Analog function select register
      ENAIO2      : aliased ENAIO2_Register;
      --  EMC clock delay register
      EMCDELAYCLK : aliased EMCDELAYCLK_Register;
      --  SD/MMC sample and drive delay register
      SDDELAY     : aliased SDDELAY_Register;
      --  Pin interrupt select register for pin interrupts 0 to 3.
      PINTSEL0    : aliased PINTSEL0_Register;
      --  Pin interrupt select register for pin interrupts 4 to 7.
      PINTSEL1    : aliased PINTSEL1_Register;
   end record
     with Volatile;

   for SCU_Peripheral use record
      SFSP0       at 16#0# range 0 .. 63;
      SFSP1       at 16#80# range 0 .. 671;
      SFSP2       at 16#100# range 0 .. 447;
      SFSP3       at 16#180# range 0 .. 287;
      SFSP4       at 16#200# range 0 .. 351;
      SFSP5       at 16#280# range 0 .. 255;
      SFSP6       at 16#300# range 0 .. 415;
      SFSP7       at 16#380# range 0 .. 255;
      SFSP8       at 16#400# range 0 .. 287;
      SFSP9       at 16#480# range 0 .. 223;
      SFSPA       at 16#500# range 0 .. 159;
      SFSPB       at 16#580# range 0 .. 223;
      SFSPC       at 16#600# range 0 .. 479;
      SFSPD       at 16#680# range 0 .. 543;
      SFSPE       at 16#700# range 0 .. 511;
      SFSPF       at 16#780# range 0 .. 383;
      SFSCLK      at 16#C00# range 0 .. 127;
      SFSUSB      at 16#C80# range 0 .. 31;
      SFSI2C0     at 16#C84# range 0 .. 31;
      ENAIO0      at 16#C88# range 0 .. 31;
      ENAIO1      at 16#C8C# range 0 .. 31;
      ENAIO2      at 16#C90# range 0 .. 31;
      EMCDELAYCLK at 16#D00# range 0 .. 31;
      SDDELAY     at 16#D80# range 0 .. 31;
      PINTSEL0    at 16#E00# range 0 .. 31;
      PINTSEL1    at 16#E04# range 0 .. 31;
   end record;

   --  System Control Unit (SCU) I/O configuration
   SCU_Periph : aliased SCU_Peripheral
     with Import, Address => System'To_Address (16#40086000#);

end LPC43_SVD.SCU;

--  This spec has been automatically generated from LPC43xx_43Sxx.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces;  use Interfaces;
with System;

--  Register cmsis file for LPC43xx parts
package LPC43_SVD is
   pragma Preelaborate;

   ---------------
   -- Base type --
   ---------------

   type UInt32 is new Interfaces.Unsigned_32;
   type UInt16 is new Interfaces.Unsigned_16;
   type UInt8 is new Interfaces.Unsigned_8;
   type Bit is mod 2**1
     with Size => 1;
   type UInt2 is mod 2**2
     with Size => 2;
   type UInt3 is mod 2**3
     with Size => 3;
   type UInt4 is mod 2**4
     with Size => 4;
   type UInt5 is mod 2**5
     with Size => 5;
   type UInt6 is mod 2**6
     with Size => 6;
   type UInt7 is mod 2**7
     with Size => 7;
   type UInt9 is mod 2**9
     with Size => 9;
   type UInt10 is mod 2**10
     with Size => 10;
   type UInt11 is mod 2**11
     with Size => 11;
   type UInt12 is mod 2**12
     with Size => 12;
   type UInt13 is mod 2**13
     with Size => 13;
   type UInt14 is mod 2**14
     with Size => 14;
   type UInt15 is mod 2**15
     with Size => 15;
   type UInt17 is mod 2**17
     with Size => 17;
   type UInt18 is mod 2**18
     with Size => 18;
   type UInt19 is mod 2**19
     with Size => 19;
   type UInt20 is mod 2**20
     with Size => 20;
   type UInt21 is mod 2**21
     with Size => 21;
   type UInt22 is mod 2**22
     with Size => 22;
   type UInt23 is mod 2**23
     with Size => 23;
   type UInt24 is mod 2**24
     with Size => 24;
   type UInt25 is mod 2**25
     with Size => 25;
   type UInt26 is mod 2**26
     with Size => 26;
   type UInt27 is mod 2**27
     with Size => 27;
   type UInt28 is mod 2**28
     with Size => 28;
   type UInt29 is mod 2**29
     with Size => 29;
   type UInt30 is mod 2**30
     with Size => 30;
   type UInt31 is mod 2**31
     with Size => 31;

   --------------------
   -- Base addresses --
   --------------------

   SCT_Base : constant System.Address :=
     System'To_Address (16#40000000#);
   GPDMA_Base : constant System.Address :=
     System'To_Address (16#40002000#);
   SPIFI_Base : constant System.Address :=
     System'To_Address (16#40003000#);
   SDMMC_Base : constant System.Address :=
     System'To_Address (16#40004000#);
   EMC_Base : constant System.Address :=
     System'To_Address (16#40005000#);
   USB0_Base : constant System.Address :=
     System'To_Address (16#40006000#);
   USB1_Base : constant System.Address :=
     System'To_Address (16#40007000#);
   LCD_Base : constant System.Address :=
     System'To_Address (16#40008000#);
   EEPROM_Base : constant System.Address :=
     System'To_Address (16#4000E000#);
   ETHERNET_Base : constant System.Address :=
     System'To_Address (16#40010000#);
   ATIMER_Base : constant System.Address :=
     System'To_Address (16#40040000#);
   REGFILE_Base : constant System.Address :=
     System'To_Address (16#40041000#);
   PMC_Base : constant System.Address :=
     System'To_Address (16#40042000#);
   CREG_Base : constant System.Address :=
     System'To_Address (16#40043000#);
   EVENTROUTER_Base : constant System.Address :=
     System'To_Address (16#40044000#);
   RTC_Base : constant System.Address :=
     System'To_Address (16#40046000#);
   CGU_Base : constant System.Address :=
     System'To_Address (16#40050000#);
   CCU1_Base : constant System.Address :=
     System'To_Address (16#40051000#);
   CCU2_Base : constant System.Address :=
     System'To_Address (16#40052000#);
   RGU_Base : constant System.Address :=
     System'To_Address (16#40053000#);
   WWDT_Base : constant System.Address :=
     System'To_Address (16#40080000#);
   USART0_Base : constant System.Address :=
     System'To_Address (16#40081000#);
   USART2_Base : constant System.Address :=
     System'To_Address (16#400C1000#);
   USART3_Base : constant System.Address :=
     System'To_Address (16#400C2000#);
   UART1_Base : constant System.Address :=
     System'To_Address (16#40082000#);
   SSP0_Base : constant System.Address :=
     System'To_Address (16#40083000#);
   SSP1_Base : constant System.Address :=
     System'To_Address (16#400C5000#);
   TIMER0_Base : constant System.Address :=
     System'To_Address (16#40084000#);
   TIMER1_Base : constant System.Address :=
     System'To_Address (16#40085000#);
   TIMER2_Base : constant System.Address :=
     System'To_Address (16#400C3000#);
   TIMER3_Base : constant System.Address :=
     System'To_Address (16#400C4000#);
   SCU_Base : constant System.Address :=
     System'To_Address (16#40086000#);
   GPIO_PIN_INT_Base : constant System.Address :=
     System'To_Address (16#40087000#);
   GPIO_GROUP_INT0_Base : constant System.Address :=
     System'To_Address (16#40088000#);
   GPIO_GROUP_INT1_Base : constant System.Address :=
     System'To_Address (16#40089000#);
   MCPWM_Base : constant System.Address :=
     System'To_Address (16#400A0000#);
   I2C0_Base : constant System.Address :=
     System'To_Address (16#400A1000#);
   I2C1_Base : constant System.Address :=
     System'To_Address (16#400E0000#);
   I2S0_Base : constant System.Address :=
     System'To_Address (16#400A2000#);
   I2S1_Base : constant System.Address :=
     System'To_Address (16#400A3000#);
   C_CAN1_Base : constant System.Address :=
     System'To_Address (16#400A4000#);
   RITIMER_Base : constant System.Address :=
     System'To_Address (16#400C0000#);
   QEI_Base : constant System.Address :=
     System'To_Address (16#400C6000#);
   GIMA_Base : constant System.Address :=
     System'To_Address (16#400C7000#);
   DAC_Base : constant System.Address :=
     System'To_Address (16#400E1000#);
   C_CAN0_Base : constant System.Address :=
     System'To_Address (16#400E2000#);
   ADC0_Base : constant System.Address :=
     System'To_Address (16#400E3000#);
   ADC1_Base : constant System.Address :=
     System'To_Address (16#400E4000#);
   ADCHS_Base : constant System.Address :=
     System'To_Address (16#400F0000#);
   GPIO_PORT_Base : constant System.Address :=
     System'To_Address (16#400F4000#);
   SPI_Base : constant System.Address :=
     System'To_Address (16#40100000#);
   SGPIO_Base : constant System.Address :=
     System'To_Address (16#40101000#);

end LPC43_SVD;

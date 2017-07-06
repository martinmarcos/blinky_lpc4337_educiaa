with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with Interfaces.LPC4337;           use Interfaces.LPC4337;
with Interfaces.LPC4337.SCU;       use Interfaces.LPC4337.SCU;
with Interfaces.LPC4337.GPIO_PORT; use Interfaces.LPC4337.GPIO_PORT;
with Ada.Real_Time;                use Ada.Real_Time;
with Ada.Text_IO;                  use Ada.Text_IO;

procedure Blinky is

   Period : constant Time_Span := Milliseconds (200);  -- arbitrary

   Next_Release : Time := Clock;

   type SFS_Register_Access is access all SFS_Register;

   type port is array (Integer range <>) of SFS_Register_Access;

   type port_access is access constant port;

   type ports_access is array (Integer range <>) of port_access;

   port0 : aliased constant port :=
     (0 => SCU_Periph.SFSP0 (0)'Access,
      1 => SCU_Periph.SFSP0 (1)'Access);

   port1 : aliased constant port :=
     (0 => SCU_Periph.SFSP1.Pins_0_16 (0)'Access,
      1 => SCU_Periph.SFSP1.Pins_0_16 (1)'Access,
      2 => SCU_Periph.SFSP1.Pins_0_16 (2)'Access,
      3 => SCU_Periph.SFSP1.Pins_0_16 (3)'Access,
      4 => SCU_Periph.SFSP1.Pins_0_16 (4)'Access,
      5 => SCU_Periph.SFSP1.Pins_0_16 (5)'Access,
      6 => SCU_Periph.SFSP1.Pins_0_16 (6)'Access,
      7 => SCU_Periph.SFSP1.Pins_0_16 (7)'Access,
      8 => SCU_Periph.SFSP1.Pins_0_16 (8)'Access,
      9 => SCU_Periph.SFSP1.Pins_0_16 (9)'Access,
      10 => SCU_Periph.SFSP1.Pins_0_16 (10)'Access,
      11 => SCU_Periph.SFSP1.Pins_0_16 (11)'Access,
      12 => SCU_Periph.SFSP1.Pins_0_16 (12)'Access,
      13 => SCU_Periph.SFSP1.Pins_0_16 (13)'Access,
      14 => SCU_Periph.SFSP1.Pins_0_16 (14)'Access,
      15 => SCU_Periph.SFSP1.Pins_0_16 (15)'Access,
      16 => SCU_Periph.SFSP1.Pins_0_16 (16)'Access,
      17 => SCU_Periph.SFSP1.Pin17 (17)'Access,
      18 => SCU_Periph.SFSP1.Pins_18_20 (18)'Access,
      19 => SCU_Periph.SFSP1.Pins_18_20 (19)'Access,
      20 => SCU_Periph.SFSP1.Pins_18_20 (20)'Access);

   port2 : aliased constant port :=
     (0 => SCU_Periph.SFSP2.Pins_0_2 (0)'Access,
      1 => SCU_Periph.SFSP2.Pins_0_2 (1)'Access,
      2 => SCU_Periph.SFSP2.Pins_0_2 (2)'Access,
      3 => SCU_Periph.SFSP2.Pins_3_5 (3)'Access,
      4 => SCU_Periph.SFSP2.Pins_3_5 (4)'Access,
      5 => SCU_Periph.SFSP2.Pins_3_5 (5)'Access,
      6 => SCU_Periph.SFSP2.Pins_6_13 (6)'Access,
      7 => SCU_Periph.SFSP2.Pins_6_13 (7)'Access,
      8 => SCU_Periph.SFSP2.Pins_6_13 (8)'Access,
      9 => SCU_Periph.SFSP2.Pins_6_13 (9)'Access,
      10 => SCU_Periph.SFSP2.Pins_6_13 (10)'Access,
      11 => SCU_Periph.SFSP2.Pins_6_13 (11)'Access,
      12 => SCU_Periph.SFSP2.Pins_6_13 (12)'Access,
      13 => SCU_Periph.SFSP2.Pins_6_13 (13)'Access);

   ports : constant ports_access (0 .. 2) := (0 => port0'Access,
                                              1 => port1'Access,
                                              2 => port2'Access);

   procedure Initialize_LEDs;
   --  Enables the clock and configures the GPIO pins and port connected to the
   --  LEDs on the target board so that we can drive them via GPIO commands.
   --  Note that the STM32.Board package provides a procedure (with the same
   --  name) to do this directly, for convenience, but we do not use it here
   --  for the sake of illustration.

   procedure Initialize_LEDs is
      --  Configuration : GPIO_Port_Configuration;
   begin
      --  Enable_Clock (All_LEDs);

      --  Configuration.Mode        := Mode_Out;
      --  Configuration.Output_Type := Push_Pull;
      --  Configuration.Speed       := Speed_100MHz;
      --  Configuration.Resistors   := Floating;
      --  Configure_IO (All_LEDs, Configuration);

      ports (2).all (12).EPUN := Enable_Pull_Up;
      ports (2).all (12).EZI  := Enable_Input_Buffer;
      ports (2).all (12).MODE := Function_0_Default;

      GPIO_PORT_Periph.DIR (1) := (As_Array => True,
                                   Arr => (12 => 1, others => <>));
      GPIO_PORT_Periph.CLR (1) := (As_Array => True,
                                   Arr => (12 => 1, others => <>));

   end Initialize_LEDs;

begin
   Put_Line ("Hello World!");
   Initialize_LEDs;

   loop
      --  Toggle (All_LEDs);
      if GPIO_PORT_Periph.SET (1).Arr (12) = 1 then
         GPIO_PORT_Periph.CLR (1) := (As_Array => True,
                                      Arr => (12 => 1, others => <>));
      else
         GPIO_PORT_Periph.SET (1) := (As_Array => True,
                                      Arr => (12 => 1, others => <>));
      end if;

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
end Blinky;

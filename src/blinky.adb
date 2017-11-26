with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with LPC43_SVD;           use LPC43_SVD;
with LPC43_SVD.SCU;       use LPC43_SVD.SCU;
with LPC43_SVD.GPIO_PORT; use LPC43_SVD.GPIO_PORT;
with Ada.Real_Time;                use Ada.Real_Time;
with Ada.Text_IO;                  use Ada.Text_IO;

procedure Blinky is

   Period : constant Time_Span := Milliseconds (200);  -- arbitrary

   Next_Release : Time := Clock;

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

      SCU_Periph.SFSP2.Pins_0_2 (0).EPUN := Enable_Pull_Up;
      SCU_Periph.SFSP2.Pins_0_2 (0).EPD := Disable_Pull_Down;
      SCU_Periph.SFSP2.Pins_0_2 (0).EZI := Enable_Input_Buffer;
      SCU_Periph.SFSP2.Pins_0_2 (0).MODE := Function_4;

      SCU_Periph.SFSP2.Pins_0_2 (1).EPUN := Enable_Pull_Up;
      SCU_Periph.SFSP2.Pins_0_2 (1).EPD := Disable_Pull_Down;
      SCU_Periph.SFSP2.Pins_0_2 (1).EZI := Enable_Input_Buffer;
      SCU_Periph.SFSP2.Pins_0_2 (1).MODE := Function_4;

      SCU_Periph.SFSP2.Pins_0_2 (2).EPUN := Enable_Pull_Up;
      SCU_Periph.SFSP2.Pins_0_2 (2).EPD := Disable_Pull_Down;
      SCU_Periph.SFSP2.Pins_0_2 (2).EZI := Enable_Input_Buffer;
      SCU_Periph.SFSP2.Pins_0_2 (2).MODE := Function_4;

      GPIO_PORT_Periph.DIR (5) := (As_Array => True,
                        Arr => (0 => True, 1 => True, 2 => True, others => <>));
      GPIO_PORT_Periph.CLR (5) := (As_Array => True,
                        Arr => (0 => True, 1 => True, 2 => True, others => <>));

      SCU_Periph.SFSP2.Pins_6_13 (10).EPUN := Enable_Pull_Up;
      SCU_Periph.SFSP2.Pins_6_13 (10).EPD := Disable_Pull_Down;
      SCU_Periph.SFSP2.Pins_6_13 (10).EZI := Enable_Input_Buffer;
      SCU_Periph.SFSP2.Pins_6_13 (10).MODE := Function_0_Default;

      GPIO_PORT_Periph.DIR (0) := (As_Array => True,
                                             Arr => (14 => True, others => <>));
      GPIO_PORT_Periph.CLR (0) := (As_Array => True,
                                             Arr => (14 => True, others => <>));

      SCU_Periph.SFSP2.Pins_6_13 (11).EPUN := Enable_Pull_Up;
      SCU_Periph.SFSP2.Pins_6_13 (11).EPD := Disable_Pull_Down;
      SCU_Periph.SFSP2.Pins_6_13 (11).EZI := Enable_Input_Buffer;
      SCU_Periph.SFSP2.Pins_6_13 (11).MODE := Function_0_Default;

      SCU_Periph.SFSP2.Pins_6_13 (12).EPUN := Enable_Pull_Up;
      SCU_Periph.SFSP2.Pins_6_13 (12).EPD := Disable_Pull_Down;
      SCU_Periph.SFSP2.Pins_6_13 (12).EZI := Enable_Input_Buffer;
      SCU_Periph.SFSP2.Pins_6_13 (12).MODE := Function_0_Default;

      GPIO_PORT_Periph.DIR (1) := (As_Array => True,
                                 Arr => (11 => True, 12 => True, others => <>));
      GPIO_PORT_Periph.CLR (1) := (As_Array => True,
                                 Arr => (11 => True, 12 => True, others => <>));

   end Initialize_LEDs;

begin
   Put_Line ("Hello World!");
   Initialize_LEDs;

   loop
      --  Toggle (All_LEDs);

      GPIO_PORT_Periph.NOT_k (1) := (As_Array => True,
                                 Arr => (12 => True, others => <>));

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
end Blinky;

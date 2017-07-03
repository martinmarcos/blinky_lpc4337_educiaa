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

   LED3 : SFS_Register_Access;

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

      LED3 := SCU_Periph.SFSP2.Pins_6_13 (12)'Access;

      LED3.EPUN := Enable_Pull_Up;
      LED3.EZI  := Enable_Input_Buffer;
      LED3.MODE := Function_0_Default;

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

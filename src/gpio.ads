with Interfaces.LPC4337.SCU;       use Interfaces.LPC4337.SCU;

package GPIO is

   type SFS_Register_Access is access all SFS_Register;

   type port is array (Integer range <>) of SFS_Register_Access;

   type port_access is access constant port;

   type groups is (P0, P1, P2, P3,
                   P4, P5, P6, P7,
                   P8, P9, PA, PB,
                   PC, PD, PE, PF);

   type ports_access is array (groups range <>) of port_access;

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

   ports : constant ports_access := (P0 => port0'Access,
                                     P1 => port1'Access,
                                     P2 => port2'Access);

end GPIO;

with Ada.Command_Line; use Ada.Command_Line;

package Config is
   type Config_Type is record
      FPS         : Natural := 30;
      Height      : Positive := 24;
      Width       : Positive := 80;
      Frame_Limit : Integer := -1;-- infinit
      Z_Step      : Float := 0.1;
      Verbose     : Boolean := False;
   end record;
   procedure Make_Config (Config : in out Config_Type);
   procedure Put (Config : in Config_Type);
end Config;

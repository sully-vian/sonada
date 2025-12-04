with Ada.Containers.Vectors;

package Config is
   package Integer_Vectors is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Integer);
   subtype Color_Vector is Integer_Vectors.Vector;

   Show_Help   : exception;
   Parse_Error : exception;

   type Config_Type is record
      FPS        : Positive := 30;
      Height     : Positive := 24;
      Width      : Positive := 80;
      Num_Frames : Integer := -1; -- infinite
      Z_Step     : Float := 0.02;
      Verbose    : Boolean := False;
      Dump       : Boolean := False;
      Colors     : Color_Vector := [for I in 0 .. 23 => I + 232];
   end record;
   procedure Make_Config (Config : in out Config_Type);
end Config;

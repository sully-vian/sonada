with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Utils;                  use Utils;

package body Colors is
   Gray_Min : constant Float := 232.0;
   Gray_Max : constant Float := 255.0;

   -- normalize X in [-1, 1] to [Min,Max]
   function Normalize
     (X : in Float; Min : in Float; Max : in Float) return Float
   is
      Temp : constant Float := Min + (X + 1.0) * (Max - Min) / 2.0;
   begin
      return Temp;
   end Normalize;

   function Grayscale (Noise : in Float; C : in Character) return Gray_String
   is
      Norm   : constant Integer :=
        Integer (Normalize (Noise, Gray_Min, Gray_Max));
      Gray   : constant String := ESC & "[48;5;" & String_Of (Norm) & "m";
      Result : constant Gray_String := Gray & C;
   begin
      return Result;
   end Grayscale;
end Colors;

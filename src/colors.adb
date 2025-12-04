with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;

package body Colors is
   subtype Color_Code is Integer range 0 .. 255;
   subtype Color_String is String (1 .. 3);

   Min_Ind : constant Float := Float (Config.Colors.First_Index);
   Max_Ind : constant Float := Float (Config.Colors.Last_Index);

   function Three_Char_Of (C : in Color_Code) return Color_String is
      S : constant String := Trim (C'Image, Ada.Strings.Left);
   begin
      return (3 - S'Length) * "0" & S;
   end Three_Char_Of;

   -- normalize X in [-1, 1] to [Min,Max]
   function ANSI_Of (X : in Float) return Color_Code is
      Temp : constant Float := Min_Ind + (X + 1.0) * (Max_Ind - Min_Ind) / 2.0;
   begin
      return Config.Colors.Element (Integer (Temp));
   end ANSI_Of;

   function Color (Noise : in Float; C : in Character) return Gray_String is
      Norm   : constant Color_Code := ANSI_Of (Noise);
      Gray   : constant String := ESC & "[48;5;" & Three_Char_Of (Norm) & "m";
      Result : constant Gray_String := Gray & C;
   begin
      return Result;
   end Color;
end Colors;

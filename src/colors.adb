with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Utils;                  use Utils;
with Constants;              use Constants;

package body Colors is
    Gray_Min : constant Float := 232.0;
    Gray_Max : constant Float := 255.0;

    -- normalize X in [-1, 1] to [A,B]
    function Normalize (X : in Float; A : in Float; B : in Float) return Float
    is
    begin
        return A + (X + 1.0) * (B - A) / 2.0;
    end Normalize;

    function Grayscale (Noise : in Float) return Wide_Wide_String is
        Norm  : constant Integer :=
           Integer (Normalize (Noise, Gray_Min, Gray_Max));
        Gray  : constant String := ESC & "[48;5;" & String_Of (Norm) & "m";
        Reset : constant String := ESC & "[0m";
    begin
        return
           Wide_Wide_String (Gray) & Lower_Square & Wide_Wide_String (Reset);
    end;
end Colors;

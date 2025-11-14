with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Utils;                  use Utils;
with Constants;              use Constants;
with Ada.Text_IO;            use Ada.Text_IO;

package body Colors is
    Gray_Min : constant Float := 232.0;
    Gray_Max : constant Float := 255.0;

    -- normalize X in [-1, 1] to [A,B]
    function Normalize (X : in Float; A : in Float; B : in Float) return Float
    is
    begin
        return A + (X + 1.0) * (B - A) / 2.0;
    end Normalize;

    function Grayscale (Noise : in Float; C : in Character) return Gray_String
    is
        Norm   : constant Integer :=
           Integer (Normalize (Noise, Gray_Min, Gray_Max));
        Gray   : constant String := ESC & "[48;5;" & String_Of (Norm) & "m";
        Reset  : constant String := ESC & "[0m";
        Result : constant Gray_String := Gray & C & Reset;
    begin
        return Result;
    end Grayscale;
end Colors;

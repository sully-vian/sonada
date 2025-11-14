with Lib;                        use Lib;
with Utils;                      use Utils;
with Colors;                     use Colors;
with Ada.Characters.Latin_1;     use Ada.Characters.Latin_1;
with Constants;                  use Constants;
--with Ada.Wide_Wide_Text_IO;      use Ada.Wide_Wide_Text_IO;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;

procedure Adanoise is
    Noise       : Float;
    Term_Width  : Integer;
    Term_Height : Integer;
    X           : Float;
    Y           : Float;
    S           : Gray_String;
begin
    Term_Size (Term_Height, Term_Width);
    for I in 0 .. Term_Height - 1 loop
        Y := Float (I) / Float (Term_Height - 1);
        for J in 0 .. Term_Width - 1 loop
            X := Float (J) / Float (Term_Width - 1);
            Noise := Perlin (X, Y);
            S := Grayscale (Noise, ' ');
            Put (S);
        end loop;
        --Put_Line ((1 => CR) & "");
    end loop;
end Adanoise;

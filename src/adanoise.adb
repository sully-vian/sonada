--with Ada.Text_IO;            use Ada.Text_IO;
with Lib;                    use Lib;
with Colors;                 use Colors;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Constants;              use Constants;
--with Ada.Wide_Text_IO;       use Ada.Wide_Text_IO;
with Ada.Wide_Wide_Text_IO;  use Ada.Wide_Wide_Text_IO;

procedure Adanoise is
    Noise : Float;
begin
    Noise := Perlin (1.0, 1.7);
    --Put_Line ("Noise:" & Noise'Image);
    --Put_Line (ESC & "[48;5;245m This has a gray background " & ESC & "[0m");
    --Put_Line (ESC & "[38;5;245m This is gray text " & ESC & "[0m");
    --Put_Line (Grayscale (Noise));
    --Put_Line (Wide_Wide_Character'Val (16#20AC#));
    Put_Line ("[€]");
end Adanoise;

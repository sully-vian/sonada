with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings;                use Ada.Strings;
with GNATCOLL.Terminal;          use GNATCOLL.Terminal;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;

package body Utils is
    function String_Of (N : in Integer) return String is
    begin
        return Trim (N'Image, Left);
    end String_Of;

    function Wide_Wide_String_Of (S : in String) return Wide_Wide_String is
    begin
        return To_Wide_Wide_String (S);
    end Wide_Wide_String_Of;

    procedure Term_Size (Height : out Positive; Width : out Positive) is
        Term_Info : Terminal_Info;
    begin
        Term_Info.Init_For_Stdout;
        Height := Term_Info.Get_Lines;
        Width := Term_Info.Get_Width;
    end Term_Size;

    procedure Clear is
        Term_Info : Terminal_Info;
    begin
        Term_Info.Init_For_Stdout;
        Term_Info.Beginning_Of_Line;
        Term_Info.Clear_To_End_Of_Line;
    end Clear;
end Utils;

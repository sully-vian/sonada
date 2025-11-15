with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

package Utils is
    function String_Of (N : in Integer) return String;
    function Wide_Wide_String_Of (S : in String) return Wide_Wide_String;
    procedure Term_Size (Height : out Positive; Width : out Positive);
    procedure Clear;
end Utils;

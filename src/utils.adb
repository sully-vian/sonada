with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Utils is
    function String_Of (N : in Integer) return String is
    begin
        return Trim (N'Image, Ada.Strings.Left);
    end;

    function Wide_Wide_String_Of (S : in String) return Wide_Wide_String is
        Result : Wide_Wide_String (0 .. S'Length - 1);
        Index  : Natural := 0;
    begin
        for I in S'Range loop
            Result (Index) := Wide_Wide_Character'Val ('A');--S (I));
            Index := Index + 1;
        end loop;
        return Result;
    end;
end Utils;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Utils is
    function String_Of (N : in Integer) return String is
    begin
        return Trim (N'Image, Ada.Strings.Left);
    end;
end;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings;       use Ada.Strings;

package body Utils is
   function String_Of (N : in Integer) return String is
   begin
      return Trim (N'Image, Left);
   end String_Of;
end Utils;

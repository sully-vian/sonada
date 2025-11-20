with Ada.Text_IO;  use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with Interfaces;   use Interfaces;
with System;       use System;

procedure Terminal_Size (Terminal_Height, Terminal_Width : out Positive) is
   STDIN_FILENO : constant C.int := 0; -- Macro from <unistd.h>
   TIOCGWINSZ   : constant C.unsigned_long :=
     16#5413#; -- Macro from <sys/ioctl.h>

   Default_Height : constant Integer := 24;
   Default_Width  : constant Integer := 80;

   type winsize is record
      ws_row    : C.unsigned_short;
      ws_col    : C.unsigned_short;
      ws_xpixel : C.unsigned_short;
      ws_ypixel : C.unsigned_short;
   end record;
   pragma Convention (C, winsize);

   function Ioctl
     (File_Desc : in C.int; Request : in C.unsigned_long; Argp : in Address)
      return C.int;
   pragma Import (C, Ioctl, "ioctl");

   w : winsize;
begin
   Put_Line
     (Ioctl
        (STDIN_FILENO,
         TIOCGWINSZ,
         w'Address)'Image); -- no check for return type, the
   Put_Line (w.ws_row'Image & "|" & w.ws_col'Image);
   Terminal_Height := Positive (w.ws_row);
   Terminal_Width := Positive (w.ws_col);
exception
   when others =>
      Terminal_Height := Default_Height;
      Terminal_Width := Default_Width;
end Terminal_Size;

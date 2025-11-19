with Ada.Text_IO; use Ada.Text_IO;
with Colors;      use Colors;
with Interrupt_Handler;
pragma Unreferenced (Interrupt_Handler);
pragma Unreserve_All_Interrupts;
with Lib;         use Lib;
with Terminal_Size;

procedure Sonada is
   -- ANSI escape codes
   Clear       : constant String := Character'Val (27) & "[2J";
   Move_Home   : constant String := Character'Val (27) & "[H";
   LF          : constant Character := Character'Val (10);
   Hide_Cursor : constant String := Character'Val (27) & "[?25l";
   Show_cursor : constant String := Character'Val (27) & "[?25h";
   Reset       : constant String := Character'Val (27) & "[0m";

   type Pixel_Coord is record
      X : Float;
      Y : Float;
   end record;

   type Coord_Matrix is
     array (Natural range <>, Natural range <>) of Pixel_Coord;

   -- Arr must have lower index 0
   procedure Build_Coord_Array
     (Min_X, Min_Y, Max_X, Max_Y : in Float; Arr : out Coord_Matrix)
   is
      D_X : constant Float := (Max_X - Min_X) / Float (Arr'Length(1) - 1);
      D_Y : constant Float :=
        (Max_Y - Min_Y) / Float (Arr'Length(2) - 1) * 2.0;
   begin
      for I in Arr'Range(1) loop
         for J in Arr'Range(2) loop
            Arr (I, J).X := Min_X + Float (I) * D_X;
            Arr (I, J).Y := Min_Y + Float (J) * D_Y;
         end loop;
      end loop;
   end Build_Coord_Array;

   procedure Print_Frame (Coords : in Coord_Matrix; Time_Z : in Float) is
      NX          : constant Integer := Coords'Length(2);
      NY          : constant Integer := Coords'Length(1);
      Cell_Length : constant Integer := Gray_String'Length;

      -- one newline per row except the last
      Total : constant Integer := NX * NY * Cell_Length + (NY - 1);
      Frame : String (1 .. Total) := (others => ' ');
      Pos   : Integer := 1;
      Noise : Float;
      Pixel : Pixel_Coord;
   begin
      for I in Coords'Range(1) loop
         for J in Coords'Range(2) loop
            Pixel := Coords (I, J);
            Noise := Perlin (Pixel.X, Pixel.Y, Time_Z);
            Frame (Pos .. Pos + Cell_Length - 1) := Grayscale (Noise, ' ');
            Pos := Pos + Cell_Length;
         end loop;
         if (I /= Coords'Last(1)) then
            Frame (Pos) := LF;
            Pos := Pos + 1;
         end if;
      end loop;
      Put (Move_Home & Frame); -- single write per frame
   end Print_Frame;

   procedure Main
     (Term_Height, Term_Width : in Positive; FPS : in Standard.Duration)
   is
      Coords : Coord_Matrix (0 .. Term_Height - 1, 0 .. Term_Width - 1);
      Time_Z : Float := 0.0;
   begin
      Build_Coord_Array (0.0, 0.0, 5.0, 5.0, Coords);
      -- loop for later animation
      Put (Hide_Cursor & Clear);
      loop
         Print_Frame (Coords, Time_Z);
         Time_Z := Time_Z + 0.1;
         delay 1.0 / FPS;
         -- exit;
      end loop;
   end Main;

   Term_Width, Term_Height : Positive;
   FPS                     : constant Integer := 30;
begin
   -- Get Variables
   -- TODO: parse cli args
   Terminal_Size (Term_Height, Term_Width);

   -- start
   --Main (5, 10, Standard.Duration (FPS));
   Main (Term_Height, Term_Width, Standard.Duration (FPS));
   Put (Reset & Show_Cursor);
exception
   when others =>
      Put (Reset & Show_Cursor);
      Put ("Caught");
      raise;
end Sonada;

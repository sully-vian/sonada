with GNAT.Command_Line; use GNAT.Command_Line;
with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Text_IO;       use Ada.Text_IO;
with Colors;            use Colors;
with Lib;               use Lib;
with Interrupt_Handler;
with Terminal_Size;

pragma Unreferenced (Interrupt_Handler);
pragma Unreserve_All_Interrupts;

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

   type Config_Type is record
      FPS         : Natural := 30;
      Height      : Positive := 24;
      Width       : Positive := 80;
      Frame_Limit : Integer := -1;-- infinit
      Z_Step      : Float := 0.1;
   end record;

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

   procedure Main (Config : in Config_Type) is
      Coords      :
        Coord_Matrix (0 .. Config.Height - 1, 0 .. Config.Width - 1);
      Time_Z      : Float := 0.0;
      Frame_Count : Integer := 0;
      Frametime   : constant Duration := Duration (1.0 / Config.FPS);
   begin
      Build_Coord_Array (0.0, 0.0, 5.0, 5.0, Coords);
      Put (Hide_Cursor & Clear);

      while Frame_Count /= Config.Frame_Limit loop
         Print_Frame (Coords, Time_Z);
         Time_Z := Time_Z + Config.Z_Step;
         Frame_Count := Frame_Count + 1;
         delay FrameTime;
      end loop;
   end Main;

   Term_Width, Term_Height : Positive;
   Config                  : Config_Type;
   Opt                     : Character;
begin
   -- Get Variables
   Terminal_Size (Term_Height, Term_Width);
   Config.Height := Term_Height;
   Config.Width := Term_Width;

   -- parse command line args (override defaults);
   loop
      Opt := Getopt ("f: h: w: n:");
      case Opt is
         when ASCII.NUL =>
            exit;

         when 'f' =>
            Config.FPS := Natural'Value (Parameter);

         when 'h' =>
            Config.Height := Positive'Value (Parameter);

         when 'w' =>
            Config.Width := Positive'Value (Parameter);

         when 'n' =>
            Config.Frame_Limit := Integer'Value (Parameter);

         when others =>
            null;
      end case;
   end loop;

   -- start
   --Main (5, 10, Standard.Duration (FPS));
   Main (Config);
   Put (Reset & Show_Cursor);
exception
   when Invalid_Switch =>
      Put_Line (Standard_Error, "Invalid switch: " & Full_Switch);
      Put (Reset & Show_Cursor);
      Set_Exit_Status (Failure);
   when Invalid_Parameter =>
      Put_Line
        (Standard_Error, "Invalid parameter for switch: " & Full_Switch);
      Put (Reset & Show_Cursor);
      Set_Exit_Status (Failure);
   when Constraint_Error =>
      Put_Line (Standard_Error, "Invalid numeric value provided.");
      Put (Reset & Show_Cursor);
      Set_Exit_Status (Failure);
   when others =>
      Put (Reset & Show_Cursor);
      raise;
end Sonada;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Text_IO;      use Ada.Text_IO;
with Colors;
with Config;           use Config;
with Lib;              use Lib;
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

   procedure Main (Config : in Config_Type) is
      package Colors_Conf is new Colors (Config => Config);

      procedure Print_Frame (Coords : in Coord_Matrix; Time_Z : in Float) is
         NX          : constant Integer := Coords'Length(2);
         NY          : constant Integer := Coords'Length(1);
         Cell_Length : constant Integer := 12;

         -- one newline per row except the last
         Total : constant Integer := NX * NY * Cell_Length + (NY - 1);
         Frame : String (1 .. Total) := [others => ' '];
         Pos   : Integer := 1;
         Noise : Float;
         Pixel : Pixel_Coord;
      begin
         for I in Coords'Range(1) loop
            for J in Coords'Range(2) loop
               Pixel := Coords (I, J);
               Noise := Perlin (Pixel.X, Pixel.Y, Time_Z);
               Frame (Pos .. Pos + Cell_Length - 1) :=
                 Colors_Conf.Color (Noise, ' ');
               Pos := Pos + Cell_Length;
            end loop;
            if (I /= Coords'Last(1)) then
               Frame (Pos) := LF;
               Pos := Pos + 1;
            end if;
         end loop;
         Put (Move_Home & Frame); -- single write per frame
      end Print_Frame;

      Coords      :
        Coord_Matrix (0 .. Config.Height - 1, 0 .. Config.Width - 1);
      Time_Z      : Float := 0.0;
      Frame_Count : Integer := 0;
      Frametime   : constant Duration := Duration (1.0 / Float (Config.FPS));
   begin
      Build_Coord_Array (0.0, 0.0, 2.0, 2.0, Coords);
      Put (Hide_Cursor & Clear);

      while Frame_Count /= Config.Num_Frames loop
         Print_Frame (Coords, Time_Z);
         Time_Z := Time_Z + Config.Z_Step;
         Frame_Count := Frame_Count + 1;
         delay FrameTime;
      end loop;
   end Main;

   Term_Width, Term_Height : Positive;
   Config                  : Config_Type;
begin
   -- Get Variables
   Terminal_Size (Term_Height, Term_Width);
   Config.Height := Term_Height;
   Config.Width := Term_Width;
   Make_Config (Config);
   if (Config.Dump) then
      Put (Config'Image);
      return;
   end if;

   -- start
   -- Main (5, 10, Standard.Duration (FPS));
   Main (Config);
   Put (Reset & Show_Cursor);
exception
   when E : Parse_Error | Show_Help =>
      Set_Exit_Status (Failure);
      Put_Line (Standard_Error, Exception_Message (E));
   when others =>
      Put (Reset & Show_Cursor);
      raise;
end Sonada;

with Lib;         use Lib;
with Colors;      use Colors;
with Ada.Text_IO; use Ada.Text_IO;
with Terminal_Size;

procedure Sonada is

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
        D_X : constant Float := (Max_X - Min_X) / Float (Arr'Length (1) - 1);
        D_Y : constant Float :=
           (Max_Y - Min_Y) / Float (Arr'Length (2) - 1) * 2.0;
    begin
        for I in Arr'Range (1) loop
            for J in Arr'Range (2) loop
                Arr (I, J).X := Min_X + Float (I) * D_X;
                Arr (I, J).Y := Min_Y + Float (J) * D_Y;
            end loop;
        end loop;
    end Build_Coord_Array;

    procedure Print_Frame (Coords : in Coord_Matrix) is
        NX          : constant Integer := Coords'Length (1);
        NY          : constant Integer := Coords'Length (2);
        Cell_Length : constant Integer := Gray_String'Length;

        -- one newline per row
        Total : constant Integer := NX * NY * Cell_Length + NY;
        Frame : String (1 .. Total) := (others => ' ');
        Pos   : Integer := 1;
        Noise : Float;
        Pixel : Pixel_Coord;
    begin
        for I in Coords'Range (1) loop
            for J in Coords'Range (2) loop
                Pixel := Coords (I, J);
                Noise := Perlin (Pixel.X, Pixel.Y);
                Frame (Pos .. Pos + Cell_Length - 1) := Grayscale (Noise, ' ');
                Pos := Pos + Cell_Length;
            end loop;
            Frame (Pos) := Character'Val (10); -- LF char
            --Frame (Pos) := '-';
            Pos := Pos + 1;
        end loop;
        Put (Frame); -- single write per frame
    end Print_Frame;

    procedure Main (Term_Height : in Positive; Term_Width : in Positive) is
        Coords : Coord_Matrix (0 .. Term_Height - 1, 0 .. Term_Width - 1);
    begin
        Build_Coord_Array (0.0, 0.0, 10.0, 10.0, Coords);
        -- loop for later animation
        loop
            Print_Frame (Coords);
            loop
                null;
            end loop;
            exit;
        end loop;
    end Main;

    Term_Width, Term_Height : Positive;
begin
    -- Get Variables
    -- TODO: parse cli args
    Terminal_Size (Term_Height, Term_Width);

    -- start
    Main (Term_Height, Term_Width);
end Sonada;

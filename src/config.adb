with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Text_IO;            use Ada.Text_IO;
with Help;

package body Config is

   Grayscale : constant Color_Vector := [for I in 0 .. 23 => I + 232];
   Viridis   : constant Color_Vector :=
     [17,
      18,
      54,
      55,
      56,
      57,
      61,
      62,
      68,
      69,
      70,
      76,
      82,
      112,
      148,
      149,
      184,
      190,
      191,
      192,
      193,
      229,
      230,
      231];

   type Option is
     (Help,
      Width,
      Height,
      Num_Frames,
      FPS,
      Z_Step,
      Verbose,
      Dump_Config,
      Colors,
      Gradient,
      Unknown_Opt);

   function Get_Option (I : in Positive) return Option is
      Arg : constant String := Argument (I);
   begin
      if Arg = "--help" then
         return Help;
      elsif Arg = "-w" or Arg = "--width" then
         return Width;
      elsif Arg = "-h" or Arg = "--height" then
         return Height;
      elsif Arg = "-f" or Arg = "--fps" then
         return FPS;
      elsif Arg = "-n" or Arg = "--num-frames" then
         return Num_Frames;
      elsif Arg = "-z" or Arg = "--z-step" then
         return Z_Step;
      elsif Arg = "-c" or Arg = "--colors" then
         return Colors;
      elsif Arg = "-g" or Arg = "--gradient" then
         return Gradient;
      elsif Arg = "-v" or Arg = "--verbose" then
         return Verbose;
      elsif Arg = "--dump-config" then
         return Dump_Config;
      else
         return Unknown_Opt;
      end if;
   end Get_Option;

   procedure Make_Config (Config : in out Config_Type) is
      I : Positive := 1;
   begin
      while (I <= Argument_Count) loop
         case Get_Option (I) is
            when Help        =>
               Help;
               Set_Exit_Status (Success);
               raise Show_Help;

            when Width       =>
               I := I + 1;
               Config.Width := Positive'Value (Argument (I));

            when Height      =>
               I := I + 1;
               Config.Height := Positive'Value (Argument (I));

            when Num_Frames  =>
               I := I + 1;
               Config.Num_Frames := Integer'Value (Argument (I));

            when FPS         =>
               I := I + 1;
               Config.FPS := Positive'Value (Argument (I));

            when Z_Step      =>
               I := I + 1;
               Config.Z_Step := Float'Value (Argument (I));

            when Colors      =>
               Config.Colors.Clear;
               while (I < Argument_Count) loop
                  if (Argument (I + 1) (1) = '-') then
                     exit;
                  end if;
                  I := I + 1; -- safe to advance
                  Config.Colors.Append (Integer'Value (Argument (I)));
               end loop;

            when Gradient    =>
               I := I + 1;
               if Argument (I) = "grayscale" then
                  Config.Colors := Grayscale;
               elsif Argument (I) = "viridis" then
                  Config.Colors := Viridis;
               else
                  raise Parse_Error
                    with
                      Command_Name
                      & ": invalid value for "
                      & Argument (I - 1)
                      & ": '"
                      & Argument (I)
                      & "'. allowed values: grayscale, viridis"
                      & LF
                      & Command_Name
                      & ": try '"
                      & Command_Name
                      & " --help' for more information";
               end if;

            when Verbose     =>
               Config.Verbose := True;

            when Dump_Config =>
               Config.Dump := True;

            when Unknown_Opt =>
               raise Parse_Error
                 with
                   Command_Name
                   & ": option '"
                   & Argument (I)
                   & "' is unknown"
                   & LF
                   & Command_Name
                   & ": try '"
                   & Command_Name
                   & " --help' for more information";
         end case;
         I := I + 1;
      end loop;
      Put_Line
        ("range:"
         & Config.Colors.First_Index'Image
         & ","
         & Config.Colors.Last_Index'Image);
   end Make_Config;
end Config;

with Ada.Text_IO; use Ada.Text_IO;

procedure Help is
begin
   Put_Line
     ("sonada: A perlin noise animation in your terminal written in Ada.");
   Put_Line ("Usage: sonada [options...]");
   Put_Line
     ("  -h, --height <height>   height of the output in number of lines");
   Put_Line
     ("  -w, --width <width>     width of the output in number of characters");
   Put_Line ("  -f, --fps <fps>         number of frames per second");
   Put_Line ("  -n, --num-frames <n>    number of frames to render");
   Put_Line ("  -z, --z-step <z>        z step between each frame");
   Put_Line ("  -c, --colors <colors>   colors used in the display");
   Put_Line ("  -h, --help              show this help");
   Put_Line ("  --dump-config           show the config and exit");
end Help;

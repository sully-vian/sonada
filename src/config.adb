with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;

package body Config is
   procedure Make_Config (Config : in out Config_Type) is
      I : Positive := 1;
   begin
      while (I <= Argument_Count) loop
         Put_Line (Integer'Image (I) & ":" & Argument (I));
         if (Argument (I) = "-h") or (Argument (I) = "--help") then
            null;

         elsif (Argument (I) = "-H") or (Argument (I) = "--height") then
            I := I + 1;
            Config.Height := Positive'Value (Argument (I));

         elsif (Argument (I) = "-W") or (Argument (I) = "--width") then
            I := I + 1;
            Config.Width := Positive'Value (Argument (I));

         elsif (Argument (I) = "-f") or (Argument (I) = "--fps") then
            I := I + 1;
            Config.FPS := Natural'Value (Argument (I));

         elsif (Argument (I) = "-n") then
            I := I + 1;
            Config.Frame_Limit := Integer'Value (Argument (I));
         elsif (Argument (I) = "-z") then
            I := I + 1;
            Config.Z_Step := Float'Value (Argument (I));

         elsif (Argument (I) = "-v") or (Argument (I) = "--verbose") then
            Config.Verbose := True;
         end if;
         I := I + 1;
      end loop;
   exception
      when others =>
         raise;
   end Make_Config;

   procedure Put (Config : in Config_Type) is
   begin
      Put_Line ("Config {");
      Put_Line ("  FPS         : " & Natural'Image (Config.FPS));
      Put_Line ("  Height      : " & Positive'Image (Config.Height));
      Put_Line ("  Width       : " & Positive'Image (Config.Width));
      Put_Line ("  Frame_Limit : " & Integer'Image (Config.Frame_Limit));
      Put_Line ("  Z_Step      : " & Float'Image (Config.Z_Step));
      Put_Line ("  Verbose     : " & Boolean'Image (Config.Verbose));
      Put_Line ("}");
   end Put;
end Config;

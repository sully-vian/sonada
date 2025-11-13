with Ada.Text_IO;                       use Ada.Text_IO;
with GNATCOLL.Terminal;                 use GNATCOLL.Terminal;
with Constants;                         use Constants;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body Lib is

    function Get_Hash (I : in Integer; J : in Integer) return Integer is
    begin
        return Permutations (((Permutations (I mod 256)) + J) mod 256);
    end;

    function Get_Angle (I : in Integer; J : in Integer) return Float is
        Hash : constant Integer := Get_Hash (I, J);
    begin
        return Float (Hash) * Inv_256 * Two_Pi;
    end;

    function Lerp
       (Start : Float; Stop : in Float; Amount : in Float) return Float is
    begin
        return (1.0 - Amount) * Start + Amount * Stop;
    end;

    function Fade (T : in Float) return Float is
    begin
        return T * T * T * (T * (T * 6.0 - 15.0 + 10.0));
    end;

    function Perlin (X : in Float; Y : in Float) return Float is
        I        : constant Integer := Integer (Float'Floor (X));
        J        : constant Integer := Integer (Float'Floor (Y));
        Xf       : constant Float := X - Float (I);
        Yf       : constant Float := Y - Float (J);
        Angle_TL : constant Float := Get_Angle (J, I); -- top-left
        Angle_TR : constant Float := Get_Angle (J, I + 1); -- top-right
        Angle_BL : constant Float := Get_Angle (J + 1, I); -- bottom-left
        Angle_BR : constant Float := Get_Angle (J + 1, I + 1); -- bottom-right
        Inf_TL   : constant Float := Xf * Cos (Angle_TL) + Yf * Sin (Angle_TL);
        Inf_TR   : constant Float :=
           (Xf - 1.0) * Cos (Angle_TR) + Yf * Sin (Angle_TR);
        Inf_BL   : constant Float :=
           Xf * Cos (Angle_BL) + (Yf - 1.0) * Sin (Angle_BL);
        Inf_BR   : constant Float :=
           (Xf - 1.0) * Cos (Angle_BR) + (Yf - 1.0) * Sin (Angle_BR);
        Fade_X   : constant Float := Fade (Xf);
        Fade_Y   : constant Float := Fade (Yf);
        Top      : constant Float := Lerp (Inf_TL, Inf_TR, Fade_X);
        Bot      : constant Float := Lerp (Inf_BL, Inf_BR, Fade_X);
    begin
        return Lerp (Top, Bot, Fade_Y);
    end;
end Lib;

with Constants; use Constants;

package body Lib is

   type Gradient is record
      X : Float;
      Y : Float;
      Z : Float;
   end record;

   -- hash(i,j,k) = perm[perm[perm[i] + j] + k]
   function Get_Hash (I, J, K : in Integer) return Integer is
   begin
      return
        Permutations
          ((Permutations ((Permutations (I mod 256) + J) mod 256) + K)
           mod 256);
   end Get_Hash;

   -- Calculates a hash value for (I,J,K), modulates it to [0,5] and gives
   -- a corresponsing iso-oriented gradient
   function Get_Gradient_3D (I, J, K : in Integer) return Gradient is
      Hash : constant Integer := Get_Hash (I, J, K);
      Grad : Gradient := (0.0, 0.0, 0.0);
   begin
      case Hash mod 6 is
         when 0 =>
            Grad.X := 1.0;

         when 1 =>
            Grad.X := -1.0;

         when 2 =>
            Grad.Y := 1.0;

         when 3 =>
            Grad.Y := -1.0;

         when 4 =>
            Grad.Z := 1.0;

         when 5 =>
            Grad.Z := -1.0;

         when others =>
            raise Constraint_Error;
      end case;
      return Grad;
   end Get_Gradient_3D;

   --     G-------------C
   --    /·            /|
   --   / ·           / |
   --  /  ·          /  |
   -- H-------------D   |       y
   -- |   E·········|···A    z__|
   -- |  ·          |  /       /
   -- | ·           | /       x
   -- |·            |/
   -- F-------------B
   function Lerp (Start, Stop, Amount : in Float) return Float is
   begin
      return (1.0 - Amount) * Start + Amount * Stop;
   end Lerp;

   function Fade (T : in Float) return Float is
   begin
      return T * T * T * (T * (T * 6.0 - 15.0) + 10.0);
   end Fade;

   function Perlin (X, Y, Z : in Float) return Float is

      -- Integer part (floor)
      I : constant Integer := Integer (Float'Floor (X));
      J : constant Integer := Integer (Float'Floor (Y));
      K : constant Integer := Integer (Float'Floor (Z));

      -- Fractional part
      Xf : constant Float := X - Float (I);
      Yf : constant Float := Y - Float (J);
      Zf : constant Float := Z - Float (K);

      -- Get gradient vectors for each cell vertex
      G_A : constant Gradient := Get_Gradient_3D (I, J, K);
      G_B : constant Gradient := Get_Gradient_3D (I + 1, J, K);
      G_C : constant Gradient := Get_Gradient_3D (I, J + 1, K);
      G_D : constant Gradient := Get_Gradient_3D (I + 1, J + 1, K);
      G_E : constant Gradient := Get_Gradient_3D (I, J, K + 1);
      G_F : constant Gradient := Get_Gradient_3D (I + 1, J, K + 1);
      G_G : constant Gradient := Get_Gradient_3D (I, J + 1, K + 1);
      G_H : constant Gradient := Get_Gradient_3D (I + 1, J + 1, K + 1);

      -- Calculate dot products (gradient·offset)
      Inf_A : constant Float := Xf * G_A.X + Yf * G_A.Y + Zf * G_A.Z;
      Inf_B : constant Float := (Xf - 1.0) * G_B.X + Yf * G_B.Y + Zf * G_B.Z;
      Inf_C : constant Float := Xf * G_C.X + (Yf - 1.0) * G_C.Y + Zf * G_C.Z;
      Inf_D : constant Float :=
        (Xf - 1.0) * G_D.X + (Yf - 1.0) * G_D.Y + Zf * G_D.Z;
      Inf_E : constant Float := Xf * G_E.X + Yf * G_E.Y + (Zf - 1.0) * G_E.Z;
      Inf_F : constant Float :=
        (Xf - 1.0) * G_F.X + Yf * G_F.Y + (Zf - 1.0) * G_F.Z;
      Inf_G : constant Float :=
        Xf * G_G.X + (Yf - 1.0) * G_G.Y + (Zf - 1.0) * G_G.Z;
      Inf_H : constant Float :=
        (Xf - 1.0) * G_H.X + (Yf - 1.0) * G_H.Y + (Zf - 1.0) * G_H.Z;

      -- Interpolation Weights
      Fade_X : constant Float := Fade (Xf);
      Fade_Y : constant Float := Fade (Yf);
      Fade_Z : constant Float := Fade (Zf);

      -- Trilinear Interpolation
      -- Along X
      Lerp_AB : constant Float := Lerp (Inf_A, Inf_B, Fade_X);
      Lerp_CD : constant Float := Lerp (Inf_C, Inf_D, Fade_X);
      Lerp_EF : constant Float := Lerp (Inf_E, Inf_F, Fade_X);
      Lerp_GH : constant Float := Lerp (Inf_G, Inf_H, Fade_X);

      -- Along Y
      Lerp_ABCD : constant Float := Lerp (Lerp_AB, Lerp_CD, Fade_Y);
      Lerp_EFGH : constant Float := Lerp (Lerp_EF, Lerp_GH, Fade_Y);
   begin
      -- Finally interpolate along Z axis
      return Lerp (Lerp_ABCD, Lerp_EFGH, Fade_Z);
   end Perlin;

end Lib;

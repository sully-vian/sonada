package Colors is
    subtype Gray_String is String (1 .. 16);
    function Grayscale (Noise : in Float; C : in Character) return Gray_String;
end Colors;

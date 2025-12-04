with Config; use Config;

generic
   Config : in  Config_Type;
package Colors is
   subtype Gray_String is String (1 .. 12);
   function Color (Noise : in Float; C : in Character) return Gray_String;
end Colors;

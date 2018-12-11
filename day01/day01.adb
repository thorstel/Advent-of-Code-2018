with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Text_IO;

use Ada.Containers;
use Ada.Text_IO;

procedure Day01 is
   package Integer_Sets    is new Ordered_Sets (Element_Type => Integer);
   package Integer_Vectors is new Vectors (Index_Type   => Natural,
                                           Element_Type => Integer);
   Frequency   : Integer := 0;
   Seen_Frequs : Integer_Sets.Set;
   Inputs      : Integer_Vectors.Vector;
begin
   -- Parsing input file & Part 1
   declare
      Input : Integer;
      File  : File_Type;
   begin
      Open (File, In_File, "input.txt");
      while not End_Of_File (File) loop
         Input     := Integer'Value (Get_Line (File));
         Frequency := Frequency + Input;
         Inputs.Append (Input);
      end loop;
      Close (File);
   end;

   Put_Line ("Part 1 =" & Integer'Image (Frequency));

   -- Part 2
   Seen_Frequs.Include (0);
   Frequency := 0;

   Infinite_Loop :
   loop
      for I in Inputs.Iterate loop
         Frequency := Frequency + Inputs (I);
         exit Infinite_Loop when Seen_Frequs.Contains (Frequency);
         Seen_Frequs.Include (Frequency);
      end loop;
   end loop Infinite_Loop;

   Put_Line ("Part 2 =" & Integer'Image (Frequency));
end Day01;

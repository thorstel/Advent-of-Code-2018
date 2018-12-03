with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Text_IO;

use Ada.Containers;
use Ada.Text_IO;

procedure Day01 is
   package Integer_Sets    is new Ordered_Sets (Element_Type => Integer);
   package Integer_Vectors is new Vectors (Index_Type   => Natural,
                                           Element_Type => Integer);
   frequency   : Integer := 0;
   seen_frequs : Integer_Sets.Set;
   inputs      : Integer_Vectors.Vector;
begin
   -- Parsing input file & Part 1
   declare
      input      : Integer;
      input_file : File_Type;
   begin
      Open (File => input_file, Mode => In_File, Name => "input.txt");
      while not End_Of_File (input_file) loop
         input     := Integer'Value (Get_Line (input_file));
         frequency := frequency + input;
         inputs.Append (input);
      end loop;
      Close (File => input_file);
   end;

   Put_Line ("Part 1 =" & Integer'Image (frequency));

   -- Part 2
   seen_frequs.Include (0);
   frequency := 0;

   Infinite_Loop :
   loop
      for i in inputs.Iterate loop
         frequency := frequency + inputs (i);
         exit Infinite_Loop when seen_frequs.Contains (frequency);
         seen_frequs.Include (frequency);
      end loop;
   end loop Infinite_Loop;

   Put_Line ("Part 2 =" & Integer'Image (frequency));
end Day01;

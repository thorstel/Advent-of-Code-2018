with Ada.Text_IO;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Vectors;

use Ada.Text_IO;

procedure Day02 is
   subtype Input_String is String (1 .. 26);

   package String_Vectors is new Ada.Containers.Vectors (Index_Type => Natural,
      Element_Type => Input_String);

   procedure String_Sort is new Ada.Containers.Generic_Array_Sort (Positive,
      Character, String);

   input_file   : File_Type;
   inputs       : String_Vectors.Vector;
   twice_count  : Integer := 0;
   thrice_count : Integer := 0;
begin
   -- Input processing + Part 1
   Open (File => input_file, Mode => In_File, Name => "input.txt");
   while not End_Of_File (input_file) loop
      declare
         line       : String    := Get_Line (input_file);
         char       : Character := ' ';
         counter    : Integer   := 0;
         has_twice  : Boolean   := False;
         has_thrice : Boolean   := False;
      begin
         inputs.Append (line);
         String_Sort (line);

         for i in line'Range loop
            if char = line (i) then
               counter := counter + 1;
            end if;

            if char /= line (i) or i = line'Last then
               if counter = 2 then
                  has_twice := True;
               elsif counter = 3 then
                  has_thrice := True;
               end if;
               counter := 1;
               char    := line (i);
            end if;
         end loop;

         if has_twice then
            twice_count := twice_count + 1;
         end if;
         if has_thrice then
            thrice_count := thrice_count + 1;
         end if;
      end;
   end loop;
   Close (File => input_file);
   Put_Line ("Part 1 =" & Integer'Image (twice_count * thrice_count));

   -- Part 2
   declare
      diff_count : Integer;
   begin
      Outer_Loop :
      for i in inputs.Iterate loop
         for j in inputs.Iterate loop
            diff_count := 0;
            for c in 1 .. 26 loop
               if inputs (i) (c) /= inputs (j) (c) then
                  diff_count := diff_count + 1;
               end if;
            end loop;
            if diff_count = 1 then
               Put_Line ("Part 2: The lines are");
               Put_Line (inputs (i) & " and");
               Put_Line (inputs (j));
               exit Outer_Loop;
            end if;
         end loop;
      end loop Outer_Loop;
   end;
end Day02;

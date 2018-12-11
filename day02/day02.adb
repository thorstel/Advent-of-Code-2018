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

   File         : File_Type;
   Inputs       : String_Vectors.Vector;
   Twice_Count  : Integer := 0;
   Thrice_Count : Integer := 0;
begin
   -- Input processing + Part 1
   Open (File, In_File, "input.txt");
   while not End_Of_File (File) loop
      declare
         Line       : String    := Get_Line (File);
         Char       : Character := ' ';
         counter    : Integer   := 0;
         Has_Twice  : Boolean   := False;
         Has_Thrice : Boolean   := False;
      begin
         Inputs.Append (Line);
         String_Sort (Line);

         for I in Line'Range loop
            if Char = Line (I) then
               Counter := Counter + 1;
            end if;

            if Char /= Line (I) or I = Line'Last then
               if Counter = 2 then
                  Has_Twice := True;
               elsif Counter = 3 then
                  Has_Thrice := True;
               end if;
               Counter := 1;
               Char    := Line (I);
            end if;
         end loop;

         if Has_Twice then
            Twice_Count := Twice_Count + 1;
         end if;
         if Has_Thrice then
            Thrice_Count := Thrice_Count + 1;
         end if;
      end;
   end loop;
   Close (File);
   Put_Line ("Part 1 =" & Integer'Image (Twice_Count * Thrice_Count));

   -- Part 2
   declare
      Diff_Count : Integer;
   begin
      Outer_Loop :
      for I in Inputs.Iterate loop
         for J in Inputs.Iterate loop
            Diff_Count := 0;
            for C in 1 .. 26 loop
               if Inputs (I) (C) /= Inputs (J) (C) then
                  Diff_Count := Diff_Count + 1;
               end if;
            end loop;
            if Diff_Count = 1 then
               Put_Line ("Part 2: The lines are");
               Put_Line (Inputs (I) & " and");
               Put_Line (Inputs (J));
               exit Outer_Loop;
            end if;
         end loop;
      end loop Outer_Loop;
   end;
end Day02;

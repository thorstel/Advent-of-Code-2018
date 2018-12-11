with Ada.Text_IO; use Ada.Text_IO;

procedure Day05 is

   function React (Unit1 : Character; Unit2 : Character) return Boolean is
      Result : Boolean := False;
   begin
      if abs (Character'Pos (Unit1) - Character'Pos (Unit2)) = 32 then
         Result := True;
      end if;
      return Result;
   end React;

   function Filter_Reactions (Polymer : String;
                              Ignore  : Character) return String
   is
      Result : String (Polymer'Range);
      Last   : Natural := Result'First - 1;
      I      : Natural := Result'First;
   begin
      while I <= Polymer'Last loop
         declare
            Keep : Boolean := True;
         begin
            if Polymer (I) = Ignore or
              abs (Character'Pos (Polymer (I)) - Character'Pos (Ignore)) = 32
            then
               I := I + 1;
            else
               if I < Polymer'Last then
                  if React (Polymer (I), Polymer (I + 1)) then
                     Keep := False;
                  end if;
               end if;

               if Keep then
                  Last          := Last + 1;
                  Result (Last) := Polymer (I);
                  I             := I + 1;
               else
                  I := I + 2;
               end if;
            end if;
         end;
      end loop;

      declare
         New_Polymer : constant String := Result (Result'First .. Last);
      begin
         if Last = Polymer'Last then
            return New_Polymer;
         else
            return Filter_Reactions (New_Polymer, Ignore);
         end if;
      end;
   end Filter_Reactions;

   Input_File : File_Type;
begin
   Open (Input_File, In_File, "input.txt");
   declare
      Input          : constant String := Get_Line (Input_File);
      Minimum_Length : Natural;
      Current_Length : Natural;
   begin
      -- Part 1
      Minimum_Length := Filter_Reactions (Input, '_')'Length;
      Put_Line ("Part 1 =" & Integer'Image (Minimum_Length));

      -- Part 2
      for C in Character'Pos ('a') .. Character'Pos ('z') loop
         Current_Length := Filter_Reactions (Input, Character'Val (C))'Length;
         if Current_Length < Minimum_Length then
            Minimum_Length := Current_Length;
         end if;
      end loop;
      Put_Line ("Part 2 =" & Integer'Image (Minimum_Length));
   end;
   Close (Input_File);
end Day05;

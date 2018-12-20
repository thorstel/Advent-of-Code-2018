with Ada.Text_IO; use Ada.Text_IO;

procedure Day20 is

   type Position is record
      X : Integer;
      Y : Integer;
   end record;

   function Next_Pos (Pos : Position; Dir : Character) return Position is
   begin
      case Dir is
         when 'N'    => return (Pos.X,     Pos.Y + 1);
         when 'S'    => return (Pos.X,     Pos.Y - 1);
         when 'W'    => return (Pos.X - 1, Pos.Y);
         when 'E'    => return (Pos.X + 1, Pos.Y);
         when others =>
            raise Constraint_Error with "Invalid direction: " & Dir;
      end case;
   end Next_Pos;

   Grid : array
     (Integer range -1000 .. 1000, Integer range -1000 .. 1000) of Natural :=
     (0      => (0      => 0,
                 others => Natural'Last),
      others => (others => Natural'Last));

   Global_Pos : Position := (0, 0);

   procedure Parse (Regex : String; Old_Pos : Position) is
      Pos     : Position := Old_Pos;
      I, J, K : Positive := Regex'First;
      Depth   : Natural  := 0;
   begin
      while I <= Regex'Last loop
         exit when Regex (I) = '(';
         declare
            New_Pos   : constant Position := Next_Pos (Pos, Regex (I));
            New_Value : constant Natural  := Grid (New_Pos.X, New_Pos.Y);
            Old_Value : constant Natural  := Grid (Pos.X, Pos.Y);
         begin
            Pos                 := New_Pos;
            Grid (Pos.X, Pos.Y) := Natural'Min (New_Value, Old_Value + 1);
         end;
         I := I + 1;
      end loop;
      Global_Pos := Pos;
      if I > Regex'Last then
         return;
      end if;

      J     := I + 1;
      Depth := 1; -- only reachable if Regex (I) = '('
      loop
         if Regex (J) = '(' then
            Depth := Depth + 1;
         elsif Regex (J) = ')' then
            Depth := Depth - 1;
         end if;
         exit when Depth = 0;
         J := J + 1;
      end loop;

      K := I;
      loop
         declare
            Sub_Regex : constant String := Regex (K + 1 .. J - 1);
         begin
            Depth := 0;
            K     := Sub_Regex'First;
            while K <= Sub_Regex'Last loop
               if Sub_Regex (K) = '(' then
                  Depth := Depth + 1;
               elsif Sub_Regex (K) = ')' then
                  Depth := Depth - 1;
               end if;
               exit when Depth = 0 and Sub_Regex (K) = '|';
               K := K + 1;
            end loop;
            Parse (Sub_Regex (Sub_Regex'First .. K - 1), Pos);
            exit when K > Sub_Regex'Last;
         end;
      end loop;

      if J < Regex'Last then
         Parse (Regex (J + 1 .. Regex'Last), Global_Pos);
      end if;
   end Parse;

   File : File_Type;
begin
   Open (File, In_File, "input.txt");
   declare
      Input : constant String := Get_Line (File);
   begin
      Parse (Input (Input'First + 1 .. Input'Last - 1), (0, 0));
   end;
   Close (File);

   declare
      Room_Count : Natural := 0;
      Max_Doors  : Natural := 0;
   begin
      for X in Grid'Range (1) loop
         for Y in Grid'Range (2) loop
            if Grid (X, Y) < Natural'Last then
               Max_Doors := Natural'Max (Max_Doors, Grid (X, Y));
               if Grid (X, Y) >= 1000 then
                  Room_Count := Room_Count + 1;
               end if;
            end if;
         end loop;
      end loop;
      Put_Line ("Part 1 =" & Natural'Image (Max_Doors));
      Put_Line ("Part 2 =" & Natural'Image (Room_Count));
   end;
end Day20;

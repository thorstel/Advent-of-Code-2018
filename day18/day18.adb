with Ada.Assertions;             use Ada.Assertions;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Text_IO;                use Ada.Text_IO;

with Ada.Strings.Hash;

procedure Day18 is

   subtype Input_String is String (1 .. 50);
   type String_Array is array (Input_String'Range) of Input_String;

   function Array_Hash (A : String_Array) return Hash_Type is
      Hash : Hash_Type := 0;
   begin
      for I in A'Range loop
         Hash := Hash + Ada.Strings.Hash (A (I));
      end loop;
      return Hash;
   end Array_Hash;

   package Array_Maps is new Hashed_Maps
     (Key_Type        => String_Array,
      Element_Type    => Natural,
      Hash            => Array_Hash,
      Equivalent_Keys => "=");

   Grid : String_Array;

   function Count_Neighbors
     (X_Start, Y_Start : Natural;
      Char             : Character) return Natural
   is
      Result : Natural := 0;
   begin
      for Y in Y_Start - 1 .. Y_Start + 1 loop
         for X in X_Start - 1 .. X_Start + 1 loop
            if (X /= X_Start or Y /= Y_Start) and
               (X >= Grid'First and X <= Grid'Last) and
               (Y >= Grid'First and Y <= Grid'Last)
            then
               if Grid (Y) (X) = Char then
                  Result := Result + 1;
               end if;
            end if;
         end loop;
      end loop;
      return Result;
   end Count_Neighbors;

   function Change_Tile (X, Y : Positive) return Character is
      Current_Tile : constant Character := Grid (Y) (X);
      New_Tile     : Character          := Current_Tile;
   begin
      if Current_Tile = '.' then
         if Count_Neighbors (X, Y, '|') >= 3 then
            New_Tile := '|';
         end if;
      elsif Current_Tile = '|' then
         if Count_Neighbors (X, Y, '#') >= 3 then
            New_Tile := '#';
         end if;
      elsif Current_Tile = '#' then
         if Count_Neighbors (X, Y, '#') = 0 or Count_Neighbors (X, Y, '|') = 0
         then
            New_Tile := '.';
         end if;
      else
         Assert (False, "Illegal tile: " & Current_Tile);
      end if;
      return New_Tile;
   end Change_Tile;

   function Change_Grid return String_Array is
      New_Grid : String_Array;
   begin
      for Y in Grid'Range loop
         for X in Grid'Range loop
            New_Grid (Y) (X) := Change_Tile (X, Y);
         end loop;
      end loop;
      return New_Grid;
   end Change_Grid;

   function Resource_Value return Natural is
      Tree_Count   : Natural := 0;
      Lumber_Count : Natural := 0;
   begin
      for I in Grid'Range loop
         for J in Grid'Range loop
            if Grid (I) (J) = '|' then
               Tree_Count := Tree_Count + 1;
            elsif Grid (I) (J) = '#' then
               Lumber_Count := Lumber_Count + 1;
            end if;
         end loop;
      end loop;
      return Tree_Count * Lumber_Count;
   end Resource_Value;
begin
   -- Input Handling
   declare
      File : File_Type;
   begin
      Open (File, In_File, "input.txt");
      for I in Grid'Range loop
         Grid (I) := Get_Line (File);
      end loop;
      Assert (End_Of_File (File), "More input than expected!");
      Close (File);
   end;

   declare
      Total      : constant Natural := 1000000000;
      Remaining  : Natural          := 0;
      Seen_Grids : Array_Maps.Map;
   begin
      Seen_Grids.Insert (Grid, 0);

      Outer_Loop :
      for I in 1 .. Total loop
         Grid := Change_Grid;

         -- Part 1
         if I = 10 then
            Put_Line ("Part 1 =" & Natural'Image (Resource_Value));
         end if;

         -- Detection of cycle
         if Seen_Grids.Contains (Grid) then
            Remaining := (Total - I) mod (I - Seen_Grids (Grid));
            exit Outer_Loop;
         else
            Seen_Grids.Insert (Grid, I);
         end if;
      end loop Outer_Loop;

      for I in 1 .. Remaining loop
         Grid := Change_Grid;
      end loop;
      Put_Line ("Part 2 =" & Natural'Image (Resource_Value));
   end;
end Day18;

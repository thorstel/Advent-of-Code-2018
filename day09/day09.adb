with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;
with Ada.Text_IO;                        use Ada.Text_IO;

procedure Day09 is
   package Natural_Lists is new Doubly_Linked_Lists (Element_Type => Natural);

   procedure Move_Cursor
     (List  : in     Natural_Lists.List;
      Pos   : in out Natural_Lists.Cursor;
      Steps : in     Integer)
   is
      function "=" (X, Y : Natural_Lists.Cursor) return Boolean
        renames Natural_Lists."=";
      Remaining_Steps : Integer := Steps;
   begin
      while Remaining_Steps /= 0 loop
         if Remaining_Steps > 0 then
            if Pos = List.Last then
               Pos := List.First;
            else
               Pos := Natural_Lists.Next (Pos);
            end if;
            Remaining_Steps := Remaining_Steps - 1;
         else
            if Pos = List.First then
               Pos := List.Last;
            else
               Pos := Natural_Lists.Previous (Pos);
            end if;
            Remaining_Steps := Remaining_Steps + 1;
         end if;
      end loop;
   end Move_Cursor;

   -- Input
   Player_Count       : constant Positive := 452;
   Last_Marble_Points : constant Natural  := 71250;

   type Big_Natural is range 0 .. (2**63 - 1);

   Max_Score     : Big_Natural := 0;
   Marble_Circle : Natural_Lists.List;
   Scores        : array (Positive range 1 .. Player_Count) of Big_Natural :=
     (others => 0);
begin
   Marble_Circle.Append (0);
   declare
      Current_Player : Positive             := 1;
      Current_Pos    : Natural_Lists.Cursor := Marble_Circle.First;
   begin
      for Marble in 1 .. (Last_Marble_Points * 100) loop
         if Marble mod 23 = 0 then
            Scores (Current_Player) :=
              Scores (Current_Player) + Big_Natural (Marble);
            Move_Cursor (Marble_Circle, Current_Pos, -7);
            Scores (Current_Player) :=
              Scores (Current_Player) +
              Big_Natural (Natural_Lists.Element (Current_Pos));
            declare
               To_Delete : Natural_Lists.Cursor := Current_Pos;
            begin
               Move_Cursor (Marble_Circle, Current_Pos, 1);
               Marble_Circle.Delete (To_Delete);
            end;
            if Scores (Current_Player) > Max_Score then
               Max_Score := Scores (Current_Player);
            end if;
         else
            Move_Cursor (Marble_Circle, Current_Pos, 2);
            Marble_Circle.Insert
              (Before   => Current_Pos,
               New_Item => Marble,
               Position => Current_Pos);
         end if;
         Current_Player := (Current_Player mod Player_Count) + 1;
         if Marble = Last_Marble_Points then
            Put_Line ("Part 1 =" & Big_Natural'Image (Max_Score));
         end if;
      end loop;
   end;
   Put_Line ("Part 2 =" & Big_Natural'Image (Max_Score));
end Day09;

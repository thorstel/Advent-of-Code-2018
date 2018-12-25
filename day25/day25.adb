with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO;    use Ada.Text_IO;

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

procedure Day25 is
   generic
      type T is private;
      with function "<" (Left, Right : T) return Boolean;
   package Disjoint_Sets is
      procedure Insert (Element : T);
      procedure Make_Union (Element1, Element2 : T);
      Number_Of_Sets : Natural := 0;
   private
      type Set_Element is record
         ID   : T;
         Rank : Natural;
         Size : Natural;
      end record;

      function Find (Element : T) return Set_Element;

      package PM is new Ada.Containers.Ordered_Maps
        (Key_Type     => T,
         Element_Type => Set_Element);
      Table : PM.Map;
   end Disjoint_Sets;

   package body Disjoint_Sets is
      function Find (Element : T) return Set_Element is
         Set : Set_Element := (Element, 0, 0);
      begin
         if not Table.Contains (Element) then
            Number_Of_Sets := Number_Of_Sets + 1;
            Set.Size       := 1;
            Table.Insert (Element, Set);
            return Set;
         else
            Set := Table (Element);
            if Set.ID = Element then
               return Set;
            else
               declare
                  Parent : constant Set_Element := Find (Set.ID);
                  ID     : constant T           := Set.ID;
               begin
                  Set.ID := Parent.ID;
                  Table.Replace (ID, Set);
                  return Parent;
               end;
            end if;
         end if;
      end Find;

      procedure Insert (Element : T) is
         Set : constant Set_Element := Find (Element);
      begin
         Assert (Set.Size > 0);
      end Insert;

      procedure Make_Union (Element1, Element2 : T) is
         Set1 : Set_Element := Find (Element1);
         Set2 : Set_Element := Find (Element2);
         ID1  : constant T  := Set1.ID;
         ID2  : constant T  := Set2.ID;
      begin
         if Set1.ID = Set2.ID then
            return;
         end if;
         Number_Of_Sets := Number_Of_Sets - 1;
         if (Set1.Rank < Set2.Rank) then
            Set1.ID   := Set2.ID;
            Set2.Size := Set2.Size + Set1.Size;
         else
            Set2.ID   := Set1.ID;
            Set1.Size := Set1.Size + Set2.Size;
            if Set1.Rank = Set2.Rank then
               Set1.Rank := Set1.Rank + 1;
            end if;
         end if;
         Table.Replace (ID1, Set1);
         Table.Replace (ID2, Set2);
      end Make_Union;
   end Disjoint_Sets;

   type Point_Type is array (Positive range 1 .. 4) of Integer;

   function Manhattan_Distance (P1, P2 : Point_Type) return Natural is
      Dist : Natural := 0;
   begin
      for I in Point_Type'Range loop
         Dist := Dist + abs (P1 (I) - P2 (I));
      end loop;
      return Dist;
   end Manhattan_Distance;

   package PV is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Point_Type);
   package DS is new Disjoint_Sets (T => Point_Type, "<" => "<");

   Points : PV.Vector;
   Input  : File_Type;
begin
   Open (Input, In_File, "input.txt");
   while not End_Of_File (Input) loop
      declare
         Line  : constant String := Get_Line (Input);
         Point : Point_Type;
         P     : Positive        := Point'First;
         F, L  : Positive        := Line'First;
      begin
         while L <= Line'Last loop
            if Line (L) = ',' or L = Line'Last then
               if L = Line'Last then
                  Point (P) := Integer'Value (Line (F .. L));
               else
                  Point (P) := Integer'Value (Line (F .. L - 1));
               end if;
               P := P + 1;
               F := L + 1;
            end if;
            L := L + 1;
         end loop;
         Assert (P = Point_Type'Last + 1, "Invalid Input");
         DS.Insert (Point);
         for Point2 of Points loop
            if Manhattan_Distance (Point, Point2) <= 3 then
               DS.Make_Union (Point, Point2);
            end if;
         end loop;
         Points.Append (Point);
      end;
   end loop;
   Close (Input);
   Put_Line ("Number of constellations =" & Natural'Image (DS.Number_Of_Sets));
end Day25;

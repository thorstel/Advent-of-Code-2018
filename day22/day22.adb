with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO;    use Ada.Text_IO;

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;

procedure Day22 is
   Max_Dim : constant Natural := 800;

   Grid : array
     (Natural range 0 .. Max_Dim, Natural range 0 .. Max_Dim) of Natural :=
     (others => (others => 0));

   Depth    : Natural;
   Target_X : Natural;
   Target_Y : Natural;

   function Geological_Index (X, Y : Natural) return Natural is
   begin
      if X = Target_X and Y = Target_Y then
         return 0;
      elsif Y = 0 then
         return X * 16807;
      elsif X = 0 then
         return Y * 48271;
      else
         return Grid (X - 1, Y) * Grid (X, Y - 1);
      end if;
   end Geological_Index;

   function Erosion_Level (X, Y : Natural) return Natural is
     ((Geological_Index (X, Y) + Depth) mod 20183);

   File : File_Type;
begin
   -- Input Handling
   Open (File, In_File, "input.txt");
   declare
      Line1 : constant String := Get_Line (File);
      Line2 : constant String := Get_Line (File);
      Comma : Positive        := Line2'First;
   begin
      Depth := Natural'Value (Line1 (8 .. Line1'Last));
      while Comma <= Line2'Last loop
         exit when Line2 (Comma) = ',';
         Comma := Comma + 1;
      end loop;
      Assert (Comma < Line2'Last, "Invalid input!");
      Target_X := Natural'Value (Line2 (9 .. Comma - 1));
      Target_Y := Natural'Value (Line2 (Comma + 1 .. Line2'Last));
   end;
   Close (File);

   -- Calculating the erosion levels for the grid.
   for Y in Grid'Range (2) loop
      for X in Grid'Range (1) loop
         Grid (X, Y) := Erosion_Level (X, Y);
      end loop;
   end loop;

   -- Changing grid from erosion Levels to terrain & part 1
   declare
      Risk_Level : Natural := 0;
   begin
      for Y in Grid'Range (2) loop
         for X in Grid'Range (1) loop
            Grid (X, Y) := Grid (X, Y) mod 3;
            if X <= Target_X and Y <= Target_Y then
               Risk_Level := Risk_Level + Grid (X, Y);
            end if;
         end loop;
      end loop;
      Put_Line ("Part 1 =" & Natural'Image (Risk_Level));
   end;

   -- Part 2
   Dijkstra_Algorithm :
   declare
      type Tool_Type is (Torch, Climbing_Gear, Neither);

      type Position is record
         X : Integer;
         Y : Integer;
      end record;

      type Node_Type is record
         Visited  : Boolean;
         Distance : Natural;
      end record;

      type Queue_Element is record
         X        : Natural;
         Y        : Natural;
         Tool     : Tool_Type;
         Distance : Natural;
      end record;

      function Can_Use (Tool : Tool_Type; Terrain : Natural) return Boolean is
      begin
         case Terrain is
            when 0      => return Tool = Climbing_Gear or Tool = Torch;
            when 1      => return Tool = Climbing_Gear or Tool = Neither;
            when 2      => return Tool = Torch         or Tool = Neither;
            when others =>
               raise Constraint_Error
                 with "Illegal terrain type:" & Natural'Image (Terrain);
         end case;
      end Can_Use;

      function Get_Priority (Element : Queue_Element) return Natural is
        (Element.Distance);

      function Before (Left, Right : Natural) return Boolean is (Left < Right);

      package QI is new Ada.Containers.Synchronized_Queue_Interfaces
        (Element_Type => Queue_Element);

      package PQ is new Ada.Containers.Unbounded_Priority_Queues
        (Queue_Interfaces => QI, Queue_Priority => Natural);
      use PQ;
      use Ada.Containers;

      Directions : constant array (Positive range <>) of Position :=
        ((1, 0), (-1, 0), (0, 1), (0, -1));

      Nodes : array
        (Natural range 0 .. Max_Dim,
         Natural range 0 .. Max_Dim,
         Tool_Type'Range) of Node_Type :=
        (others => (others => (others => (False, 0))));

      E : Queue_Element;
      Q : Queue;
   begin
      Q.Enqueue ((0, 0, Torch, 0)); -- Start with torch equipped
      while Q.Current_Use > 0 and not Nodes (Target_X, Target_Y, Torch).Visited
      loop
         Q.Dequeue (E);
         if not Nodes (E.X, E.Y, E.Tool).Visited then
            Nodes (E.X, E.Y, E.Tool).Visited  := True;
            Nodes (E.X, E.Y, E.Tool).Distance := E.Distance;
            for Dir of Directions loop
               if (E.X > 0       or Dir.X >= 0) and
                  (E.Y > 0       or Dir.Y >= 0) and
                  (E.X < Max_Dim or Dir.X <= 0) and
                  (E.Y < Max_Dim or Dir.Y <= 0)
               then
                  declare
                     New_X : constant Natural := E.X + Dir.X;
                     New_Y : constant Natural := E.Y + Dir.Y;
                  begin
                     if Can_Use (E.Tool, Grid (New_X, New_Y)) and
                        not Nodes (New_X, New_Y, E.Tool).Visited
                     then
                        Q.Enqueue ((New_X, New_Y, E.Tool, E.Distance + 1));
                     end if;
                  end;
               end if;
            end loop;
            for Tool in Tool_Type'Range loop
               if not Nodes (E.X, E.Y, Tool).Visited and
                  Can_Use (Tool, Grid (E.X, E.Y))
               then
                  Q.Enqueue ((E.X, E.Y, Tool, E.Distance + 7));
               end if;
            end loop;
         end if;
      end loop;
      Put_Line
        ("Part 2 =" &
         Natural'Image (Nodes (Target_X, Target_Y, Torch).Distance));
   end Dijkstra_Algorithm;
end Day22;

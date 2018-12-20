with Ada.Containers.Hashed_Sets,
     Ada.Containers.Ordered_Sets,
     Ada.Containers.Ordered_Maps,
     Ada.Containers.Vectors;      use Ada.Containers;
with Ada.Text_IO;                 use Ada.Text_IO;

procedure Day07 is

   type Edge_Record is record
      From : Character;
      To   : Character;
   end record;

   function Edge_Hash (Edge : Edge_Record) return Hash_Type
   is (Character'Pos (Edge.From) + Character'Pos (Edge.To));

   function Edge_Equal (X, Y : Edge_Record) return Boolean
   is (X.From = Y.From and X.To = Y.To);

   package Edge_Sets is new Hashed_Sets
     (Element_Type        => Edge_Record,
      Hash                => Edge_Hash,
      Equivalent_Elements => Edge_Equal);

   package Node_Sets is new Ordered_Sets (Element_Type => Character);

   package Node_Maps is new Ordered_Maps
     (Key_Type     => Character,
      Element_Type => Natural);

   package Node_Vectors is new Vectors
     (Index_Type   => Natural,
      Element_Type => Character);

   procedure Topological_Sort
     (Incoming_Edges    : in Node_Maps.Map;
      No_Incoming_Edges : in Node_Sets.Set;
      All_Edges         : in Edge_Sets.Set)
   is
      Incoming      : Node_Maps.Map := Incoming_Edges;
      None_Incoming : Node_Sets.Set := No_Incoming_Edges;
      Graph_Edges   : Edge_Sets.Set := All_Edges;
      Node          : Character;
      Result        : Node_Vectors.Vector;
   begin
      while not None_Incoming.Is_Empty loop
         Node := None_Incoming.First_Element;
         None_Incoming.Delete (Node);
         Result.Append (Node);
         for M in Incoming.Iterate loop
            if Graph_Edges.Contains ((Node, Node_Maps.Key (M))) then
               Graph_Edges.Delete ((Node, Node_Maps.Key (M)));
               if Node_Maps.Element (M) = 1 then
                  None_Incoming.Insert (Node_Maps.Key (M));
               else
                  Incoming.Replace_Element (M, Node_Maps.Element (M) - 1);
               end if;
            end if;
         end loop;
      end loop;
      Put ("Part 1 = ");
      for C of Result loop
         Put ("" & C);
      end loop;
      Put_Line ("");
   end Topological_Sort;

   Incoming_Edges    : Node_Maps.Map;
   No_Incoming_Edges : Node_Sets.Set;
   All_Edges         : Edge_Sets.Set;
   File              : File_Type;
begin
   -- Input Setup
   Open (File, In_File, "input.txt");
   while not End_Of_File (File) loop
      declare
         Line : constant String      := Get_Line (File);
         Edge : constant Edge_Record := (Line (6), Line (37));
      begin
         All_Edges.Insert (Edge);
         if not Incoming_Edges.Contains (Edge.To) then
            Incoming_Edges.Insert (Edge.To, 1);
         else
            Incoming_Edges.Replace
              (Edge.To, Incoming_Edges.Element (Edge.To) + 1);
         end if;
      end;
   end loop;
   Close (File);

   for I in Character'Pos ('A') .. Character'Pos ('Z') loop
      if not Incoming_Edges.Contains (Character'Val (I)) then
         No_Incoming_Edges.Insert (Character'Val (I));
      end if;
   end loop;

   -- Part 1 (TopoSort)
   Topological_Sort (Incoming_Edges, No_Incoming_Edges, All_Edges);

   -- Part 2
   declare
      function Task_Time (Node : Character) return Natural
      is (60 + Character'Pos (Node) - Character'Pos ('A') + 1);

      type Worker_Type is record
         Remaining_Time : Natural;
         Current_Task   : Character;
      end record;

      Workers : array (Positive range 1 .. 5) of Worker_Type :=
        (others => (0, '_'));

      Total_Time       : Natural := 0;
      Work_In_Progress : Boolean := True;
      Node             : Character;
   begin
      while not No_Incoming_Edges.Is_Empty or Work_In_Progress loop
         for Worker of Workers loop
            if Worker.Remaining_Time = 0 and not No_Incoming_Edges.Is_Empty
            then
               Node := No_Incoming_Edges.First_Element;
               No_Incoming_Edges.Delete (Node);
               Worker.Remaining_Time := Task_Time (Node);
               Worker.Current_Task   := Node;
            end if;
         end loop;

         Work_In_Progress := False;

         for Worker of Workers loop
            if Worker.Remaining_Time = 1 then
               for Dst in Incoming_Edges.Iterate loop
                  if All_Edges.Contains ((Worker.Current_Task,
                                          Node_Maps.Key (Dst)))
                  then
                     All_Edges.Delete ((Worker.Current_Task,
                                        Node_Maps.Key (Dst)));
                     if Node_Maps.Element (Dst) = 1 then
                        No_Incoming_Edges.Insert (Node_Maps.Key (Dst));
                     else
                        Incoming_Edges.Replace_Element
                          (Dst, Node_Maps.Element (Dst) - 1);
                     end if;
                  end if;
               end loop;
            end if;

            if Worker.Remaining_Time > 0 then
               Worker.Remaining_Time := Worker.Remaining_Time - 1;
            end if;

            if Worker.Remaining_Time > 0 then
               Work_In_Progress := True;
            end if;
         end loop;

         Total_Time := Total_Time + 1;
      end loop;

      Put_Line ("Part 2 =" & Natural'Image (Total_Time));
   end;
end Day07;

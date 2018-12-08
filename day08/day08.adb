with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Text_IO;            use Ada.Text_IO;

procedure Day08 is
   package Natural_Vectors is new Vectors
     (Index_Type   => Positive,
      Element_Type => Natural);

   function Metadata_Sum
     (Input    : in     Natural_Vectors.Vector;
      Index    : in out Positive;
      Is_Part2 : in     Boolean) return Natural
   is
      Sum            : Natural := 0;
      Child_Nodes    : Natural := Input (Index);
      Metadata_Count : Natural := Input (Index + 1);
      Child_Sums     : array (Positive range 1 .. Child_Nodes) of Natural :=
        (others => 0);
   begin
      Index := Index + 2;
      for I in 1 .. Child_Nodes loop
         Child_Sums (I) := Metadata_Sum (Input, Index, Is_Part2);
         if not Is_Part2 then
            Sum := Sum + Child_Sums (I);
         end if;
      end loop;
      for I in 1 .. Metadata_Count loop
         if Is_Part2 and Child_Nodes > 0 then
            if Input (Index) >= Child_Sums'First and
               Input (Index) <= Child_Sums'Last
            then
               Sum := Sum + Child_Sums (Input (Index));
            end if;
         else
            Sum := Sum + Input (Index);
         end if;
         Index := Index + 1;
      end loop;
      return Sum;
   end Metadata_Sum;

   File   : File_Type;
   Inputs : Natural_Vectors.Vector;
begin
   -- Input Processing
   Open (File, In_File, "input.txt");
   declare
      Input : String  := Get_Line (File);
      I, J  : Natural := Input'First;
   begin
      while J < Input'Last loop
         J := J + 1;
         if Input (J) = ' ' or J = Input'Last then
            Inputs.Append (Natural'Value (Input (I .. J)));
            I := J;
         end if;
      end loop;
   end;
   Close (File);

   -- Part 1 and 2
   declare
      Index : Positive := Inputs.First_Index;
      Sum   : Natural  := Metadata_Sum (Inputs, Index, False);
   begin
      Put_Line ("Part 1 =" & Natural'Image (Sum));
      Index := Inputs.First_Index;
      Sum   := Metadata_Sum (Inputs, Index, True);
      Put_Line ("Part 2 =" & Natural'Image (Sum));
   end;
end Day08;

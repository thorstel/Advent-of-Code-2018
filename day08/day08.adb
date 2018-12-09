with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Text_IO;            use Ada.Text_IO;

procedure Day08 is
   type Sum_Pair is record
      Part1 : Natural;
      Part2 : Natural;
   end record;

   package Natural_Vectors is new Vectors
     (Index_Type   => Positive,
      Element_Type => Natural);

   function Metadata_Sums
     (Input : in     Natural_Vectors.Vector;
      Index : in out Positive) return Sum_Pair
   is
      Sums           : Sum_Pair := (0, 0);
      Child_Nodes    : Natural  := Input (Index);
      Metadata_Count : Natural  := Input (Index + 1);
      Child_Sums     : array (Positive range 1 .. Child_Nodes) of Sum_Pair :=
        (others => (0, 0));
   begin
      Index := Index + 2;
      for I in 1 .. Child_Nodes loop
         Child_Sums (I) := Metadata_Sums (Input, Index);
         Sums.Part1     := Sums.Part1 + Child_Sums (I).Part1;
      end loop;
      for I in 1 .. Metadata_Count loop
         if Child_Nodes > 0 then
            if Input (Index) >= Child_Sums'First and
               Input (Index) <= Child_Sums'Last
            then
               Sums.Part2 := Sums.Part2 + Child_Sums (Input (Index)).Part2;
            end if;
         else
            Sums.Part2 := Sums.Part2 + Input (Index);
         end if;
         Sums.Part1 := Sums.Part1 + Input (Index);
         Index      := Index + 1;
      end loop;
      return Sums;
   end Metadata_Sums;

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
      Sums  : Sum_Pair := Metadata_Sums (Inputs, Index);
   begin
      Put_Line ("Part 1 =" & Natural'Image (Sums.Part1));
      Put_Line ("Part 2 =" & Natural'Image (Sums.Part2));
   end;
end Day08;

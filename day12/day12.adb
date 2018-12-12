with Ada.Containers.Ordered_Maps; use Ada.Containers;
with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Input12;                     use Input12;

procedure Day12 is
   package String_Maps is new Ordered_Maps
     (Key_Type     => String_Pattern,
      Element_Type => Character);

   type Integer_64 is range -(2**63) .. +(2**63 - 1);

   Lookup_Table : String_Maps.Map;

   function Generation_Sum
     (Initial_State : String;
      Num_Gens      : Integer_64) return Integer_64
   is
      Diff_Count    : Natural          := 0;
      Old_Sum       : Integer_64       := 0;
      Old_Diff      : Integer_64       := 0;
      Zero_Index    : Positive         := Initial_State'First + 5;
      Current_State : Unbounded_String :=
        To_Unbounded_String ("....." & Initial_State & ".....");
   begin
      for Gen in 1 .. Num_Gens loop
         declare
            Old_State : constant String := To_String (Current_State);
            New_State : String          := Old_State;
            Sum       : Integer_64      := 0;
         begin
            for I in Old_State'First + 2 .. Old_State'Last - 2 loop
               declare
                  Pattern : constant String := Old_State (I - 2 .. I + 2);
               begin
                  New_State (I) := Lookup_Table.Element (Pattern);
                  if New_State (I) = '#' then
                     Sum := Sum + Integer_64 (I - Zero_Index);
                  end if;
               end;
            end loop;

            if Old_Diff = (Sum - Old_Sum) then
               Diff_Count := Diff_Count + 1;
            else
               Diff_Count := 0;
            end if;
            if Diff_Count > 10 then
               return Sum + ((Num_Gens - Gen) * (Sum - Old_Sum));
            end if;

            Old_Diff      := Sum - Old_Sum;
            Old_Sum       := Sum;
            Current_State := To_Unbounded_String (New_State);

            if New_State (1 .. 5) /= "....." then
               Current_State := "....." & Current_State;
               Zero_Index    := Zero_Index + 5;
            end if;
            if New_State (New_State'Last - 4 .. New_State'Last) /= "....." then
               Append (Current_State, ".....");
            end if;
         end;
      end loop;
      return Old_Sum;
   end Generation_Sum;
begin
   for I in Inputs'Range loop
      Lookup_Table.Insert (Inputs (I).Pattern, Inputs (I).Plant);
   end loop;

   Put_Line
     ("Part 1 =" & Integer_64'Image (Generation_Sum (Initial_State, 20)));
   Put_Line
     ("Part 2 =" &
      Integer_64'Image (Generation_Sum (Initial_State, 50000000000)));
end Day12;

with Ada.Assertions;              use Ada.Assertions;
with Ada.Containers.Ordered_Maps; use Ada.Containers;
with Ada.Text_IO;                 use Ada.Text_IO;
with Input;                       use Input;

procedure Day04 is

   type Hour is array (Natural range 0 .. 59) of Natural;

   package Time_Maps is new Ordered_Maps (Key_Type     => Natural,
                                          Element_Type => Hour);

   Time_Table              : Time_Maps.Map;
   Most_Minutes_Guard      : Natural;
   Most_Minutes_Max_Minute : Natural;
   Most_Minutes_Count      : Natural := 0;
   Most_In_Minute_Guard    : Natural;
   Most_In_Minute_Count    : Natural := 0;
   Most_In_Minute          : Natural := 0;
begin
   -- Input Setup
   declare
      Current_Guard : Natural := 0;
      Sleep_Start   : Natural range 0 .. 59;
   begin
      for Repose_Record of Repose_Records loop
         if Repose_Record.Action = Shift_Start then
            Current_Guard := Repose_Record.Data;
         elsif Repose_Record.Action = Fall_Asleep then
            Sleep_Start := Repose_Record.Data;
         elsif Repose_Record.Action = Wake_Up then
            if not Time_Table.Contains (Current_Guard) then
               Time_Table.Insert (Current_Guard, (others => 0));
            end if;

            declare
               Current_Hour : Hour := Time_Table.Element (Current_Guard);
            begin
               for S in Sleep_Start .. (Repose_Record.Data - 1) loop
                  Current_Hour (S) := Current_Hour (S) + 1;
               end loop;
               Time_Table.Replace (Current_Guard, Current_Hour);
            end;
         else
            Assert (False, "This should not happen!");
         end if;
      end loop;
   end;

   -- Part 1 & 2
   for C in Time_Table.Iterate loop
      declare
         Max_Minute   : Natural := 0;
         Minute_Count : Natural := 0;
      begin
         for I in 0 .. 59 loop
            Minute_Count := Minute_Count + Time_Maps.Element (C) (I);
            if Time_Maps.Element (C) (I) > Time_Maps.Element (C) (Max_Minute) then
               Max_Minute := I;
            end if;

            if Time_Maps.Element (C) (I) > Most_In_Minute_Count then
               Most_In_Minute       := I;
               Most_In_Minute_Guard := Time_Maps.Key (C);
               Most_In_Minute_Count := Time_Maps.Element (C) (I);
            end if;
         end loop;

         if Minute_Count > Most_Minutes_Count then
            Most_Minutes_Guard      := Time_Maps.Key (C);
            Most_Minutes_Count      := Minute_Count;
            Most_Minutes_Max_Minute := Max_Minute;
         end if;
      end;
   end loop;

   Put_Line ("Part 1 =" & Natural'Image (Most_Minutes_Guard * Most_Minutes_Max_Minute));
   Put_Line ("Part 2 =" & Natural'Image (Most_In_Minute_Guard * Most_In_Minute));
end Day04;

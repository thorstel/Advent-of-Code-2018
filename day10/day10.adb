with Ada.Text_IO; use Ada.Text_IO;
with Input;       use Input;

procedure Day10 is
   procedure Print_Grid
     (Points                     : Point_Array;
      Min_X, Max_X, Min_Y, Max_Y : Integer)
   is
      Grid : array
        (Integer range Min_X .. Max_X,
         Integer range Min_Y .. Max_Y) of Character :=
        (others => (others => '.'));
   begin
      for I in Points'Range loop
         Grid (Points (I).X, Points (I).Y) := '#';
      end loop;
      for Y in Min_Y .. Max_Y loop
         for X in Min_X .. Max_X loop
            Put ("" & Grid (X, Y));
         end loop;
         Put_Line ("");
      end loop;
   end Print_Grid;

   Seconds : Natural := 0;
begin
   -- Assumption: The points are converging until the message appears
   -- and then diverge again. Meaning the diameter on the Y-axis should
   -- increase for the first time directly after the message is shown.
   Infinite_Loop :
   loop
      declare
         Min_X        : Integer := Integer'Last;
         Max_X        : Integer := Integer'First;
         Min_Y_Before : Integer := Integer'Last;
         Max_Y_Before : Integer := Integer'First;
         Min_Y_After  : Integer := Integer'Last;
         Max_Y_After  : Integer := Integer'First;
      begin
         for I in Points'Range loop
            Min_X        := Integer'Min (Min_X, Points (I).X);
            Max_X        := Integer'Max (Max_X, Points (I).X);
            Min_Y_Before := Integer'Min (Min_Y_Before, Points (I).Y);
            Max_Y_Before := Integer'Max (Max_Y_Before, Points (I).Y);

            Points (I) := Points (I) + Velocities (I);

            Min_Y_After := Integer'Min (Min_Y_After, Points (I).Y);
            Max_Y_After := Integer'Max (Max_Y_After, Points (I).Y);
         end loop;

         if abs (Max_Y_After - Min_Y_After) > abs (Max_Y_Before - Min_Y_Before)
         then
            -- Roll-back the last step
            for I in Points'Range loop
               Points (I) := Points (I) - Velocities (I);
            end loop;

            -- Print the result
            Put_Line ("Part 1:");
            Print_Grid (Points, Min_X, Max_X, Min_Y_Before, Max_Y_Before);
            Put_Line ("Part 2 =" & Natural'Image (Seconds));
            exit Infinite_Loop;
         end if;
         Seconds := Seconds + 1;
      end;
   end loop Infinite_Loop;
end Day10;

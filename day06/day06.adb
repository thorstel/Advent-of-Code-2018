with Ada.Text_IO; use Ada.Text_IO;

procedure Day06 is

   type Coordinate is record
      X : Integer;
      Y : Integer;
   end record;

   function Manhattan_Distance (A : Coordinate; B : Coordinate) return Integer
   is (abs (B.X - A.X) + abs (B.Y - A.Y));

   -- Input coordinates
   Coordinates : constant array (Positive range <>) of Coordinate :=
     ((137, 282), (229, 214), (289, 292), (249, 305), (90,  289), (259, 316),
      (134, 103), (96,  219), (92,  308), (269, 59),  (141, 132), (71,  200),
      (337, 350), (40,  256), (236, 105), (314, 219), (295, 332), (114, 217),
      (43,  202), (160, 164), (245, 303), (339, 277), (310, 316), (164, 44),
      (196, 335), (228, 345), (41,  49),  (84,  298), (43,  51),  (158, 347),
      (121, 51),  (176, 187), (213, 120), (174, 133), (259, 263), (210, 205),
      (303, 233), (265, 98),  (359, 332), (186, 340), (132, 99),  (174, 153),
      (206, 142), (341, 162), (180, 166), (152, 249), (221, 118), (95,  227),
      (152, 186), (72,  330));

   Closest_Count : array (Coordinates'Range) of Integer := (others => 0);

   X_Begin : Integer := 1000;
   X_End   : Integer := -1000;
   Y_Begin : Integer := 1000;
   Y_End   : Integer := -1000;

begin
   -- Determine the borders of the Grid.
   for C of Coordinates loop
      if C.X < X_Begin then
         X_Begin := C.X;
      end if;
      if C.X > X_End then
         X_End := C.X;
      end if;
      if C.Y < Y_Begin then
         Y_Begin := C.Y;
      end if;
      if C.Y > Y_End then
         Y_End := C.Y;
      end if;
   end loop;

   -- Part 1
   for X in X_Begin .. X_End loop
      for Y in Y_Begin .. Y_End loop
         declare
            Min_Distance : Integer := 9999;
            Min_Index    : Positive;
            Min_Count    : Natural;
            Tmp_Distance : Integer;
         begin
            for I in Coordinates'Range loop
               Tmp_Distance := Manhattan_Distance (Coordinates (I), (X, Y));
               if Tmp_Distance < Min_Distance then
                  Min_Distance := Tmp_Distance;
                  Min_Count    := 1;
                  Min_Index    := I;
               elsif Tmp_Distance = Min_Distance then
                  Min_Count := Min_Count + 1;
               end if;
            end loop;

            -- Coordinate must not be on the border of the grid.
            if X = X_Begin or X = X_End or Y = Y_Begin or Y = Y_End then
               Closest_Count (Min_Index) := -1;
            elsif Min_Count = 1 and Closest_Count (Min_Count) >= 0 then
               Closest_Count (Min_Index) := Closest_Count (Min_Index) + 1;
            end if;
         end;
      end loop;
   end loop;

   declare
      Max_Count : Natural := 0;
   begin
      for I in Coordinates'Range loop
         if Closest_Count (I) > Max_Count then
            Max_Count := Closest_Count (I);
         end if;
      end loop;
      Put_Line ("Part 1 =" & Natural'Image (Max_Count));
   end;

   -- Part 2
   declare
      Region_Size : Natural := 0;
      Sum         : Integer;
   begin
      for X in X_Begin .. X_End loop
         for Y in Y_Begin .. Y_End loop
            Sum := 0;
            for C of Coordinates loop
               Sum := Sum + Manhattan_Distance ((X, Y), C);
            end loop;
            if Sum < 10000 then
               Region_Size := Region_Size + 1;
            end if;
         end loop;
      end loop;
      Put_Line ("Part 2 =" & Natural'Image (Region_Size));
   end;
end Day06;

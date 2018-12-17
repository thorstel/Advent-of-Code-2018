with Ada.Text_IO; use Ada.Text_IO;
with Input17;     use Input17;

procedure Day17 is

   Grid : array
     (Natural range 0 .. 2000, Natural range 0 .. 2000) of Character :=
     (others => (others => '.'));

   Min_X, Min_Y : Natural := Natural'Last;
   Max_X, Max_Y : Natural := Natural'First;

   function Can_Spread (X, Y : Natural) return Boolean is
     (Grid (X, Y) = '.' or Grid (X, Y) = '|');

   procedure Spread_Water (X, Y : Natural) is
   begin
      if Y > Max_Y then
         return;
      end if;
      if not Can_Spread (X, Y) then
         return;
      end if;
      if not Can_Spread (X, Y + 1) then
         declare
            Left_X  : Natural := X;
            Right_X : Natural := X + 1;
         begin
            while Can_Spread (Left_X, Y) and not Can_Spread (Left_X, Y + 1)
            loop
               Grid (Left_X, Y) := '|';
               Left_X           := Left_X - 1;
            end loop;
            while Can_Spread (Right_X, Y) and not Can_Spread (Right_X, Y + 1)
            loop
               Grid (Right_X, Y) := '|';
               Right_X           := Right_X + 1;
            end loop;
            if Can_Spread (Left_X, Y + 1) or Can_Spread (Right_X, Y + 1) then
               Spread_Water (Left_X, Y);
               Spread_Water (Right_X, Y);
            elsif Grid (Left_X, Y) = '#' and Grid (Right_X, Y) = '#' then
               for X2 in Left_X + 1 .. Right_X - 1 loop
                  Grid (X2, Y) := '~';
               end loop;
            end if;
         end;
      elsif Grid (X, Y) = '.' then
         Grid (X, Y) := '|';
         Spread_Water (X, Y + 1);
         if Grid (X, Y + 1) = '~' then
            Spread_Water (X, Y);
         end if;
      end if;
   end Spread_Water;

begin
   for I in X_Inputs'Range loop
      Min_X := Natural'Min (Min_X, X_Inputs (I).Fixed);
      Max_X := Natural'Max (Max_X, X_Inputs (I).Fixed);
      Min_Y := Natural'Min (Min_Y, X_Inputs (I).First);
      Max_Y := Natural'Max (Max_Y, X_Inputs (I).Last);
   end loop;
   for I in Y_Inputs'Range loop
      Min_Y := Natural'Min (Min_Y, Y_Inputs (I).Fixed);
      Max_Y := Natural'Max (Max_Y, Y_Inputs (I).Fixed);
      Min_X := Natural'Min (Min_X, Y_Inputs (I).First);
      Max_X := Natural'Max (Max_X, Y_Inputs (I).Last);
   end loop;

   for I in X_Inputs'Range loop
      declare
         X : constant Natural := X_Inputs (I).Fixed;
      begin
         for Y in X_Inputs (I).First .. X_Inputs (I).Last loop
            Grid (X, Y) := '#';
         end loop;
      end;
   end loop;
   for I in Y_Inputs'Range loop
      declare
         Y : constant Natural := Y_Inputs (I).Fixed;
      begin
         for X in Y_Inputs (I).First .. Y_Inputs (I).Last loop
            Grid (X, Y) := '#';
         end loop;
      end;
   end loop;

   Spread_Water (500, Min_Y);

   declare
      Water_Tiles : Natural := 0;
      Perma_Water : Natural := 0;
   begin
      for Y in Min_Y .. Max_Y loop
         for X in 0 .. 2000 loop
            if Grid (X, Y) = '~' then
               Perma_Water := Perma_Water + 1;
               Water_Tiles := Water_Tiles + 1;
            elsif Grid (X, Y) = '|' then
               Water_Tiles := Water_Tiles + 1;
            end if;
         end loop;
      end loop;
      Put_Line ("Part 1 =" & Natural'Image (Water_Tiles));
      Put_Line ("Part 2 =" & Natural'Image (Perma_Water));
   end;

   -- Write the whole grid to a file (just for fun).
   declare
      File : File_Type;
   begin
      Create (File, Out_File, "grid.out");
      for Y in Min_Y .. Max_Y loop
         for X in Min_X .. Max_X loop
            Put (File, Grid (X, Y));
         end loop;
         Put_Line (File, "");
      end loop;
      Close (File);
   end;
end Day17;

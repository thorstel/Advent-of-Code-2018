with Ada.Text_IO; use Ada.Text_IO;

-- https://en.wikipedia.org/wiki/Summed-area_table
procedure Day11 is

   -- Puzzle Input
   Serial_Number : constant Integer := 9445;

   function Power_Level (X, Y : Integer) return Integer is
     (((((((X + 10) * Y) + Serial_Number) * (X + 10)) / 100) mod 10) - 5);

   Sums : array (Natural range 0 .. 300, Natural range 0 .. 300) of Integer :=
     (others => (others => 0));

   Max_X     : Natural := 0;
   Max_Y     : Natural := 0;
   Max_Size  : Natural := 0;
   Max_Power : Integer := Integer'First;
begin
   -- Setup area table
   for Y in 1 .. 300 loop
      for X in 1 .. 300 loop
         Sums (X, Y) := Power_Level (X, Y)
                      + Sums (X,     Y - 1)
                      + Sums (X - 1, Y    )
                      - Sums (X - 1, Y - 1);
      end loop;
   end loop;

   for Size in 1 .. 300 loop
      for Y in Size .. 300 loop
         for X in Size .. 300 loop
            declare
               Power : constant Integer := Sums (X,        Y       )
                                         - Sums (X,        Y - Size)
                                         - Sums (X - Size, Y       )
                                         + Sums (X - Size, Y - Size);
            begin
               if Power > Max_Power then
                  Max_X     := X;
                  Max_Y     := Y;
                  Max_Size  := Size;
                  Max_Power := Power;
               end if;
            end;
         end loop;
      end loop;
   end loop;

   Put_Line ("Part 2 =" & Natural'Image (Max_X - Max_Size + 1) & ","
                        & Natural'Image (Max_Y - Max_Size + 1) & ","
                        & Natural'Image (Max_Size));
end Day11;

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Text_IO;            use Ada.Text_IO;

procedure Day14 is

   package Natural_Vectors is new Vectors 
     (Index_Type   => Natural,
      Element_Type => Natural);

   Score_Board : Natural_Vectors.Vector;

   Part2_Goal       : Natural_Vectors.Vector;
   Part2_Goal_Index : Natural := 0;

   Input : constant Natural := 165061;
   Elf1  : Natural          := 0;
   Elf2  : Natural          := 1;

   procedure Mix_Recipes is
      Sum     : constant Natural := Score_Board (Elf1) + Score_Board (Elf2);
      Recipe1 : constant Natural := Sum / 10;
      Recipe2 : constant Natural := Sum mod 10;
   begin
      if Sum >= 10 then
         Score_Board.Append (Recipe1);
      end if;
      Score_Board.Append (Recipe2);

      Elf1 := (Elf1 + Score_Board (Elf1) + 1) mod Natural (Score_Board.Length);
      Elf2 := (Elf2 + Score_Board (Elf2) + 1) mod Natural (Score_Board.Length);
   end Mix_Recipes;

begin
   -- Part 1
   Score_Board.Append (3);
   Score_Board.Append (7);
   while Natural (Score_Board.Length) < (Input + 10) loop
      Mix_Recipes;
   end loop;

   Put ("Part 1 =");
   for I in Input .. (Input + 9) loop
      Put (Natural'Image (Score_Board (I)));
   end loop;
   Put_Line ("");

   -- Part 2
   Part2_Goal.Append (1);
   Part2_Goal.Append (6);
   Part2_Goal.Append (5);
   Part2_Goal.Append (0);
   Part2_Goal.Append (6);
   Part2_Goal.Append (1);
   Score_Board.Clear;
   Score_Board.Reserve_Capacity (2500000);
   Score_Board.Append (3);
   Score_Board.Append (7);
   Elf1 := 0;
   Elf2 := 1;

   Infinite_Loop :
   loop
      declare
         End_Index : constant Natural := Natural (Score_Board.Length);
      begin
         Mix_Recipes;
         for I in End_Index .. Natural (Score_Board.Length) - 1 loop
            if Score_Board (I) = Part2_Goal (Part2_Goal_Index) then
               Part2_Goal_Index := Part2_Goal_Index + 1;
            else
               Part2_Goal_Index := 0;
            end if;
            if Part2_Goal_Index = Natural (Part2_Goal.Length) then
               Put_Line
                 ("Part 2 =" & Natural'Image (I - (Part2_Goal_Index - 1)));
               exit Infinite_Loop;
            end if;
         end loop;
      end;
   end loop Infinite_Loop;
end Day14;

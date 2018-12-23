with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

with Ada.Containers.Vectors;

procedure Day23 is
   type Coordinates is record
      X : Integer_64;
      Y : Integer_64;
      Z : Integer_64;
   end record;

   function Manhattan_Distance (Pos1, Pos2 : Coordinates) return Integer_64 is
     (abs (Pos1.X - Pos2.X) + abs (Pos1.Y - Pos2.Y) + abs (Pos1.Z - Pos2.Z));

   type Nanobot is record
      Pos    : Coordinates;
      Radius : Integer_64;
   end record;

   package Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Nanobot);
   use Vectors;

   Bots    : Vector;
   File    : File_Type;
   Max_Bot : Nanobot := ((0, 0, 0), 0);
begin
   Open (File, In_File, "input.txt");
   while not End_Of_File (File) loop
      declare
         Line   : constant String := Get_Line (File);
         XYZ    : array (Positive range 1 .. 3) of Integer_64;
         I      : Positive        := XYZ'First;
         F, L   : Positive        := 6;
         Radius : Integer_64;
         Bot    : Nanobot;
      begin
         while I <= XYZ'Last loop
            if Line (L) = ',' or Line (L) = '>' then
               XYZ (I) := Integer_64'Value (Line (F .. L - 1));
               I       := I + 1;
               F       := L + 1;
            end if;
            L := L + 1;
         end loop;
         while F <= Line'Last and Line (F) /= '=' loop
            F := F + 1;
         end loop;
         Radius := Integer_64'Value (Line (F + 1 .. Line'Last));
         Bot    := ((XYZ (1), XYZ (2), XYZ (3)), Radius);
         Bots.Append (Bot);
         if Bot.Radius > Max_Bot.Radius then
            Max_Bot := Bot;
         end if;
      end;
   end loop;
   Close (File);

   declare
      Count : Natural := 0;
   begin
      for Bot of Bots loop
         if Manhattan_Distance (Max_Bot.Pos, Bot.Pos) <= Max_Bot.Radius then
            Count := Count + 1;
         end if;
      end loop;
      Put_Line ("Part 1 =" & Natural'Image (Count));
   end;
end Day23;

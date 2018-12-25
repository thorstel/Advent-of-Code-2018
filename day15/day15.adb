with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO;    use Ada.Text_IO;

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;

procedure Day15 is
   subtype Input_String is String (1 .. 32);
   type    Grid_Type    is array (Input_String'Range) of Input_String;
   type    Race_Type    is (Elf, Goblin);

   type Unit_Type is record
      Row    : Positive;
      Col    : Positive;
      Race   : Race_Type;
      HP     : Integer;
      Attack : Natural;
   end record;

   type Position is record
      Row : Positive;
      Col : Positive;
   end record;

   function "<" (Left, Right : Position) return Boolean is
   begin
      if Left.Row /= Right.Row then
         return Left.Row < Right.Row;
      else
         return Left.Col < Right.Col;
      end if;
   end "<";

   function Get_Race (Tile : Character) return Race_Type is
   begin
      case Tile is
         when 'E'    => return Elf;
         when 'G'    => return Goblin;
         when others => raise Constraint_Error with "Invalid Race: " & Tile;
      end case;
   end Get_Race;

   package PV is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Position);
   package Pos_Sorter is new PV.Generic_Sorting;

   function Get_Neighbors (Row, Col : Positive) return PV.Vector is
      Result : PV.Vector;
   begin
      if Row > 1 then
         Result.Append ((Row - 1, Col));
      end if;
      if Col > 1 then
         Result.Append ((Row, Col - 1));
      end if;
      if Col < Input_String'Last then
         Result.Append ((Row, Col + 1));
      end if;
      if Row < Input_String'Last then
         Result.Append ((Row + 1, Col));
      end if;
      return Result;
   end Get_Neighbors;

   function Lesser_Pos (Unit1, Unit2 : Unit_Type) return Boolean is
      Pos1 : constant Position := (Unit1.Row, Unit1.Col);
      Pos2 : constant Position := (Unit2.Row, Unit2.Col);
   begin
      return Pos1 < Pos2;
   end Lesser_Pos;

   package UV is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Unit_Type);
   package Unit_Sorter is new UV.Generic_Sorting ("<" => Lesser_Pos);

   Input_Grid : Grid_Type;
   Grid       : Grid_Type;
   Units      : UV.Vector;

   procedure Print_Grid (Log : File_Type; Round : Natural) is
   begin
      Put_Line (Log, "=== Round" & Natural'Image (Round) & " ===");
      for Row of Grid loop
         Put_Line (Log, Row);
      end loop;
      Put_Line (Log, "");
   end Print_Grid;

   function Find_Unit (Pos : Position) return Positive is
   begin
      for I in Units.First_Index .. Units.Last_Index loop
         if Units (I).Row = Pos.Row and
            Units (I).Col = Pos.Col and
            Units (I).HP > 0
         then
            return I;
         end if;
      end loop;
      raise Constraint_Error with
        "Invalid unit position: " & Positive'Image (Pos.Row) & "," &
        Positive'Image (Pos.Col);
   end Find_Unit;

   function Can_Attack (Row, Col : Positive; Self : Race_Type) return Natural
   is
      Pos    : constant PV.Vector := Get_Neighbors (Row, Col);
      Min_HP : Integer            := Integer'Last;
      Index  : Natural            := 0;
   begin
      for I in Pos.First_Index .. Pos.Last_Index loop
         if (Self = Elf    and Grid (Pos (I).Row) (Pos (I).Col) = 'G') or
            (Self = Goblin and Grid (Pos (I).Row) (Pos (I).Col) = 'E')
         then
            declare
               HP : constant Integer := Units (Find_Unit (Pos (I))).HP;
            begin
               if HP < Min_HP then
                  Index  := I;
                  Min_HP := HP;
               end if;
            end;
         end if;
      end loop;
      return Index;
   end Can_Attack;

   function Find_Target (Row, Col : Positive) return Position is
      type Queue_Element is record
         Row  : Positive;
         Col  : Positive;
         Dist : Natural;
      end record;

      use Ada.Containers;
      package QI is new Synchronized_Queue_Interfaces
        (Element_Type => Queue_Element);

      package USQ is new Unbounded_Synchronized_Queues
        (Queue_Interfaces => QI);

      function Pos_Hash (Pos : Position) return Hash_Type is
        (Hash_Type ((Pos.Row * Input_String'Last) + Pos.Col));
      package HM is new Hashed_Maps
        (Key_Type        => Position,
         Element_Type    => Position,
         Hash            => Pos_Hash,
         Equivalent_Keys => "=");

      Pred        : HM.Map;
      Targets     : PV.Vector;
      Q           : USQ.Queue;
      E           : Queue_Element      := (Row, Col, 0);
      Target_Dist : Natural            := Natural'Last;
      Self        : constant Race_Type := Get_Race (Grid (Row) (Col));
   begin
      Q.Enqueue (E);
      while Q.Current_Use > 0 loop
         Q.Dequeue (E);
         exit when E.Dist > Target_Dist;
         if Can_Attack (E.Row, E.Col, Self) > 0 then
            Targets.Append ((E.Row, E.Col));
            Target_Dist := E.Dist;
         end if;
         for Pos of Get_Neighbors (E.Row, E.Col) loop
            if not Pred.Contains (Pos) and Grid (Pos.Row) (Pos.Col) = '.' then
               Pred.Insert (Pos, (E.Row, E.Col));
               Q.Enqueue ((Pos.Row, Pos.Col, E.Dist + 1));
            end if;
         end loop;
      end loop;
      if Targets.Is_Empty then
         return (Row, Col);
      else
         Pos_Sorter.Sort (Targets);
         declare
            Pos : Position := Targets.First_Element;
         begin
            if Pos /= (Row, Col) then
               while Pred (Pos) /= (Row, Col) loop
                  Pos := Pred (Pos);
               end loop;
            end if;
            return Pos;
         end;
      end if;
   end Find_Target;

   function Play_Game
     (Attack_Power : Natural;
      Is_Part2     : Boolean;
      Create_Log   : Boolean) return Boolean
   is
      Log_File     : File_Type;
      Elf_Count    : Natural := 0;
      Goblin_Count : Natural := 0;
      Round_Count  : Natural := 0;
   begin
      Grid := Input_Grid;
      Units.Clear;
      for R in Grid'Range loop
         for C in Grid (R)'Range loop
            if Grid (R) (C) = 'E' then
               Units.Append ((R, C, Elf, 200, Attack_Power));
               Elf_Count := Elf_Count + 1;
            elsif Grid (R) (C) = 'G' then
               Units.Append ((R, C, Goblin, 200, 3));
               Goblin_Count := Goblin_Count + 1;
            end if;
         end loop;
      end loop;
      Assert (Unit_Sorter.Is_Sorted (Units), "Units should be sorted!");

      if Create_Log then
         Create (Log_File, Out_File, "combat-log.txt");
      end if;
      Game_Loop :
      loop
         if Create_Log then
            Print_Grid (Log_File, Round_Count);
         end if;
         for U in Units.First_Index .. Units.Last_Index loop
            declare
               Unit    : Unit_Type          := Units (U);
               Old_Pos : constant Position  := (Unit.Row, Unit.Col);
               Sym     : constant Character := Grid (Old_Pos.Row) (Old_Pos.Col);
               New_Pos : Position;
               Target  : Natural;
            begin
               if Unit.HP > 0 then
                  exit Game_Loop when Goblin_Count = 0 or Elf_Count = 0;
                  -- Move Phase
                  New_Pos := Find_Target (Old_Pos.Row, Old_Pos.Col);
                  Grid (Old_Pos.Row) (Old_Pos.Col) := '.';
                  Grid (New_Pos.Row) (New_Pos.Col) := Sym;
                  Unit.Row := New_Pos.Row;
                  Unit.Col := New_Pos.Col;
                  Units.Replace_Element (U, Unit);
                  -- Attack Phase
                  Target := Can_Attack (Unit.Row, Unit.Col, Unit.Race);
                  if Target > 0 then
                     declare
                        Pos   : constant Position := Get_Neighbors (Unit.Row, Unit.Col) (Target);
                        Idx   : constant Positive := Find_Unit (Pos);
                        Enemy : Unit_Type         := Units (Idx);
                     begin
                        Enemy.HP := Enemy.HP - Unit.Attack;
                        Units.Replace_Element (Idx, Enemy);
                        if Enemy.HP <= 0 then
                           Grid (Enemy.Row) (Enemy.Col) := '.';
                           if Enemy.Race = Elf then
                              Elf_Count := Elf_Count - 1;
                              if Is_Part2 then
                                 if Create_Log then
                                    Close (Log_File);
                                 end if;
                                 return False;
                              end if;
                           else
                              Goblin_Count := Goblin_Count - 1;
                           end if;
                        end if;
                     end;
                  end if;
               end if;
            end;
         end loop;
         Unit_Sorter.Sort (Units);
         Round_Count := Round_Count + 1;
      end loop Game_Loop;

      if Create_Log then
         Close (Log_File);
      else
         declare
            HP_Total : Natural := 0;
         begin
            for Unit of Units loop
               if Unit.HP > 0 then
                  HP_Total := HP_Total + Unit.HP;
               end if;
            end loop;
            if Is_Part2 then
               Put ("Part 2 =");
            else
               Put ("Part 1 =");
            end if;
            Put_Line (Natural'Image (Round_Count * HP_Total));
         end;
      end if;
      return True;
   end Play_Game;
begin
   Input_Handling :
   declare
      File : File_Type;
   begin
      Open (File, In_File, "input.txt");
      for R in Input_Grid'Range loop
         Input_Grid (R) := Get_Line (File);
      end loop;
      Assert (End_Of_File (File), "More input than expected!");
      Close (File);
   end Input_Handling;

   -- Part 1
   Assert (Play_Game (3, False, False));

   -- Part 2
   declare
      Attack_Power : Natural := 4;
   begin
      while not Play_Game (Attack_Power, True, False) loop
         Attack_Power := Attack_Power + 1;
      end loop;
      Put_Line ("Creating combat-log.txt...");
      Assert (Play_Game (Attack_Power, True, True));
   end;
end Day15;

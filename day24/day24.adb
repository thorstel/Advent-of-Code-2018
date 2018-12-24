with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO;    use Ada.Text_IO;

with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Ordered_Sets;

procedure Day24 is
   type Damage_Type is (Radiation, Fire, Cold, Slashing, Bludgeoning);
   type Army_Type is (Immune_System, Infection);

   package DS is new Ada.Containers.Ordered_Sets (Element_Type => Damage_Type);

   type Group_Type is record
      Allegiance  : Army_Type;
      Unit_Count  : Natural;
      Unit_HP     : Natural;
      Damage      : Natural;
      Weapon      : Damage_Type;
      Initiative  : Positive;
      Is_Targeted : Boolean;
      Targets     : Natural;
   end record;

   type Group_Array is array (Positive range <>) of Group_Type;

   function Eff_Pow (G : Group_Type) return Natural is
     (G.Unit_Count * G.Damage);

   function Lesser_Initiative (G1, G2 : Group_Type) return Boolean is
     (G1.Initiative < G2.Initiative);

   procedure Initiative_Sort is new Ada.Containers.Generic_Array_Sort
     (Index_Type   => Positive,
      Element_Type => Group_Type,
      Array_Type   => Group_Array,
      "<"          => Lesser_Initiative);

   function Greater_Power (G1, G2 : Group_Type) return Boolean is
     (Eff_Pow (G1) > Eff_Pow (G2));

   procedure Power_Sort is new Ada.Containers.Generic_Array_Sort
     (Index_Type   => Positive,
      Element_Type => Group_Type,
      Array_Type   => Group_Array,
      "<"          => Greater_Power);

   Groups     : Group_Array (1 .. 20);
   Immunities : array (Groups'Range) of DS.Set;
   Weaknesses : array (Groups'Range) of DS.Set;

   function Get_Damage (A, D : Positive) return Natural is
      Damage : Natural := Eff_Pow (Groups (A));
   begin
      if Immunities (Groups (D).Initiative).Contains (Groups (A).Weapon) then
         Damage := 0;
      elsif Weaknesses (Groups (D).Initiative).Contains (Groups (A).Weapon)
      then
         Damage := Damage * 2;
      end if;
      return Damage;
   end Get_Damage;

   procedure Reset_Input is
   begin
      for G in Groups'Range loop
         Immunities (G).Clear;
         Weaknesses (G).Clear;
      end loop;

      Groups (16) := (Immune_System, 3020, 3290,  10,  Radiation,   16, False, 0);
      Groups (9)  := (Immune_System, 528,  6169,  113, Fire,        9,  False, 0);
      Groups (1)  := (Immune_System, 4017, 2793,  6,   Slashing,    1,  False, 0);
      Weaknesses (1).Insert (Radiation);
      Groups (4)  := (Immune_System, 2915, 7735,  26,  Cold,        4,  False, 0);
      Groups (13) := (Immune_System, 3194, 1773,  5,   Cold,        13, False, 0);
      Immunities (13).Insert (Radiation);
      Weaknesses (13).Insert (Fire);
      Groups (7)  := (Immune_System, 1098, 4711,  36,  Radiation,   7,  False, 0);
      Groups (5)  := (Immune_System, 2530, 3347,  12,  Bludgeoning, 5,  False, 0);
      Immunities (5).Insert (Slashing);
      Groups (15) := (Immune_System, 216,  7514,  335, Slashing,    15, False, 0);
      Immunities (15).Insert (Cold);
      Immunities (15).Insert (Slashing);
      Weaknesses (15).Insert (Bludgeoning);
      Groups (14) := (Immune_System, 8513, 9917,  10,  Fire,        14, False, 0);
      Immunities (14).Insert (Slashing);
      Weaknesses (14).Insert (Cold);
      Groups (10) := (Immune_System, 1616, 3771,  19,  Bludgeoning, 10, False, 0);

      Groups (3)  := (Infection,     1906, 37289, 28,  Radiation,   3,  False, 0);
      Immunities (3).Insert (Radiation);
      Weaknesses (3).Insert (Fire);
      Groups (18) := (Infection,     6486, 32981, 9,   Bludgeoning, 18, False, 0);
      Groups (6)  := (Infection,     489,  28313, 110, Bludgeoning, 6,  False, 0);
      Immunities (6).Insert (Radiation);
      Immunities (6).Insert (Bludgeoning);
      Groups (12) := (Infection,     1573, 44967, 42,  Slashing,    12, False, 0);
      Weaknesses (12).Insert (Bludgeoning);
      Weaknesses (12).Insert (Cold);
      Groups (2)  := (Infection,     2814, 11032, 7,   Slashing,    2,  False, 0);
      Immunities (2).Insert (Fire);
      Immunities (2).Insert (Slashing);
      Weaknesses (2).Insert (Radiation);
      Groups (19) := (Infection,     1588, 18229, 20,  Radiation,   19, False, 0);
      Weaknesses (19).Insert (Slashing);
      Immunities (19).Insert (Radiation);
      Immunities (19).Insert (Cold);
      Groups (20) := (Infection,     608,  39576, 116, Slashing,    20, False, 0);
      Immunities (20).Insert (Bludgeoning);
      Groups (8)  := (Infection,     675,  48183, 138, Slashing,    8,  False, 0);
      Immunities (8).Insert (Cold);
      Immunities (8).Insert (Slashing);
      Immunities (8).Insert (Bludgeoning);
      Groups (17) := (Infection,     685,  11702, 32,  Fire,        17, False, 0);
      Groups (11) := (Infection,     1949, 32177, 32,  Radiation,   11, False, 0);

      Assert (for all I in Groups'Range => I = Groups (I).Initiative);
   end Reset_Input;

   -- Returns True if immune system wins. False if the infection wins or
   -- if the battle would go on forever (only immune units left).
   function Perform_Battle (Boost : Natural) return Boolean is
   begin
      Reset_Input;
      for G in Groups'Range loop
         if Groups (G).Allegiance = Immune_System then
            Groups (G).Damage := Groups (G).Damage + Boost;
         end if;
      end loop;
      Battle_Loop :
      loop
         -- Reset meta info
         for G in Groups'Range loop
            Groups (G).Targets     := 0;
            Groups (G).Is_Targeted := False;
         end loop;

         -- Target Phase
         Power_Sort (Groups);
         for G in Groups'Range loop
            if Groups (G).Unit_Count > 0 then
               declare
                  Damage     : Natural;
                  Max_Damage : Natural := 0;
                  Target     : Natural := 0;
               begin
                  for E in Groups'Range loop
                     if Groups (G).Allegiance /= Groups (E).Allegiance and
                        Groups (E).Unit_Count > 0 and 
                        not Groups (E).Is_Targeted
                     then
                        Damage := Get_Damage (G, E);
                        if Damage > Max_Damage then
                           Max_Damage := Damage;
                           Target     := E;
                        elsif Damage = Max_Damage and Target > 0 then
                           if Eff_Pow (Groups (Target)) = Eff_Pow (Groups (E)) and
                              Groups (E).Initiative > Groups (Target).Initiative
                           then
                              Target := E;
                           end if;
                        end if;
                     end if;
                  end loop;
                  if Target > 0 then
                     Groups (Target).Is_Targeted := True;
                     -- Note: After sorting with Initiative_Sort, the
                     -- value stored in 'Targets' will be the index of
                     -- the target.
                     Groups (G).Targets := Groups (Target).Initiative;
                  end if;
               end;
            end if;
         end loop;

         -- Attack Phase
         Initiative_Sort (Groups);
         declare
            G        : Natural := Groups'Last;
            Fighting : Boolean := False;
         begin
            -- Note: We sort in ascending order and iterate from the end
            -- for the 'Targets' trick above to work.
            while G > 0 loop
               if Groups (G).Unit_Count > 0 and Groups (G).Targets > 0 then
                  declare
                     E      : constant Positive := Groups (G).Targets;
                     Damage : constant Natural  := Get_Damage (G, E);
                     Losses : constant Natural  := Damage / Groups (E).Unit_HP;
                  begin
                     if Losses > Groups (E).Unit_Count then
                        Groups (E).Unit_Count := 0;
                     else
                        Groups (E).Unit_Count :=
                          Groups (E).Unit_Count - Losses;
                     end if;
                     if Losses > 0 then
                        Fighting := True;
                     end if;
                  end;
               end if;
               G := G - 1;
            end loop;
            exit Battle_Loop when not Fighting;
         end;
      end loop Battle_Loop;

      -- Determine if the immune system has won.
      for G in Groups'Range loop
         if Groups (G).Unit_Count > 0 and Groups (G).Allegiance = Infection
         then
            return False;
         end if;
      end loop;
      return True;
   end Perform_Battle;

   function Count_Alive return Natural is
      Total_Units : Natural := 0;
   begin
      for G in Groups'Range loop
         if Groups (G).Unit_Count > 0 then
            Total_Units := Total_Units + Groups (G).Unit_Count;
         end if;
      end loop;
      return Total_Units;
   end Count_Alive;
begin
   -- Part 1
   Assert (Perform_Battle (0) = False);
   Put_Line ("Part 1 =" & Natural'Image (Count_Alive));

   -- Part 2
   declare
      Boost : Natural := 1;
   begin
      while not Perform_Battle (Boost) loop
         Boost := Boost + 1;
      end loop;
      Put_Line ("Part 2 =" & Natural'Image (Count_Alive));
   end;
end Day24;

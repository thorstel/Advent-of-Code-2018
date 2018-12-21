with Ada.Containers.Ordered_Sets,
     Ada.Containers.Vectors; use Ada.Containers;
with Ada.Text_IO;            use Ada.Text_IO;
with Interfaces;             use Interfaces;
with Input16;                use Input16, Input16.OH;

procedure Day16 is

   subtype Op_String is String (1 .. 4);

   package String_Sets is new Ordered_Sets (Element_Type => Op_String);

   package String_Set_Vectors is new Vectors
     (Index_Type   => Positive,
      Element_Type => String_Sets.Set,
      "="          => String_Sets."=");

   Opcodes       : array (Natural range 0 .. 15) of String_Set_Vectors.Vector;
   Opcode_Lookup : array (Natural range 0 .. 15) of Op_String :=
     ("addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori",
      "setr", "seti", "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr");

   Part1_Count : Natural := 0;
begin
   -- Part 1
   for I in Part1_Input'Range loop
      declare
         Sample           : constant Sample_Record := Part1_Input (I);
         A                : constant Unsigned_64   := Sample.Instr (1);
         B                : constant Unsigned_64   := Sample.Instr (2);
         C                : constant Unsigned_64   := Sample.Instr (3);
         Match_Count      : Natural                := 0;
         Possible_Opcodes : String_Sets.Set;
      begin
         for I in Opcode_Lookup'Range loop
            declare 
               Op : constant Op_String := Opcode_Lookup (I);
            begin
               if Execute_Instruction (Op, Sample.Before, A, B, C) = Sample.After
               then
                  Match_Count := Match_Count + 1;
                  Possible_Opcodes.Insert (Op);
               end if;
            end;
         end loop;
         Opcodes (Natural (Sample.Instr (0))).Append (Possible_Opcodes);
         if Match_Count >= 3 then
            Part1_Count := Part1_Count + 1;
         end if;
      end;
   end loop;
   Put_Line ("Part 1 =" & Natural'Image (Part1_Count));

   -- Part 2 Setup (find the opcode values)
   declare
      Opcode      : array (Natural range 0 .. 15) of String_Sets.Set;
      Is_Finished : Boolean := False;
   begin
      for Op in Opcodes'Range loop
         declare
            Set : String_Sets.Set := Opcodes (Op).First_Element;
         begin
            for I in Opcodes (Op).First_Index + 1 .. Opcodes (Op).Last_Index
            loop
               Set := String_Sets.Intersection (Set, Opcodes (Op) (I));
            end loop;
            Opcode (Op) := Set;
         end;
      end loop;
      while not Is_Finished loop
         Is_Finished := True;
         for I in Opcode'Range loop
            if Opcode (I).Length = 1 then
               Opcode_Lookup (I) := Opcode (I).First_Element;
               for J in Opcode'Range loop
                  if J /= I then
                     Opcode (J).Exclude (Opcode (I).First_Element);
                  end if;
               end loop;
            else
               Is_Finished := False;
            end if;
         end loop;
      end loop;
   end;

   Put_Line ("Opcodes:");
   for I in Opcode_Lookup'Range loop
      Put_Line (Natural'Image (I) & " " & Opcode_Lookup (I));
   end loop;

   -- Part 2 Solution
   declare
      Reg : Registers := (0, 0, 0, 0);
   begin
      for I in Part2_Input'Range loop
         declare
            Op : constant Natural     := Natural (Part2_Input (I) (0));
            A  : constant Unsigned_64 := Part2_Input (I) (1);
            B  : constant Unsigned_64 := Part2_Input (I) (2);
            C  : constant Unsigned_64 := Part2_Input (I) (3);
         begin
            Reg := Execute_Instruction (Opcode_Lookup (Op), Reg, A, B, C);
         end;
      end loop;
      Put_Line ("Part 2 =" & Unsigned_64'Image (Reg (3)));
   end;
end Day16;

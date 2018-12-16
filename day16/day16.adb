with Ada.Containers.Ordered_Sets,
     Ada.Containers.Vectors; use Ada.Containers;
with Ada.Text_IO;            use Ada.Text_IO;
with Interfaces;             use Interfaces;
with Input16;                use Input16;

procedure Day16 is

   subtype Op_String is String (1 .. 4);

   package String_Sets is new Ordered_Sets (Element_Type => Op_String);

   package String_Set_Vectors is new Vectors
     (Index_Type   => Positive,
      Element_Type => String_Sets.Set,
      "="          => String_Sets."=");

   function Addr (Reg : Registers; A, B, C : Natural) return Registers is
      Result : Registers := Reg;
   begin
      Result (C) := Result (A) + Result (B);
      return Result;
   end Addr;

   function Addi (Reg : Registers; A, B, C : Natural) return Registers is
      Result : Registers := Reg;
   begin
      Result (C) := Result (A) + B;
      return Result;
   end Addi;

   function Mulr (Reg : Registers; A, B, C : Natural) return Registers is
      Result : Registers := Reg;
   begin
      Result (C) := Result (A) * Result (B);
      return Result;
   end Mulr;

   function Muli (Reg : Registers; A, B, C : Natural) return Registers is
      Result : Registers := Reg;
   begin
      Result (C) := Result (A) * B;
      return Result;
   end Muli;

   function Banr (Reg : Registers; A, B, C : Natural) return Registers is
      Result : Registers            := Reg;
      Value1 : constant Unsigned_32 := Unsigned_32 (Result (A));
      Value2 : constant Unsigned_32 := Unsigned_32 (Result (B));
   begin
      Result (C) := Natural (Value1 and Value2);
      return Result;
   end Banr;

   function Bani (Reg : Registers; A, B, C : Natural) return Registers is
      Result : Registers            := Reg;
      Value1 : constant Unsigned_32 := Unsigned_32 (Result (A));
      Value2 : constant Unsigned_32 := Unsigned_32 (B);
   begin
      Result (C) := Natural (Value1 and Value2);
      return Result;
   end Bani;

   function Borr (Reg : Registers; A, B, C : Natural) return Registers is
      Result : Registers            := Reg;
      Value1 : constant Unsigned_32 := Unsigned_32 (Result (A));
      Value2 : constant Unsigned_32 := Unsigned_32 (Result (B));
   begin
      Result (C) := Natural (Value1 or Value2);
      return Result;
   end Borr;

   function Bori (Reg : Registers; A, B, C : Natural) return Registers is
      Result : Registers            := Reg;
      Value1 : constant Unsigned_32 := Unsigned_32 (Result (A));
      Value2 : constant Unsigned_32 := Unsigned_32 (B);
   begin
      Result (C) := Natural (Value1 or Value2);
      return Result;
   end Bori;

   function Setr (Reg : Registers; A, C : Natural) return Registers is
      Result : Registers := Reg;
   begin
      Result (C) := Result (A);
      return Result;
   end Setr;

   function Seti (Reg : Registers; A, C : Natural) return Registers is
      Result : Registers := Reg;
   begin
      Result (C) := A;
      return Result;
   end Seti;

   function Gtir (Reg : Registers; A, B, C : Natural) return Registers is
      Result : Registers := Reg;
   begin
      if A > Result (B) then
         Result (C) := 1;
      else
         Result (C) := 0;
      end if;
      return Result;
   end Gtir;

   function Gtri (Reg : Registers; A, B, C : Natural) return Registers is
      Result : Registers := Reg;
   begin
      if Result (A) > B then
         Result (C) := 1;
      else
         Result (C) := 0;
      end if;
      return Result;
   end Gtri;

   function Gtrr (Reg : Registers; A, B, C : Natural) return Registers is
      Result : Registers := Reg;
   begin
      if Result (A) > Result (B) then
         Result (C) := 1;
      else
         Result (C) := 0;
      end if;
      return Result;
   end Gtrr;

   function Eqir (Reg : Registers; A, B, C : Natural) return Registers is
      Result : Registers := Reg;
   begin
      if A = Result (B) then
         Result (C) := 1;
      else
         Result (C) := 0;
      end if;
      return Result;
   end Eqir;

   function Eqri (Reg : Registers; A, B, C : Natural) return Registers is
      Result : Registers := Reg;
   begin
      if Result (A) = B then
         Result (C) := 1;
      else
         Result (C) := 0;
      end if;
      return Result;
   end Eqri;

   function Eqrr (Reg : Registers; A, B, C : Natural) return Registers is
      Result : Registers := Reg;
   begin
      if Result (A) = Result (B) then
         Result (C) := 1;
      else
         Result (C) := 0;
      end if;
      return Result;
   end Eqrr;

   function Execute
     (Op      : String;
      Reg     : Registers;
      A, B, C : Natural)
     return Registers
   is
      type Operations is
        (addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir,
         gtri, gtrr, eqir, eqri, eqrr);
   begin
      case Operations'Value (Op) is
         when addr => return Addr (Reg, A, B, C);
         when addi => return Addi (Reg, A, B, C);
         when mulr => return Mulr (Reg, A, B, C);
         when muli => return Muli (Reg, A, B, C);
         when banr => return Banr (Reg, A, B, C);
         when bani => return Bani (Reg, A, B, C);
         when borr => return Borr (Reg, A, B, C);
         when bori => return Bori (Reg, A, B, C);
         when setr => return Setr (Reg, A,    C);
         when seti => return Seti (Reg, A,    C);
         when gtir => return Gtir (Reg, A, B, C);
         when gtri => return Gtri (Reg, A, B, C);
         when gtrr => return Gtrr (Reg, A, B, C);
         when eqir => return Eqir (Reg, A, B, C);
         when eqri => return Eqri (Reg, A, B, C);
         when eqrr => return Eqrr (Reg, A, B, C);
      end case;
   exception
      when Constraint_Error =>
         raise Constraint_Error with "There is no " & Op & " operation";
   end Execute;

   Opcodes       : array (Natural range 0 .. 15) of String_Set_Vectors.Vector;
   Opcode_Lookup : array (Natural range 0 .. 15) of Op_String :=
     ("addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr",
      "seti", "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr");

   Part1_Count : Natural := 0;
begin
   -- Part 1
   for I in Part1_Input'Range loop
      declare
         Sample           : constant Sample_Record := Part1_Input (I);
         A                : constant Natural       := Sample.Instr (1);
         B                : constant Natural       := Sample.Instr (2);
         C                : constant Natural       := Sample.Instr (3);
         Match_Count      : Natural                := 0;
         Possible_Opcodes : String_Sets.Set;
      begin
         for I in Opcode_Lookup'Range loop
            declare 
               Op : constant Op_String := Opcode_Lookup (I);
            begin
               if Execute (Op, Sample.Before, A, B, C) = Sample.After then
                  Match_Count := Match_Count + 1;
                  Possible_Opcodes.Insert (Op);
               end if;
            end;
         end loop;
         Opcodes (Sample.Instr (0)).Append (Possible_Opcodes);
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
            Op : constant Natural := Part2_Input (I) (0);
            A  : constant Natural := Part2_Input (I) (1);
            B  : constant Natural := Part2_Input (I) (2);
            C  : constant Natural := Part2_Input (I) (3);
         begin
            Reg := Execute (Opcode_Lookup (Op), Reg, A, B, C);
         end;
      end loop;
      Put_Line ("Part 2 =" & Natural'Image (Reg (3)));
   end;
end Day16;

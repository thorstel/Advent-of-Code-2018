with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;  use Interfaces;

with Ada.Containers.Ordered_Sets;
with Opcode_Helper;

procedure Day21 is
   subtype Op_String is String (1 .. 4);

   type Instruction is record
      Op : Op_String;
      A  : Unsigned_64;
      B  : Unsigned_64;
      C  : Unsigned_64;
   end record;

   package OH is new Opcode_Helper (Register_Size => 6);
   use OH;

   package Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Unsigned_64);
   use Sets;

   Program : constant array (Natural range <>) of Instruction :=
     (("seti", 123,      0,        1),
      ("bani", 1,        456,      1),
      ("eqri", 1,        72,       1),
      ("addr", 1,        3,        3),
      ("seti", 0,        0,        3),
      ("seti", 0,        0,        1),
      ("bori", 1,        65536,    2),
      ("seti", 10605201, 9,        1),
      ("bani", 2,        255,      5),
      ("addr", 1,        5,        1),
      ("bani", 1,        16777215, 1),
      ("muli", 1,        65899,    1),
      ("bani", 1,        16777215, 1),
      ("gtir", 256,      2,        5),
      ("addr", 5,        3,        3),
      ("addi", 3,        1,        3),
      ("seti", 27,       3,        3),
      ("seti", 0,        3,        5),
      ("addi", 5,        1,        4),
      ("muli", 4,        256,      4),
      ("gtrr", 4,        2,        4),
      ("addr", 4,        3,        3),
      ("addi", 3,        1,        3),
      ("seti", 25,       3,        3),
      ("addi", 5,        1,        5),
      ("seti", 17,       5,        3),
      ("setr", 5,        5,        2),
      ("seti", 7,        6,        3),
      ("eqrr", 1,        0,        5), -- only instruction using register 0!
      ("addr", 5,        3,        3),
      ("seti", 5,        8,        3));

   IP_Reg      : constant Natural := 3;
   IP          : Unsigned_64      := 0;
   Reg         : Registers        := (others => 0);
   Last_Value  : Unsigned_64      := 0;
   Seen_Values : Set;
begin
   while IP < Program'Length loop
      declare
         Idx : constant Natural     := Natural (IP);
         Op  : constant Op_String   := Program (Idx).Op;
         A   : constant Unsigned_64 := Program (Idx).A;
         B   : constant Unsigned_64 := Program (Idx).B;
         C   : constant Unsigned_64 := Program (Idx).C;
      begin
         -- Register 0 is only used in this instruction. The program
         -- terminates if register 0 contains the value calculated in
         -- register 1.
         if IP = 28 then
            if Seen_Values.Is_Empty then
               Put_Line ("Part 1 =" & Unsigned_64'Image (Reg (1)));
            end if;
            if Seen_Values.Contains (Reg (1)) then
               -- Note: this takes quite long...
               Put_Line ("Part 2 =" & Unsigned_64'Image (Last_Value));
               exit;
            end if;
            Seen_Values.Insert (Reg (1));
            Last_Value := Reg (1);
         end if;

         Reg (IP_Reg) := IP;
         Reg          := Execute_Instruction (Op, Reg, A, B, C);
         IP           := Reg (IP_Reg) + 1;
      end;
   end loop;
end Day21;

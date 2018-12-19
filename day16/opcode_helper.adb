with Interfaces; use Interfaces;

package body Opcode_Helper is

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

   function Execute_Instruction
     (Op      : String;
      Reg     : Registers;
      A, B, C : Natural)
     return Registers
   is
      type Operations is
        (addr, addi, mulr, muli, banr, bani, borr, bori,
         setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr);
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
   end Execute_Instruction;

end Opcode_Helper;

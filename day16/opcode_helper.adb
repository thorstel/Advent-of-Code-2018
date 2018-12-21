package body Opcode_Helper is

   function Execute_Instruction
     (Op      : String;
      Reg     : Registers;
      A, B, C : Unsigned_64) return Registers
   is
      type Operations is
        (addr, addi, mulr, muli, banr, bani, borr, bori,
         setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr);
      Result : Registers := Reg;
   begin
      case Operations'Value (Op) is
         when addr =>
            Result (Natural (C)) :=
              Result (Natural (A)) + Result (Natural (B));

         when addi =>
            Result (Natural (C)) := Result (Natural (A)) + B;

         when mulr =>
            Result (Natural (C)) :=
              Result (Natural (A)) * Result (Natural (B));

         when muli =>
            Result (Natural (C)) := Result (Natural (A)) * B;

         when banr =>
            Result (Natural (C)) :=
              Result (Natural (A)) and Result (Natural (B));

         when bani =>
            Result (Natural (C)) := Result (Natural (A)) and B;

         when borr =>
            Result (Natural (C)) :=
              Result (Natural (A)) or Result (Natural (B));

         when bori =>
            Result (Natural (C)) := Result (Natural (A)) or B;

         when setr =>
            Result (Natural (C)) := Result (Natural (A));

         when seti =>
            Result (Natural (C)) := A;

         when gtir =>
            if A > Result (Natural (B)) then
               Result (Natural (C)) := 1;
            else
               Result (Natural (C)) := 0;
            end if;

         when gtri =>
            if Result (Natural (A)) > B then
               Result (Natural (C)) := 1;
            else
               Result (Natural (C)) := 0;
            end if;

         when gtrr =>
            if Result (Natural (A)) > Result (Natural (B)) then
               Result (Natural (C)) := 1;
            else
               Result (Natural (C)) := 0;
            end if;

         when eqir =>
            if A = Result (Natural (B)) then
               Result (Natural (C)) := 1;
            else
               Result (Natural (C)) := 0;
            end if;

         when eqri =>
            if Result (Natural (A)) = B then
               Result (Natural (C)) := 1;
            else
               Result (Natural (C)) := 0;
            end if;

         when eqrr =>
            if Result (Natural (A)) = Result (Natural (B)) then
               Result (Natural (C)) := 1;
            else
               Result (Natural (C)) := 0;
            end if;
      end case;
      return Result;
   exception
      when Constraint_Error =>
         raise Constraint_Error with "There is no " & Op & " operation";
   end Execute_Instruction;

end Opcode_Helper;

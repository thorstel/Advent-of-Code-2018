generic
   Register_Size : Natural := 4;
package Opcode_Helper is

   type Registers is array (Natural range 0 .. Register_Size - 1) of Natural;

   function Execute_Instruction
     (Op      : String;
      Reg     : Registers;
      A, B, C : Natural)
     return Registers;

end Opcode_Helper;

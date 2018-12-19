with Ada.Text_IO; use Ada.Text_IO;

with Ada.Containers.Vectors;
with Opcode_Helper;

procedure Day19 is
   package OH is new Opcode_Helper (Register_Size => 6);
   use OH;

   subtype Op_String is String (1 .. 4);

   type Instruction is record
      Op : Op_String;
      A  : Natural;
      B  : Natural;
      C  : Natural;
   end record;

   package Instr_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Instruction);
   use Instr_Vectors;

   Program : Vector;
   IP      : Natural   := 0;
   IP_Reg  : Natural   := 0;
   Reg     : Registers := (others => 0);

   procedure Execute_Program is
   begin
      while IP < Natural (Program.Length) loop
         declare
            Op : constant Op_String := Program (IP).Op;
            A  : constant Natural   := Program (IP).A;
            B  : constant Natural   := Program (IP).B;
            C  : constant Natural   := Program (IP).C;
         begin
            Reg (IP_Reg) := IP;
            Reg          := Execute_Instruction (Op, Reg, A, B, C);
            IP           := Reg (IP_Reg) + 1;
         end;
      end loop;
   end Execute_Program;
begin
   -- Input Handling
   declare
      File : File_Type;
   begin
      Open (File, In_File, "input.txt");
      IP_Reg := Natural'Value (Get_Line (File) (5 .. 5));
      while not End_Of_File (File) loop
         declare
            Line : constant String    := Get_Line (File);
            Op   : constant Op_String := Line (1 .. 4);
            I, J : Positive           := 6;
            ABC  : array (Positive range 1 .. 3) of Natural;
            K    : Positive           := ABC'First;
         begin
            while J <= Line'Last loop
               if Line (J) = ' ' or J = Line'Last then
                  ABC (K) := Natural'Value (Line (I .. J));
                  K       := K + 1;
                  I       := J;
               end if;
               J := J + 1;
            end loop;
            Program.Append ((Op, ABC (1), ABC (2), ABC (3)));
         end;
      end loop;
      Close (File);
   end;

   -- Part 1
   Execute_Program;
   Put_Line ("Part 1 =" & Natural'Image (Reg (0)));

   -- Reset for part 2
   Reg := (0 => 1, others => 0);
   IP  := 0;

   -- Quit the program instead of continuing the loop
   Program (Program.Last_Index).A := Natural'Last - 1;
   Execute_Program;

   -- The program would now calculate the sum of divisors. We can do
   -- that a lot faster. The corresponding register (in this case 5)
   -- may be different with other inputs.
   declare
      Sum : Natural := 0;
   begin
      for I in 1 .. Reg (5) loop
         if Reg (5) mod I = 0 then
            Sum := Sum + I;
         end if;
      end loop;
      Put_Line ("Part 2 =" & Natural'Image (Sum));
   end;
end Day19;

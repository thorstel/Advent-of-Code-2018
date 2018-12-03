with Ada.Text_IO; use Ada.Text_IO;

with Input;

procedure Day03 is
   type Fabric_Piece_Array is
     array (Natural range 1 .. 1000, Natural range 1 .. 1000) of Natural;

   Fabric_Piece : Fabric_Piece_Array := (others => (others => 0));
begin
   -- Part 1
   declare
      Claim_Counter : Integer := 0;
   begin
      for Claim of Input.Claims loop
         for I in (Claim.X + 1) .. (Claim.X + Claim.Width) loop
            for J in (Claim.Y + 1) .. (Claim.Y + Claim.Height) loop
               if Fabric_Piece (I, J) = 1 then
                  Claim_Counter := Claim_Counter + 1;
               end if;
               Fabric_Piece (I, J) := Fabric_Piece (I, J) + 1;
            end loop;
         end loop;
      end loop;
      Put_Line ("Part 1 =" & Integer'Image (Claim_Counter));
   end;

   -- Part 2
   Outer_Loop :
   for Claim of Input.Claims loop
      declare
         Is_Intact : Boolean := True;
      begin
         Check_Loop :
         for I in (Claim.X + 1) .. (Claim.X + Claim.Width) loop
            for J in (Claim.Y + 1) .. (Claim.Y + Claim.Height) loop
               if Fabric_Piece (I, J) /= 1 then
                  Is_Intact := False;
                  exit Check_Loop;
               end if;
            end loop;
         end loop Check_Loop;
         if Is_Intact then
            Put_Line ("Part 2 =" & Integer'Image (Claim.ID));
            exit Outer_Loop;
         end if;
      end;
   end loop Outer_Loop;
end Day03;

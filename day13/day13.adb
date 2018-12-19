with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO;    use Ada.Text_IO;

with Ada.Containers.Vectors;

procedure Day13 is
   subtype Input_String is String (1 .. 150);

   type Turn_Type is (Left, Straight, Right);

   type Cart_Type is record
      X           : Natural;
      Y           : Natural;
      XV          : Integer range -1 .. 1;
      YV          : Integer range -1 .. 1;
      Is_Alive    : Boolean;
      Next_Choice : Turn_Type;
   end record;

   function Lesser_Pos (C1, C2 : Cart_Type) return Boolean is
   begin
      if C1.Y = C2.Y then
         return C1.X < C2.X;
      else
         return C1.Y < C2.Y;
      end if;
   end Lesser_Pos;

   package Cart_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Cart_Type);
   use Cart_Vectors;

   package Cart_Sorter is new Generic_Sorting ("<" => Lesser_Pos);

   function Is_Cart (Symbol : Character) return Boolean is
     (Symbol = '<' or Symbol = '>' or Symbol = 'v' or Symbol = '^');

   function Cart_Symbol (XV, YV : Integer) return Character is
   begin
      if XV = -1 and YV = 0 then
         return '<';
      elsif XV = 1 and YV = 0 then
         return '>';
      elsif XV = 0 and YV = -1 then
         return '^';
      elsif XV = 0 and YV = 1 then
         return 'v';
      else
         Assert (False, "Invalid direction!");
         return ' ';
      end if;
   end Cart_Symbol;

   procedure Turn (Cart : in out Cart_Type; Tile : Character) is
      Old_XV : constant Integer := Cart.XV;
      Old_YV : constant Integer := Cart.YV;
   begin
      if Tile = '/' then
         Cart.XV := -Old_YV;
         Cart.YV := -Old_XV;
      elsif Tile = '\' then
         Cart.XV := Old_YV;
         Cart.YV := Old_XV;
      elsif Tile = '+' then
         if Cart.Next_Choice = Left then
            Cart.XV          := Old_YV;
            Cart.YV          := -Old_XV;
            Cart.Next_Choice := Straight;
         elsif Cart.Next_Choice = Straight then
            Cart.Next_Choice := Right;
         elsif Cart.Next_Choice = Right then
            Cart.XV          := -Old_YV;
            Cart.YV          := Old_XV;
            Cart.Next_Choice := Left;
         else
            Assert (False, "Unreachable");
         end if;
      end if;
   end Turn;

   Carts       : Vector;
   Cart_Grid   : array (Input_String'Range) of Input_String;
   Empty_Grid  : array (Input_String'Range) of Input_String;
   First_Crash : Boolean := True;
   Carts_Alive : Natural := 0;
begin
   Input_Handling :
   declare
      File : File_Type;
   begin
      Open (File, In_File, "input.txt");
      for Y in Input_String'Range loop
         declare
            Line : Input_String := Get_Line (File);
         begin
            Cart_Grid (Y) := Line;
            for X in Line'Range loop
               if Line (X) = 'v' then
                  Carts.Append ((X, Y, 0, 1, True, Left));
                  Line (X) := '|';
               elsif Line (X) = '^' then
                  Carts.Append ((X, Y, 0, -1, True, Left));
                  Line (X) := '|';
               elsif Line (X) = '<' then
                  Carts.Append ((X, Y, -1, 0, True, Left));
                  Line (X) := '-';
               elsif Line (X) = '>' then
                  Carts.Append ((X, Y, 1, 0, True, Left));
                  Line (X) := '-';
               end if;
            end loop;
            Empty_Grid (Y) := Line;
         end;
      end loop;
      Assert (End_Of_File (File), "More input than expected!");
      Close (File);
   end Input_Handling;
   Assert
     (Cart_Sorter.Is_Sorted (Carts),
      "Should be sorted after first parsing of the input!");
   Carts_Alive := Natural (Carts.Length);

   Infinite_Loop :
   loop
      for C in Carts.Iterate loop
         declare
            Cart  : Cart_Type         := Element (C);
            Old_X : constant Positive := Cart.X;
            Old_Y : constant Positive := Cart.Y;
            X     : constant Positive := Old_X + Cart.XV;
            Y     : constant Positive := Old_Y + Cart.YV;
         begin
            -- Remove the cart from its old position on the grid.
            Cart_Grid (Old_Y) (Old_X) := Empty_Grid (Old_Y) (Old_X);

            -- Move carts that still are alive.
            if Element (C).Is_Alive then
               if Is_Cart (Cart_Grid (Y) (X)) then
                  Cart.Is_Alive := False;
                  -- Find the other cart and kill it.
                  for C2 in Carts.Iterate loop
                     if C2 /= C then
                        if Element (C2).X = X and Element (C2).Y = Y then
                           Carts.Replace_Element
                             (C2, (X, Y, 0, 0, False, Left));
                        end if;
                     end if;
                  end loop;
                  if First_Crash then
                     Put_Line
                       ("Part 1 =" & Natural'Image (X - 1) & "," &
                        Natural'Image (Y - 1));
                     First_Crash := False;
                  end if;
                  Carts_Alive := Carts_Alive - 2;
                  if Carts_Alive <= 1 then
                     exit Infinite_Loop;
                  end if;
               else
                  Cart.X := X;
                  Cart.Y := Y;
                  Turn (Cart, Empty_Grid (Y) (X));
                  Cart_Grid (Y) (X) := Cart_Symbol (Cart.XV, Cart.YV);
               end if;
               Carts.Replace_Element (C, Cart);
            end if;
         end;
      end loop;
      Cart_Sorter.Sort (Carts);
   end loop Infinite_Loop;
   Assert (Carts_Alive = 1, "No one left alive :(");

   for C in Carts.Iterate loop
      if Element (C).Is_Alive then
         Put_Line
           ("Part 2 =" & Natural'Image (Element (C).X - 1) & "," &
            Natural'Image (Element (C).Y - 1));
         exit;
      end if;
   end loop;
end Day13;

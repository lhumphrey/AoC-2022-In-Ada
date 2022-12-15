with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

procedure Main with SPARK_Mode is

   Filename : String := "input.txt";
   File : File_Type;
   Error : Boolean := False;

   S : String (1 .. 32);
   Last : Natural;

   X : Integer := 1;
   Amount : Integer;
   Cycle : Positive := 1;
   Next_Cycle_Check : Positive := 20;
   Total : Integer := 0;
   Display : array (Integer range 0 .. 5, Integer range 0 .. 39)
                    of Character := (others => (others => '.'));

   procedure Update_Total is
   begin
      if Cycle = Next_Cycle_Check then
         if Integer'First / Cycle <= X and then X <= Integer'Last / Cycle
           and then (if Cycle * X >= 0 then Total <= Integer'Last - Cycle * X
                     else Total >= Integer'First - Cycle * X)
         then
            Total := Total + Cycle * X;
         else
            Error := True;
         end if;
         Next_Cycle_Check := Next_Cycle_Check + 40;
      end if;
      Cycle := Cycle + 1;
   end Update_Total;

   procedure Update_Display is
      Pos : Natural := (Cycle - 1) mod 40;
   begin
      if Cycle <= 240 and then
        (Pos = X or else Pos - 1 = X or else Pos + 1 = X)
      then
         Display ( (Cycle - 1) / 40, Pos) := '#';
      end if;
   end Update_Display;

begin

   Open (File, In_File, Filename);

   while not End_Of_File (File) and then not Error and then Cycle <= 240 loop

      Ada.Text_IO.Get_Line (File, S, Last);

      if S (1 .. 4) = "noop" then
         Update_Display;
         Update_Total;
      end if;

      if S (1 .. 4) = "addx" then
         Update_Display;
         Update_Total;
         Update_Display;
         Update_Total;
         Ada.Integer_Text_IO.Get (S (5 .. Last), Amount, Last);
         if (if Amount >= 0 then X <= Integer'Last - Amount else X >= Integer'First - Amount)
         then
            X := X + Amount;
         else
            Error := True;
         end if;
      end if;

      pragma Loop_Invariant (Cycle <= 243);

   end loop;

   if Error then
      Put_Line ("There was an error.");
   else
      Put_Line ("The total for Part 1 is: " & Total'Image);
      New_Line;
      Put_Line ("The image for Part 2 is: ");
      New_Line;
      for I in 0 .. 5 loop
         for J in 0 .. 39 loop
            Put (Display (I, J));
         end loop;
         New_Line;
      end loop;
   end if;

   Close (File);

end;

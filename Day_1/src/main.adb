with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

procedure Main with SPARK_Mode is
   Filename : String := "input.txt";
   File : File_Type;
   S : String (1 .. 256);
   Last : Natural;
   Error : Boolean := False;
   Element : Integer;
   Sum, Maximum : Natural := 0;
   Max1, Max2, Max3 : Natural := 0;
begin

   Open (File, In_File, Filename);

   while not End_Of_File (File) and not Error loop

      Get_Line (File, S, Last);

      if Last > 0 then
         Ada.Integer_Text_IO.Get (S (1 .. Last), Element, Last);
         if (Element <= Integer'Last - Sum and then Element >= 0) then
            Sum := Sum + Element;
         else
            Error := True;
         end if;
      else
         if Sum > Maximum then
            Maximum := Sum;
         end if;
         if Sum >= Max1 then
            Max3 := Max2;
            Max2 := Max1;
            Max1 := Sum;
         elsif Sum >= Max2 then
            Max3 := Max2;
            Max2 := Sum;
         elsif Sum >= Max3 then
            Max3 := Sum;
         end if;
         Sum := 0;
      end if;

   end loop;

   if (To_Big_Integer (Max1) + To_Big_Integer (Max2) + To_Big_Integer (Max3) <
         To_Big_Integer (Integer'Last))
   then
      Sum := Max1 + Max2 + Max3;
   else
      Error := True;
   end if;

   if Error then
      Put_Line ("There was an error.");
   else
      Put_Line ("Part 1, Maximum calories: " & Maximum'Image);
      Put_Line ("Part 2, Sum of top three calories: " & Sum'Image);
   end if;

   Close (File);
end;

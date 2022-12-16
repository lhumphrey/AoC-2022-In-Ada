with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Main with SPARK_Mode is

   pragma Warnings (Off, """*"" is set by ""*"" but not used after the call",
                    Reason => "Unused parameter is mandated by the API");

   Filename : String := "input.txt";
   File : File_Type;
   Error : Boolean := False;

   S : String (1 .. 256);
   Comma, Hyphen1, Hyphen2, Line_Last, Num_Last : Natural;
   X1, X2, Y1, Y2 : Integer;

   Num_Subset_Pairs : Natural := 0;
   Num_Overlapping_Pairs : Natural := 0;

begin

   Open (File, In_File, Filename);

   while not End_Of_File (File) and not Error loop

      Get_Line (File, S, Line_Last);

      Comma := Index (Source => S (1 .. Line_Last), Pattern => ",", From => 1);
      Hyphen1 := Index (Source => S (1 .. Line_Last), Pattern => "-", From => 1);
      if Comma > 0 then
         Hyphen2 := Index (Source => S (1 .. Line_Last), Pattern => "-", From => Comma);
      else
         Hyphen2 := 0;
      end if;

      if Hyphen1 > 0 and then Hyphen2 > 0 and then Comma > 0 and then
         Hyphen1 < Comma and then Hyphen2 > Comma
      then
         Ada.Integer_Text_IO.Get (S (1 .. Hyphen1 - 1), X1, Num_Last);
         Ada.Integer_Text_IO.Get (S (Hyphen1 + 1 .. Comma - 1), X2, Num_Last);
         Ada.Integer_Text_IO.Get (S (Comma + 1 .. Hyphen2 - 1), Y1, Num_Last);
         Ada.Integer_Text_IO.Get (S (Hyphen2 + 1 .. Line_Last), Y2, Num_Last);

         if (X1 <= Y1 and then Y2 <= X2) or else
            (Y1 <= X1 and then X2 <= Y2)
         then
            -- One segment is a subset of the other
            if Num_Subset_Pairs < Integer'Last then
               Num_Subset_Pairs := Num_Subset_Pairs + 1;
            else
               Error := True;
            end if;
         end if;

         if (Y1 <= X1 and then X1 <= Y2) or else
            (Y1 <= X2 and then X2 <= Y2) or else
            (X1 <= Y1 and then Y2 <= X2)
         then
            -- The first segment has at least one endpoint in the second or
            -- the first segment contains the second, and the two overlap
            if Num_Overlapping_Pairs < Integer'Last then
               Num_Overlapping_Pairs := Num_Overlapping_Pairs + 1;
            else
               Error := True;
            end if;
         end if;

      end if;

   end loop;

   if Error then
      Put_Line ("There was an error.");
   else
      Put_Line ("Number of subset pairs for Part 1: " & Num_Subset_Pairs'Image);
      Put_Line ("Number of overlapping pairs for Part 2: " & Num_Overlapping_Pairs'Image);
   end if;

   Close (File);
end;

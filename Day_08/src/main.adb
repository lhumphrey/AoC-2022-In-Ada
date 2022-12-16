with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
-- with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;

procedure Main with SPARK_Mode is

   pragma Warnings (Off, """*"" is set by ""*"" but not used after the call",
                    Reason => "Unused parameter is mandated by the API");

   Max : constant Positive := 150;

   subtype Forest_Idx is Integer range 1 .. Max;

   type Forest_Arr is array (Forest_Idx, Forest_Idx) of Integer
     with Default_Component_Value => 0;

   Forest, Left_Height, Right_Height, Top_Height, Bottom_Height : Forest_Arr;

   Last_Row, Last_Column : Natural := 0;

   Visible : Natural := 0;
   Max_Score : Natural := 0;

   Filename : String := "input.txt";
   File : File_Type;
   Error : Boolean := False;

begin

   Open (File, In_File, Filename);

   declare
      S : String (1 .. Max);
      Last : Natural;
   begin

      ---------------------------------------------------------------
      -- Read file into Forest, recording Last_Row and Last_Column --
      ---------------------------------------------------------------

      while not End_Of_File (File) and not Error loop

         Ada.Text_IO.Get_Line (File, S, Last);

         if Last_Row < Max then
            Last_Row := Last_Row + 1;
         else
            Error := True;
            exit;
         end if;

         if Last_Column = 0 and then Last > 0 and then Last <= Max then
            Last_Column := Last;
         elsif Last /= Last_Column or else Last_Column > Max or else Last_Column <= 0 then
            Error := True;
            exit;
         end if;

         for I in 1 .. Last_Column loop
            Ada.Integer_Text_IO.Get (S (I .. I), Forest (Last_Row, I), Last);
         end loop;

         pragma Loop_Invariant (Last_Row <= Max);
         pragma Loop_Invariant (Last_Column > 0 and then Last_Column <= Max);

      end loop;
   end;

   pragma Assert (Last_Row <= Max and then Last_Column <= Max);

   ------------------------------------------------------------------------
   -- For each tree, determine tallest tree to left, right, top, bottom. --
   -- Use this to determine how many trees are visible.                  --
   ------------------------------------------------------------------------

   for I in 2 .. Last_Row - 1 loop
      for J in 2 .. Last_Column - 1 loop
         if J = 2 then
            Left_Height (I, J) := Forest (I, J - 1);
         else
            Left_Height (I, J) := (if Forest (I, J - 1) > Left_Height (I, J - 1)
                                   then Forest (I, J - 1)
                                   else Left_Height (I, J - 1));
         end if;
      end loop;
   end loop;

   for I in 2 .. Last_Row - 1 loop
      for J in reverse 2 .. Last_Column - 1 loop
         if J = Last_Column - 1 then
            Right_Height (I, J) := Forest (I, J + 1);
         else
            Right_Height (I, J) := (if Forest (I, J + 1) > Right_Height (I, J + 1)
                                   then Forest (I, J + 1)
                                   else Right_Height (I, J + 1));
         end if;
      end loop;
   end loop;

   for I in 2 .. Last_Row - 1 loop
      for J in 2 .. Last_Column - 1 loop
         if I = 2 then
            Top_Height (I, J) := Forest (I - 1, J);
         else
            Top_Height (I, J) := (if Forest (I - 1, J) > Top_Height (I - 1, J)
                                  then Forest (I - 1, J)
                                  else Top_Height (I - 1, J));
         end if;
      end loop;
   end loop;

   for I in reverse 2 .. Last_Row - 1 loop
      for J in 2 .. Last_Column - 1 loop
         if I = Last_Row - 1 then
            Bottom_Height (I, J) := Forest (I + 1, J);
         else
            Bottom_Height (I, J) := (if Forest (I + 1, J) > Bottom_Height (I + 1, J)
                                     then Forest (I + 1, J)
                                     else Bottom_Height (I + 1, J));
         end if;
      end loop;
   end loop;

   if Last_Row > 0 and then Last_Column > 0 then
      Visible := 2 * Last_Row + 2 * Last_Column - 4;
   end if;

   for I in 2 .. Last_Row - 1 loop
      for J in 2 .. Last_Column - 1 loop
         if Forest (I, J) > Right_Height (I, J) or else
           Forest (I, J) > Left_Height (I, J) or else
           Forest (I, J) > Top_Height (I, J) or else
           Forest (I, J) > Bottom_Height (I, J)
         then
            if Visible < Integer'Last then
               Visible := Visible + 1;
            else
               Error := True;
            end if;
         end if;
      end loop;
   end loop;

   ----------------------------------------------------------------
   -- For each tree, compute its scenic score. Find the largest. --
   ----------------------------------------------------------------

   for I in 1 .. Last_Row loop
      for J in 1 .. Last_Column loop
         declare
            Score : Natural := 1;
            Count : Natural := 0;
         begin
            for II in I + 1 .. Last_Row loop
               Count := Count + 1;
               if Forest (I, J) <= Forest (II, J) then
                  exit;
               end if;
               pragma Loop_Invariant (Count = II - I);
            end loop;

            Score := Score * Count;
            Count := 0;

            for II in reverse 1 .. I - 1 loop
               Count := Count + 1;
               if Forest (I, J) <= Forest (II, J) then
                  exit;
               end if;
               pragma Loop_Invariant (Count = I - II);
            end loop;

            Score := Score * Count;
            Count := 0;

            for JJ in J + 1 .. Last_Column loop
               Count := Count + 1;
               if Forest (I, J) <= Forest (I, JJ) then
                  exit;
               end if;
               pragma Loop_Invariant (Count = JJ - J);
            end loop;

            if (Count = 0 or else Score < Integer'Last / Count)
            then
               Score := Score * Count;
            else
               Error := True;
            end if;
            Count := 0;

            for JJ in reverse 1 .. J - 1 loop
               Count := Count + 1;
               if Forest (I, J) <= Forest (I, JJ) then
                  exit;
               end if;
               pragma Loop_Invariant (Count = J - JJ);
            end loop;

            if (Count = 0 or else Score < Integer'Last / Count)
            then
               Score := Score * Count;
            else
               Error := True;
            end if;

            if Score > Max_Score then
               Max_Score := Score;
            end if;

         end;
      end loop;
   end loop;


   if Error then
      Put_Line ("There was an error.");
   else
      Put_Line ("The number of trees visible in Part 1 is: " & Visible'Image);
      Put_Line ("The maximum scenic score for Part 2 is: " & Max_Score'Image);
   end if;

   Close (File);

end;

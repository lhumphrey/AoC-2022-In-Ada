with Ada.Text_IO; use Ada.Text_IO;

procedure Main with SPARK_Mode is

   Filename : String := "input.txt";
   File : File_Type;
   Error : Boolean := False;

   S : String (1 .. 6000) := (others => ' ');
   Ind_1, Ind_2, Last : Natural := 0;

begin

   Open (File, In_File, Filename);

   if not End_Of_File (File) then
      Get_Line (File, S, Last);
   else
      Error := True;
   end if;


   for I in S'First .. Last - 3 loop
      if (for all J in I .. I + 2 =>
            (for all K in J + 1 .. I + 3 => S (J) /= S (K)))
      then
         Ind_1 := I + 3;
         exit;
      end if;
   end loop;

   for I in S'First .. Last - 13 loop
      if (for all J in I .. I + 12 =>
            (for all K in J + 1 .. I + 13 => S (J) /= S (K)))
      then
         Ind_2 := I + 13;
         exit;
      end if;
   end loop;


   Put_Line ("String size is : " & Last'Image);

   if Error then
      Put_Line ("There was an error.");
   else
      Put_Line ("Packet index for Part 1 is: " & Ind_1'Image);
      Put_Line ("Packet index for Part 2 is: " & Ind_2'Image);
   end if;

   Close (File);

end;

with Ada.Text_IO; use Ada.Text_IO;

procedure Main with SPARK_Mode is

   pragma Warnings (Off, """*"" is set by ""*"" but not used after the call",
                    Reason => "Unused parameter is mandated by the API");

   subtype Item is Character range 'A' .. 'z';

   type Priority is array (Item) of Positive;

   function Initialize_Priority return Priority is
      P : Priority := (others => 1);
      I : Positive := 1;
   begin
      for C in Item'('a') .. Item'('z') loop
         P (C) := I;
         I := I + 1;
         pragma Loop_Invariant (I <= Item'Pos (C));
      end loop;

      for C in Item'('A') .. Item'('Z') loop
         P (C) := I;
         I := I + 1;
         pragma Loop_Invariant (I <= Item'Pos (C) + 100);
      end loop;

      return P;
   end Initialize_Priority;

   To_Priority : Priority := Initialize_Priority;

   function Find_Common_Sack_Priority (S1, S2 : String) return Natural is
      Result : Integer := 0;
   begin
      for I in S1'Range loop
         for J in S2'Range loop
            if S1 (I) = S2 (J) then
               if S1 (I) in Item then
                  return To_Priority (S1 (I));
               end if;
            end if;
         end loop;
      end loop;
      return Result;
   end Find_Common_Sack_Priority;

   function Find_Common_Badge_Priority (S1, S2, S3 : String) return Natural is
      Result : Integer := 0;
   begin
      for I in S1'Range loop
         for J in S2'Range loop
            for K in S3'Range loop
               if S1 (I) = S2 (J) and then S1 (I) = S3 (K) then
                  if S1 (I) in Item then
                     return To_Priority (S1 (I));
                  end if;
               end if;
            end loop;
         end loop;
      end loop;
      return Result;
   end Find_Common_Badge_Priority;

   Filename : String := "input.txt";
   File : File_Type;
   Error : Boolean := False;

   subtype String256 is String (1 .. 256);
   type String3by256 is array (0 .. 2) of String256;

   S : String256;
   Last : Natural;
   S3 : String3by256 := (others => (others => ' '));
   Last3 : array (0 .. 2) of Integer := (others => 0);

   Line_Number : Natural := 0;
   Sack_Priority_Result, Badge_Priority_Result : Natural;
   Sack_Sum, Badge_Sum : Natural := 0;
begin

   Open (File, In_File, Filename);

   while not End_Of_File (File) and not Error loop

      Get_Line (File, S, Last);

      Sack_Priority_Result :=
        Find_Common_Sack_Priority (S (1 .. Last/2), S (Last/2 + 1 .. Last));

      if Sack_Sum <= Integer'Last - Sack_Priority_Result then
         Sack_Sum := Sack_Sum + Sack_Priority_Result;
      else
         Error := True;
      end if;

      if Line_Number < Integer'Last then
         Line_Number := Line_Number + 1;
      else
         Error := True;
      end if;

      S3 (Line_Number mod 3) := S;
      Last3 (Line_Number mod 3) := Last;

      if Line_Number mod 3 = 0 then
         Badge_Priority_Result :=
           Find_Common_Badge_Priority (S3 (0)(1 .. Last3(0)),
                                       S3 (1)(1 .. Last3(1)),
                                       S3 (2)(1 .. Last3(2)));
         if Badge_Sum <= Integer'Last - Badge_Priority_Result then
            Badge_Sum := Badge_Sum + Badge_Priority_Result;
         else
            Error := True;
         end if;
      end if;

      pragma Loop_Invariant (Last3(0) <= 256
                             and Last3(1) <= 256
                             and Last3(2) <= 256);

   end loop;

   if Error then
      Put_Line ("There was an error.");
   else
      Put_Line ("Priority sum for Part 1: " & Sack_Sum'Image);
      Put_Line ("Priority sum for Part 2: " & Badge_Sum'Image);
   end if;

   Close (File);
end;

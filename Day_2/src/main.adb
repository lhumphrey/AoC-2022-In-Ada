with Ada.Text_IO; use Ada.Text_IO;

procedure Main with SPARK_Mode is

   type Input1 is (A, B, C);
   type Input2 is (X, Y, Z);
   type Play is (Rock, Paper, Scissors);
   type Outcome is (Win, Lose, Draw);

   function To_Play (I1 : Input1) return Play is
     (case I1 is when A => Rock, when B => Paper, when C => Scissors);

   function To_Play (I2 : Input2) return Play is
     (case I2 is when X => Rock, when Y => Paper, when Z => Scissors);

   function To_Outcome (I2 : Input2) return Outcome is
     (case I2 is when X => Lose, when Y => Draw, when Z => Win);

   function Points (P : Play) return Positive is
     (case P is when Rock => 1, when Paper => 2, when Scissors => 3);

   function Points (O : Outcome) return Natural is
     (case O is when Win => 6, when Draw => 3, when Lose => 0);

   -------------------
   -- Process_Input --
   -------------------

   procedure Process_Input (File : in File_Type;
                            I1 : out Input1;
                            I2 : out Input2;
                            Error : out Boolean)
     with Pre => Is_Open (File) and then Mode (File) = In_File and then not End_Of_File (File)
   is
      S : String (1 .. 256);
      Last : Natural;
   begin

      Error := False;

      Get_Line (File, S, Last);

      case S (1) is
         when 'A' =>
            I1 := A;
         when 'B' =>
            I1 := B;
         when 'C' =>
            I1 := C;
         when others =>
            I1 := A;
            Error := True;
      end case;

      case S (3) is
         when 'X' =>
            I2 := X;
         when 'Y' =>
            I2 := Y;
         when 'Z' =>
            I2 := Z;
         when others =>
            I2 := X;
            Error := True;
      end case;

   end Process_Input;

   ---------------
   -- Score_One --
   ---------------

   function Score_One (Opponent_Choice, Player_Choice : Play) return Natural
     with Post => Score_One'Result <= 9
   is
      The_Outcome : Outcome;
   begin
      case Player_Choice is
         when Rock =>
            The_Outcome := (case Opponent_Choice is
                               when Rock => Draw,
                               when Paper => Lose,
                               when Scissors => Win);
         when Paper =>
            The_Outcome := (case Opponent_Choice is
                               when Rock => Win,
                               when Paper => Draw,
                               when Scissors => Lose);
         when Scissors =>
            The_Outcome := (case Opponent_Choice is
                               when Rock => Lose,
                               when Paper => Win,
                               when Scissors => Draw);
      end case;

      return Points (The_Outcome) + Points (Player_Choice);
   end Score_One;

   ---------------
   -- Score_Two --
   ---------------

   function Score_Two (Opponent_Choice : Play; Desired_Outcome : Outcome)
                       return Natural
     with Post => Score_Two'Result <= 9
   is
      Player_Choice : Play;
   begin
      case Opponent_Choice is
         when Rock =>
            Player_Choice := (case Desired_Outcome is
                                 when Win => Paper,
                                 when Draw => Rock,
                                 when Lose => Scissors);
         when Paper =>
            Player_Choice := (case Desired_Outcome is
                                 when Win => Scissors,
                                 when Draw => Paper,
                                 when Lose => Rock);
         when Scissors =>
            Player_Choice := (case Desired_Outcome is
                                 when Win => Rock,
                                 when Draw => Scissors,
                                 when Lose => Paper);
      end case;

      return Points (Desired_Outcome) + Points (Player_Choice);
   end Score_Two;

   Filename : String := "input.txt";
   File : File_Type;
   Error : Boolean := False;

   I1 : Input1;
   I2 : Input2;
   Total_Score1 : Natural := 0;
   Total_Score2 : Natural := 0;

begin

   Open (File, In_File, Filename);

   while not End_Of_File (File) and not Error loop

      Process_Input (File, I1, I2, Error);

      if Total_Score1 <= Integer'Last - 9
        and Total_Score2 <= Integer'Last - 9
      then
         Total_Score1 :=
           Total_Score1 + Score_One (To_Play (I1), To_Play (I2));

         Total_Score2 :=
           Total_Score2 + Score_Two (To_Play (I1), To_Outcome (I2));
      else
         Error := True;
      end if;

   end loop;

   if Error then
      Put_Line ("There was an error.");
   else
      Put_Line ("Total score for part one: " & Total_Score1'Image);
      Put_Line ("Total score for part two: " & Total_Score2'Image);
   end if;

   Close (File);
end;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Formal_Hashed_Sets;
use type Ada.Containers.Hash_Type;

procedure Main with SPARK_Mode is

   pragma Warnings (Off, """*"" is set by ""*"" but not used after the call",
                    Reason => "Unused parameter is mandated by the API");

   type Direction is (Up, Down, Left, Right);

   subtype Input_String is String (1 .. 256);

   type Position is record
      X, Y : Integer := 0;
   end record;

   type Position_Arr is array (1 .. 9) of Position;

   function Position_Hash (P : Position) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type'Mod(P.Y)*1000 + Ada.Containers.Hash_Type'Mod(P.X));

   package Position_Sets is new
     Ada.Containers.Formal_Hashed_Sets (Position, Position_Hash);
   use Position_Sets;

   -----------------
   -- Get_Command --
   -----------------

   procedure Get_Command (S : String;
                          Amount : out Natural;
                          Dir : out Direction;
                          Error : in out Boolean)
     with Pre => S'Length >= 3;

   procedure Get_Command (S : String;
                          Amount : out Natural;
                          Dir : out Direction;
                          Error : in out Boolean)
   is
      C : Character := S (S'First);
      Num : Integer := 0;
      Last : Positive;
   begin

      if C in 'D' | 'U' | 'L' | 'R' then
         Dir := (case C is when 'D' => Down, when 'U' => Up, when 'L' => Left,
                    when 'R' => Right, when others => Down);
      else
         Dir := Down;
         Error := True;
      end if;

      if S'First + 2 <= S'Last then
         Ada.Integer_Text_IO.Get (S (S'First + 2 .. S'Last), Num, Last);
      else
         Error := True;
      end if;

      if Num > 0 then
         Amount := Num;
      else
         Amount := 0;
         Error := True;
      end if;

   end Get_Command;

   -------------------
   -- Wont_Overflow --
   -------------------

   function Wont_Overflow (P : Position; Amount : Natural) return Boolean is
     (Integer'Last - Amount >= P.X and then
      Integer'First + Amount <= P.X and then
      Integer'Last - Amount >= P.Y and then
      Integer'First + Amount <= P.Y);

   ---------------
   -- Move_Head --
   ---------------

   procedure Move_Head (Head : in out Position; Dir : Direction; Amount : Natural)
     with Pre => Wont_Overflow (Head, Amount);

   procedure Move_Head (Head : in out Position; Dir : Direction; Amount : Natural)
   is
   begin
      case Dir is
         when Up => Head.Y := Head.Y + Amount;
         when Down => Head.Y := Head.Y - Amount;
         when Left => Head.X := Head.X - Amount;
         when Right => Head.X := Head.X + Amount;
      end case;
   end Move_Head;

   -----------------
   -- Is_Adjacent --
   -----------------

   function Is_Adjacent (X1, X2 : Integer) return Boolean is
   begin
      if X1 = Integer'Last then
         return (X2 = Integer'Last or else X2 = Integer'Last - 1);
      elsif X2 = Integer'Last then
         return (X1 = Integer'Last - 1);
      elsif X1 = Integer'First then
         return (X2 = Integer'First or else X2 = Integer'First + 1);
      elsif X2 = Integer'First then
         return (X1 = Integer'First + 1);
      else
         return X1 <= 1 + X2 and then X1 >= -1 + X2;
      end if;
   end Is_Adjacent;

   function Is_Adjacent (P1, P2 : Position) return Boolean;

   function Is_Adjacent (P1, P2 : Position) return Boolean is
     (Is_Adjacent(P1.X, P2.X) and then Is_Adjacent(P1.Y, P2.Y));

   -----------------
   -- Is_Diagonal --
   -----------------

   function Is_Diagonal (P1, P2 : Position) return Boolean;

   function Is_Diagonal (P1, P2 : Position) return Boolean is
     (P1.X /= P2.X and then P1.Y /= P2.Y);

   ---------------
   -- Move_Knot --
   ---------------

   procedure Move_Knot_One (Head : Position; Knot : in out Position);

   procedure Move_Knot_One (Head : Position; Knot : in out Position)
   is
   begin
      if Head /= Knot and then not Is_Adjacent (Head, Knot) then
         if Is_Diagonal (Head, Knot) then
            Knot.X := (if Knot.X < Head.X then Knot.X + 1 else Knot.X - 1);
            Knot.Y := (if Knot.Y < Head.Y then Knot.Y + 1 else Knot.Y - 1);
         else
            if Knot.X /= Head.X then
               Knot.X := (if Knot.X < Head.X then Knot.X + 1 else Knot.X - 1);
            else
               Knot.Y := (if Knot.Y < Head.Y then Knot.Y + 1 else Knot.Y - 1);
            end if;
         end if;
      end if;
   end Move_Knot_One;

   ---------------
   -- Move_Tail --
   ---------------

   procedure Move_Tail (Head : Position; Tail : in out Position;
                        Visited_Positions : in out Position_Sets.Set;
                        Error : in out Boolean);

   procedure Move_Tail (Head : Position; Tail : in out Position;
                        Visited_Positions : in out Position_Sets.Set;
                        Error : in out Boolean)
   is
      use type Ada.Containers.Count_Type;
   begin
      while Head /= Tail and then not Is_Adjacent (Head, Tail) loop

         if Is_Diagonal (Head, Tail) then
            Tail.X := (if Tail.X < Head.X then Tail.X + 1 else Tail.X - 1);
            Tail.Y := (if Tail.Y < Head.Y then Tail.Y + 1 else Tail.Y - 1);
         else
            if Tail.X /= Head.X then
               Tail.X := (if Tail.X < Head.X then Tail.X + 1 else Tail.X - 1);
            else
               Tail.Y := (if Tail.Y < Head.Y then Tail.Y + 1 else Tail.Y - 1);
            end if;
         end if;

         if Length (Visited_Positions) < Visited_Positions.Capacity then
            Include (Visited_Positions, Tail);
         else
            Error := True;
         end if;

      end loop;
   end Move_Tail;

   ----------------
   -- Move_Knots --
   ----------------

   procedure Move_Knots (Head : Position; Knots : in out Position_Arr;
                         Visited_Positions : in out Position_Sets.Set;
                         Error : in out Boolean);

   procedure Move_Knots (Head : Position; Knots : in out Position_Arr;
                         Visited_Positions : in out Position_Sets.Set;
                         Error : in out Boolean)
   is
      use type Ada.Containers.Count_Type;
   begin

      while not Error and then
        ((Knots (1) /= Head and then not Is_Adjacent (Head, Knots (1)))
         or else (for some I in Knots'First .. Knots'Last - 1 =>
                   Knots (I) /= Knots (I + 1) and then
                    not Is_Adjacent (Knots (I), Knots (I + 1))))
      loop

            Move_Knot_One (Head, Knots (Knots'First));

         for I in Knots'First .. Knots'Last - 1 loop
               Move_Knot_One (Knots (I), Knots (I + 1));
         end loop;

         if Length (Visited_Positions) < Visited_Positions.Capacity then
            Include (Visited_Positions, Knots (Knots'Last));
         else
            Error := True;
         end if;

      end loop;

   end Move_Knots;

   Filename : String := "input.txt";
   File : File_Type;
   Error : Boolean := False;
   S : Input_String;
   Dir : Direction := Up;
   Amount : Natural := 0;
   Last : Natural;
   Head, Tail : Position := (0, 0);
   Knots : Position_Arr;
   Visited_Positions_1, Visited_Positions_2 : Position_Sets.Set (8192, 256);

begin

   Include (Visited_Positions_1, (0,0));
   Include (Visited_Positions_2, (0,0));

   Open (File, In_File, Filename);

   while not End_Of_File (File) and not Error loop
      Ada.Text_IO.Get_Line (File, S, Last);

      if Last >= 3 then
         Get_Command (S (1 .. Last), Amount, Dir, Error);
      else
         Error := True;
      end if;

      if not Error and then Wont_Overflow (Head, Amount) then
         Move_Head (Head, Dir, Amount);
         Move_Tail (Head, Tail, Visited_Positions_1, Error);
         Move_Knots (Head, Knots, Visited_Positions_2, Error);
      else
         Error := True;
      end if;
   end loop;

   if Error then
      Put_Line ("There was an error.");
   else
      Put_Line ("The number of visited locations in Part 1 is: " &
                  Length (Visited_Positions_1)'Image);
      Put_Line ("The number of visited locations in Part 2 is: " &
                  Length (Visited_Positions_2)'Image);
   end if;

   Close (File);

end;

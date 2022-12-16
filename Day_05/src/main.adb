with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Formal_Vectors;
with Ada.Strings.Fixed;

procedure Main with SPARK_Mode is

   pragma Warnings (Off, """*"" is set by ""*"" but not used after the call",
                    Reason => "Unused parameter is mandated by the API");

   pragma Warnings (Off, "analyzing unreferenced function ""*""",
                    Reason => "Extra functions for debugging only");

   pragma Warnings (Off, "analyzing unreferenced procedure ""*""",
                    Reason => "Extra functions for debugging only");

   Max_Stacks : constant Positive := 9;
   Max_Crates : constant Positive := 256;

   subtype Crate_Index is Integer range 1 .. Max_Crates;
   subtype Extended_Crate_Index is Integer range 0 .. Max_Crates;
   subtype Crate_Arr is String (Crate_Index);

   type Crate_Stack is record
      Crates : Crate_Arr := (others => '0');
      Size: Extended_Crate_Index := 0;
   end record;

   subtype Stack_Index is Integer range 1 .. Max_Stacks;
   subtype Extended_Stack_Index is Integer range 1 .. Max_Stacks;
   type Dock is array (Stack_Index) of Crate_Stack;

   procedure Add_Crate
     (Stack : in out Crate_Stack;
      Crate_Label : in Character)
     with
       Pre => Stack.Size < Max_Crates,
       Post => Stack.Size = Stack.Size'Old + 1;

   procedure Add_Crate
     (Stack : in out Crate_Stack;
      Crate_Label : in Character) is
   begin
      Stack.Size := Stack.Size + 1;
      Stack.Crates (Stack.Size) := Crate_Label;
   end Add_Crate;

   procedure Insert_Crate_On_Bottom
     (Stack : in out Crate_Stack;
      Crate_Label : in Character)
     with
       Pre => Stack.Size < Max_Crates,
       Post => Stack.Size = Stack.Size'Old + 1;

   procedure Insert_Crate_On_Bottom
     (Stack : in out Crate_Stack;
      Crate_Label : in Character) is
   begin
      Stack.Crates (2 .. Stack.Size + 1) := Stack.Crates (1 .. Stack.Size);
      Stack.Crates (1) := Crate_Label;
      Stack.Size := Stack.Size + 1;
   end Insert_Crate_On_Bottom;

   procedure Remove_Crate
     (Stack : in out Crate_Stack;
      Crate_Label : out Character)
     with
       Pre => Stack.Size > 0,
       Post => Stack.Size = Stack.Size'Old - 1;

   procedure Remove_Crate
     (Stack : in out Crate_Stack;
      Crate_Label : out Character) is
   begin
      Crate_Label := Stack.Crates (Stack.Size);
      Stack.Size := Stack.Size - 1;
   end Remove_Crate;

   function Top_Crate (Stack : Crate_Stack) return Character is
     (if (Stack.Size > 0) then Stack.Crates (Stack.Size) else ' ');

   function Number_Of_Crates (This_Dock : Dock) return Natural is
      Result : Natural := 0;
   begin
      for I in Stack_Index loop
         Result := Result + This_Dock (I).Size;
      end loop;
      return Result;
   end Number_Of_Crates;

   ------------------------
   -- Build_Crate_Stacks --
   ------------------------

   procedure Build_Crate_Stacks
     (S : in String;
      D : in out Dock;
      Error : in out Boolean)
   is
      Idx : Positive;
   begin
      for I in 1 .. Max_Stacks loop
         Idx := (I - 1) * 4 + 2;
         if Idx <= S'Last and then D (I).Size < Max_Crates then
            if S (Idx) in 'A' .. 'z' then
               Insert_Crate_On_Bottom (D (I), S (Idx));
            end if;
         else
            Error := True;
         end if;
      end loop;
   end Build_Crate_Stacks;


   --------------------------
   -- Get_Move_Instruction --
   --------------------------

   procedure Get_Move_Instruction
     (S : in String;
      Amount, From, To : out Positive;
      Error : in out Boolean)
   is
      Num : Integer;
      IdxMove, IdxFrom, IdxTo, Last : Natural;
      Idx11, Idx12, Idx21, Idx22, Idx31, Idx32 : Positive := 1;
   begin

      Amount := 1;
      From := 1;
      To := 1;

      IdxMove := Ada.Strings.Fixed.Index (S, "move", 1);
      IdxFrom := Ada.Strings.Fixed.Index (S, "from", 1);
      IdxTo := Ada.Strings.Fixed.Index (S, "to", 1);

      if IdxMove > 0 and then IdxFrom > IdxMove and then IdxTo > IdxFrom then
         Idx11 := IdxMove + 4;
         Idx12 := IdxFrom - 1;
         Idx21 := IdxFrom + 4;
         Idx22 := IdxTo - 1;
         Idx31 := IdxTo + 2;
         Idx32 := S'Last;
      else
         Error := True;
      end if;

      if Idx11 in S'Range and Idx12 in S'Range then
         Ada.Integer_Text_IO.Get (S (Idx11 .. Idx12), Num, Last);
         if Num in Positive then
            Amount := Num;
         else
            Error := True;
         end if;
      else
         Error := True;
      end if;

      if Idx21 in S'Range and Idx22 in S'Range then
         Ada.Integer_Text_IO.Get (S (Idx21 .. Idx22), Num, Last);
         if Num in Positive then
            From := Num;
         else
            Error := True;
         end if;
      else
         Error := True;
      end if;

      if Idx31 in S'Range and Idx32 in S'Range then
         Ada.Integer_Text_IO.Get (S (Idx31 .. Idx32), Num, Last);
         if Num in Positive then
            To := Num;
         else
            Error := True;
         end if;
      else
         Error := True;
      end if;

   end Get_Move_Instruction;

   -----------------
   -- Move_Crates --
   -----------------

   procedure Move_Crates (This_Dock : in out Dock;
                          Amount : Positive;
                          From, To : in Stack_Index)
     with
       Pre => Amount <= This_Dock (From).Size and then
       This_Dock (To).Size + Amount <= Max_Crates;

   procedure Move_Crates (This_Dock : in out Dock;
                          Amount : Positive;
                          From, To : in Stack_Index)
   is
      To_Stack : Crate_Stack := This_Dock (To);
      From_Stack : Crate_Stack := This_Dock (From);
      C : Character;
   begin

      for I in 1 .. Amount loop
         pragma Loop_Invariant (From_Stack.Size =
                                  From_Stack.Size'Loop_Entry - I + 1);
         pragma Loop_Invariant (From_Stack.Size > 0);
         pragma Loop_Invariant (To_Stack.Size =
                                  To_Stack.Size'Loop_Entry + I - 1);
         pragma Loop_Invariant (To_Stack.Size < Max_Crates);
         Remove_Crate (From_Stack, C);
         Add_Crate (To_Stack, C);
      end loop;

      This_Dock (To) := To_Stack;
      This_Dock (From) := From_Stack;

   end;

   ----------------------
   -- Crate_Mover_9001 --
   ----------------------

   procedure Crate_Mover_9001 (This_Dock : in out Dock;
                               Amount : in Positive;
                               From, To : in Stack_Index)
     with Pre =>
       Amount <= This_Dock (From).Size and then
       This_Dock (To).Size + Amount <= Max_Crates;


   procedure Crate_Mover_9001
     (This_Dock : in out Dock;
      Amount : in Positive;
      From, To : in Stack_Index)
   is
      Temp_Stack : Crate_Stack;
      C : Character;
   begin

      for I in 1 .. Amount loop
         pragma Loop_Invariant (This_Dock (From).Size =
                                  This_Dock (From).Size'Loop_Entry - I + 1);
         pragma Loop_Invariant (This_Dock (From).Size > 0);
         pragma Loop_Invariant (Temp_Stack.Size = I - 1);
         Remove_Crate (This_Dock (From), C);
         Add_Crate (Temp_Stack, C);
      end loop;

         for I in 1 .. Amount loop
            pragma Loop_Invariant (This_Dock (To).Size =
                                     This_Dock (To).Size'Loop_Entry + I - 1);
         pragma Loop_Invariant (This_Dock (To).Size < Max_Crates);
         pragma Loop_Invariant (Temp_Stack.Size = Amount - I + 1);
            Remove_Crate (Temp_Stack, C);
            Add_Crate (This_Dock (To), C);
      end loop;

   end;

   ----------------
   -- Print_Dock --
   ----------------

   procedure Print_Dock (This_Dock : Dock) is
   begin
      for I in This_Dock'Range loop
         Put (I'Image & ": ");
         for J in 1 .. This_Dock (I).Size loop
            Put (This_Dock (I).Crates (J));
         end loop;
         New_Line;
      end loop;
   end;

   ---------------
   -- Print_Top --
   ---------------

   procedure Print_Top (This_Dock : Dock) is
      C : Character;
   begin
      for I in Stack_Index loop
         C := Top_Crate (This_Dock (I));
         Put (C);
      end loop;
      New_Line;
   end;

   Filename : String := "input.txt";
   File : File_Type;
   S : String (1 .. 256);
   Last: Natural;
   Amount, From, To : Positive;
   Error : Boolean := False;
   This_Dock_1, This_Dock_2 : Dock;

begin

   Open (File, In_File, Filename);

   -- Initialize Crate_Stacks for Part 1 and copy for Part 2
   while not End_Of_File (File) and not Error loop
      Get_Line (File, S, Last);
      if Ada.Strings.Fixed.Index (S, "1", S'First) > 0 then
         exit;
      else
         Build_Crate_Stacks (S (S'First .. Last), This_Dock_1, Error);
      end if;
   end loop;

   This_Dock_2 := This_Dock_1;

   -- Discard blank line
   if not End_Of_File (File) then
      Get_Line (File, S, Last);
   else
      Error := True;
   end if;

   -- Implement moves
   while not End_Of_File (File) and not Error loop
      Get_Line (File, S, Last);
      Get_Move_Instruction (S (S'First .. Last), Amount, From, To, Error);
      if From in Stack_Index and then To in Stack_Index and then Amount > 0
        and then Amount <= This_Dock_1 (From).Size
        and then This_Dock_1 (To).Size + Amount <= Max_Crates
      then
         Move_Crates (This_Dock_1, Amount, From, To);
      else
         Error := True;
      end if;
      if From in Stack_Index and then To in Stack_Index and then Amount > 0
        and then Amount <= This_Dock_2 (From).Size
        and then This_Dock_2 (To).Size + Amount <= Max_Crates
      then
         Crate_Mover_9001 (This_Dock_2, Amount, From, To);
      else
         Error := True;
      end if;
   end loop;

   -- Print results
   if Error then
      Put_Line ("There was an error.");
   else
      Put_Line ("Top crates for Part 1: ");
      Print_Top (This_Dock_1);
      Put_Line ("Top crates for Part 2: ");
      Print_Top (This_Dock_2);
   end if;

   Close (File);
end;

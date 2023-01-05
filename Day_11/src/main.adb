with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Integer_Text_IO;
with Ada.Containers.Formal_Vectors;
use type Ada.Containers.Count_Type;

procedure Main with SPARK_Mode is

   pragma Warnings (Off, """*"" is set by ""*"" but not used after the call",
                    Reason => "Unused parameter is mandated by the API");

   pragma Warnings (Off, "analyzing unreferenced procedure ""*""",
                    Reason => "Unreferenced procedure used for debugging only");


   Filename : String := "input.txt";
   File : File_Type;
   Error : Boolean := False;
   Max_Monkeys : constant Positive := 10;
   Last_Monkey : Natural := 0;
   Total_1 : Natural := 0;
   Total_2 : Long_Long_Integer := 0;
   Mod_Factor : Positive;

   subtype Monkey_Ind is Integer range 0 .. Max_Monkeys;

   type Operation is (Mult, Plus);

   package Natural_Vectors is new Ada.Containers.Formal_Vectors (Positive, Natural);
   use Natural_Vectors;

   type Item_Vector is new Natural_Vectors.Vector (256);

   type Monkey is record
      ID : Monkey_Ind := 0;
      Items : Item_Vector;
      Op : Operation := Plus;
      Arg_2 : Natural := 0;
      Div_Test_Value : Positive := 1;
      Throw_To_If_True : Monkey_Ind := 0;
      Throw_To_If_False: Monkey_Ind := 0;
      Items_Examined : Natural := 0;
   end record;

   type Monkey_Arr is array (Monkey_Ind) of Monkey;

   Monkeys_1, Monkeys_2 : Monkey_Arr;

   Numeric : constant Character_Set := To_Set (Span => ('0','9'));

   ------------------
   -- Print_Monkey --
   ------------------

   procedure Print_Monkey (M : Monkey) is

   begin
      Put_Line ("ID : " & M.ID'Image);
      Put ("Items: ");
      for I of M.Items loop
         Put (I'Image & ",");
      end loop;
      New_Line;
      Put_Line ("Operation: " & (if M.Op = Mult then "*" else "+"));
      Put_Line ("Arg2 : " & (if M.Arg_2 = 0 then "old" else M.Arg_2'Image));
      Put_Line ("Div test: " & M.Div_Test_Value'Image);
      Put_Line ("Throw if true: " & M.Throw_To_If_True'Image);
      Put_Line ("Throw if false: " & M.Throw_To_If_False'Image);
      Put_Line ("Items examined : " & M.Items_Examined'Image);
      New_Line;
   end Print_Monkey;

   -----------------------
   -- Get_Monkey_Number --
   -----------------------

   procedure Get_Monkey_Number
     (S : String;
      M : in out Monkey;
      Error : in out Boolean)
     with Pre => S'Length > 1;

   procedure Get_Monkey_Number
     (S : String;
      M : in out Monkey;
      Error : in out Boolean)
   is
      Idx, Last, Num : Integer;
   begin

      if Index (S, "Monkey", S'First) = 0 then
         Error := True;
         return;
      end if;

      Idx := Index (S, Numeric, S'First);
      if Idx in S'Range then
         Ada.Integer_Text_IO.Get (S (Idx .. S'Last), Num, Last);
         if Num in Monkey_Ind then
            M.ID := Num;
         else
            Error := True;
            return;
         end if;
      else
         Error := True;
         return;
      end if;

   end Get_Monkey_Number;

   ----------------------
   -- Get_Monkey_Items --
   ----------------------

   procedure Get_Monkey_Items
     (S : String;
      M : in out Monkey;
      Error : in out Boolean)
     with Pre => S'Length > 1;

   procedure Get_Monkey_Items
     (S : String;
      M : in out Monkey;
      Error : in out Boolean)
   is
      Idx, Last : Natural;
      Num : Integer;
   begin

      if Index (S, "items:", S'First) = 0 then
         Error := True;
         return;
      end if;

      Idx := Index (S, Numeric, S'First);
      if Idx = 0 then
         Error := True;
         return;
      end if;

      while Idx in S'Range loop
         Ada.Integer_Text_IO.Get (S (Idx .. S'Last), Num, Last);
         if Length (M.Items) < M.Items.Capacity and then Num >= 0 then
            Append (M.Items, Num);
         else
            Error := True;
            return;
         end if;
         if Last in S'First .. S'Last - 1 then
            Idx := Index (S, Numeric, Last + 1);
         else
            Idx := 0;
         end if;
      end loop;

   end Get_Monkey_Items;

   --------------------------
   -- Get_Monkey_Operation --
   --------------------------

   procedure Get_Monkey_Operation
     (S : String;
      M : in out Monkey;
      Error : in out Boolean)
     with Pre => S'Length > 1;

   procedure Get_Monkey_Operation
     (S : String;
      M : in out Monkey;
      Error : in out Boolean)
   is
      Idx, Last : Natural;
      Num : Integer;
      Operator : constant Character_Set := To_Set ("*+");
   begin

      if Index (S, "Operation: new = old", S'First) = 0 then
         Error := True;
         return;
      end if;

      Idx := Index (S, Operator, S'First);
      if Idx > 0 then
         M.Op := (if S (Idx .. Idx) = "+" then Plus else Mult);
      else
         Error := True;
         return;
      end if;

      if Idx <= S'Last - 1 then
         Idx := Idx + 1;
      else
         Error := True;
         return;
      end if;

      if Idx > 0 then
         if Index (S (Idx .. S'Last), "old", Idx) > 0 then
            M.Arg_2 := 0;
         else
            Idx := Index (S, Numeric, Idx);
            if Idx > 0 then
               Ada.Integer_Text_IO.Get (S (Idx .. S'Last), Num, Last);
               if Num >= 0 then
                  M.Arg_2 := Num;
               else
                  Error := True;
                  return;
               end if;
            else
               Error := True;
               return;
            end if;
         end if;
      else
         Error := True;
         return;
      end if;

   end Get_Monkey_Operation;

   ---------------------------
   -- Get_Monkey_Test_Value --
   ---------------------------

   procedure Get_Monkey_Test_Value
     (S : String;
      M : in out Monkey;
      Error : in out Boolean)
     with Pre => S'Length > 1;

   procedure Get_Monkey_Test_Value
     (S : String;
      M : in out Monkey;
      Error : in out Boolean)
   is
      Idx, Last : Natural;
      Num : Integer;
   begin

      if Index (S, "Test: divisible by", S'First) = 0 then
         Error := True;
         return;
      end if;

      Idx := Index (S, Numeric, S'First);

      if Idx > 0 then
         Ada.Integer_Text_IO.Get (S (Idx .. S'Last), Num, Last);
         if Num > 0 then
            M.Div_Test_Value := Num;
         else
            Error := True;
            return;
         end if;
      else
         Error := True;
         return;
      end if;

   end Get_Monkey_Test_Value;

   ----------------------------
   -- Get_Monkey_Throw_Value --
   ----------------------------

   procedure Get_Monkey_Throw_Value
     (S : String;
      M : in out Monkey;
      Cond : Boolean;
      Error : in out Boolean)
     with Pre => S'Length > 1;

   procedure Get_Monkey_Throw_Value
     (S : String;
      M : in out Monkey;
      Cond : Boolean;
      Error : in out Boolean)
   is
      Idx, Last : Natural;
      Num : Integer;
   begin
      if Cond and then
        Index (S, "If true: throw to monkey", S'First) = 0
      then
         Error := True;
         return;
      elsif not Cond and then
        Index (S, "If false: throw to monkey", S'First) = 0
      then
         Error := True;
         return;
      end if;

      Idx := Index (S, Numeric, S'First);
      if Idx > 0 then
         Ada.Integer_Text_IO.Get (S (Idx .. S'Last), Num, Last);
         if Num in Monkey_Ind then
            if Cond then
               M.Throw_To_If_True := Num;
            else
               M.Throw_To_If_False := Num;
            end if;
         else
            Error := True;
            return;
         end if;
      else
         Error := True;
         return;
      end if;
   end;

   -----------------------
   -- Initialize_Monkey --
   -----------------------

   procedure Initialize_Monkey
     (File : File_Type;
      M : in out Monkey;
      Error : in out Boolean)
     with Pre =>
       Is_Open (File) and then
       Mode (File) = In_File and then
       not End_Of_File (File);

   procedure Initialize_Monkey
     (File : File_Type;
      M : in out Monkey;
      Error : in out Boolean)
   is
      S : String (1 .. 256);
      Last : Natural;
   begin

      -- Get monkey number
      Get_Line (File, S, Last);

      if Last > 1 then
         Get_Monkey_Number (S (1 .. Last), M, Error);
      else
         Error := True;
         return;
      end if;

      -- Get items
      if not End_Of_File (File) then
         Get_Line (File, S, Last);
      else
         Error := True;
         return;
      end if;

      if Last > 1 then
         Get_Monkey_Items (S (1 .. Last), M, Error);
      else
         Error := True;
         return;
      end if;

      -- Get operation
      if not End_Of_File (File) then
         Get_Line (File, S, Last);
      else
         Error := True;
         return;
      end if;

      if Last > 1 then
         Get_Monkey_Operation (S (1 .. Last), M, Error);
      else
         Error := True;
         return;
      end if;

      -- Get division test value
      if not End_Of_File (File) then
         Get_Line (File, S, Last);
      else
         Error := True;
         return;
      end if;

      if Last > 1 then
         Get_Monkey_Test_Value (S (1 .. Last), M, Error);
      else
         Error := True;
         return;
      end if;

      -- Get Monkey to throw to if test is true.
      if not End_Of_File (File) then
         Get_Line (File, S, Last);
      else
         Error := True;
         return;
      end if;

      if Last > 1 then
         Get_Monkey_Throw_Value (S (1 .. Last), M, True, Error);
      else
         Error := True;
         return;
      end if;

      -- Get Monkey to throw to if test is true.
      if not End_Of_File (File) then
         Get_Line (File, S, Last);
      else
         Error := True;
         return;
      end if;

      if Last > 1 then
         Get_Monkey_Throw_Value (S (1 .. Last), M, False, Error);
      else
         Error := True;
         return;
      end if;

      -- Discard empty line.
      if not End_Of_File (File) then
         Get_Line (File, S, Last);
      end if;

   end Initialize_Monkey;

   ---------------------
   -- Inspect_Items_1 --
   ---------------------

   procedure Inspect_Items_1 (M : in out Monkey; Error : in out Boolean) is
      Arg, Item : Natural;
      Result : Integer;
   begin
      for I in First_Index (M.Items) .. Last_Index (M.Items) loop
         Item := Element (M.Items, I);
         Arg := (if M.Arg_2 = 0 then Item else M.Arg_2);
         if M.Op = Plus then
            if Item <= Integer'Last - Arg then
               Result := Item + Arg;
            else
               Error := True;
               return;
            end if;
         else
            if Arg = 0 or else Item <= Integer'Last / Arg then
               Result := Item * Arg;
            else
               Error := True;
               return;
            end if;
         end if;
         Result := Result / 3;
         Replace_Element (M.Items, I, Result);

         if M.Items_Examined < Integer'Last then
            M.Items_Examined := M.Items_Examined + 1;
         else
            Error := True;
            return;
         end if;

         pragma Loop_Invariant (Length (M.Items) = Length (M.Items'Loop_Entry));
      end loop;
   end Inspect_Items_1;

   ---------------------
   -- Inspect_Items_2 --
   ---------------------

   procedure Inspect_Items_2
     (M : in out Monkey;
      Mod_Factor : Positive;
      Error : in out Boolean)
   is
      Arg, Item, Result : Long_Long_Integer;
      MF : Long_Long_Integer := Long_Long_Integer (Mod_Factor);
   begin
      for I in First_Index (M.Items) .. Last_Index (M.Items) loop
         Item := Long_Long_Integer (Element (M.Items, I)) mod MF;
         Arg := (if M.Arg_2 = 0
                 then Long_Long_Integer (Item)
                 else Long_Long_Integer (M.Arg_2));
         if M.Op = Plus then
            if Item <= Long_Long_Integer'Last - Arg then
               Result :=  Item +  Arg;
            else
               Error := True;
               return;
            end if;
         else
            if Arg = 0 or else Item <= Long_Long_Integer'Last / Arg then
               Result :=  Item * Arg;
            else
               Put_Line ("Here");
               Error := True;
               return;
            end if;
         end if;

         Result := Result mod MF;
         Replace_Element (M.Items, I, Natural (Result));

         if M.Items_Examined < Integer'Last then
            M.Items_Examined := M.Items_Examined + 1;
         else
            Error := True;
            return;
         end if;

         pragma Loop_Invariant (Length (M.Items) = Length (M.Items'Loop_Entry));
      end loop;
   end Inspect_Items_2;

   -----------------
   -- Throw_Items --
   -----------------

   procedure Throw_Items
     (Items, Items_True, Items_False : in out Item_Vector;
      Div_Test_Value : Positive)
   with
     Pre =>
       Length (Items_True) + Length (Items) <= Items_True.Capacity and then
       Length (Items_False) + Length (Items) <= Items_False.Capacity;

   procedure Throw_Items
     (Items, Items_True, Items_False : in out Item_Vector;
      Div_Test_Value : Positive)
   is
      Item : Natural;
   begin
      for I in First_Index (Items) .. Last_Index (Items) loop
         Item := Element (Items, I);
         if Item mod Div_Test_Value = 0 then
            Append (Items_True, Item);
         else
            Append (Items_False, Item);
         end if;
         pragma Loop_Invariant
           (Length (Items_True) <=
                Length (Items_True'Loop_Entry) +
                Capacity_Range (I) - Capacity_Range (First_Index (Items)) + 1);
         pragma Loop_Invariant
           (Length (Items_False) <=
                Length (Items_False'Loop_Entry) +
                Capacity_Range (I) - Capacity_Range (First_Index (Items)) + 1);
      end loop;
      if Length (Items) > 0 then
         Delete (Items, First_Index (Items), Length (Items));
      end if;
   end Throw_Items;

begin

   Open (File, In_File, Filename);

   while not End_Of_File (File) and not Error loop
      declare
         M : Monkey;
      begin
         Initialize_Monkey (File, M, Error);
         Monkeys_1 (M.ID) := M;
         Last_Monkey := M.ID;
      end;
   end loop;

   Close (File);

   Monkeys_2 := Monkeys_1;

   for Round in 1 .. 20 loop
      for ID in Monkey_Ind'First .. Last_Monkey loop
         Inspect_Items_1 (Monkeys_1 (ID), Error);
         declare
            M_True : Item_Vector :=
              Monkeys_1 (Monkeys_1 (ID).Throw_To_If_True).Items;
            M_False : Item_Vector :=
              Monkeys_1 (Monkeys_1 (ID).Throw_To_If_False).Items;
         begin
            if Length (M_True) + Length (Monkeys_1 (ID).Items) <= M_True.Capacity
              and then
                Length (M_False) + Length (Monkeys_1 (ID).Items) <= M_False.Capacity
            then
               Throw_Items (Monkeys_1 (ID).Items, M_True, M_False,
                            Monkeys_1 (ID).Div_Test_Value);
               Monkeys_1 (Monkeys_1 (ID).Throw_To_If_True).Items := M_True;
               Monkeys_1 (Monkeys_1 (ID).Throw_To_If_False).Items := M_False;
            end if;
         end;
      end loop;
   end loop;

   declare
      Max1, Max2 : Natural := 0;
   begin
      for ID in Monkey_Ind'First .. Last_Monkey loop
         if Monkeys_1 (ID).Items_Examined >= Max1 then
            Max2 := Max1;
            Max1 := Monkeys_1 (ID).Items_Examined;
         elsif Monkeys_1 (ID).Items_Examined >= Max2 then
            Max2 := Monkeys_1 (ID).Items_Examined;
         end if;
      end loop;

      if Max2 = 0 or else Max1 < Integer'Last / Max2 then
         Total_1 := Max1 * Max2;
      else
         Error := True;
      end if;
   end;

   Mod_Factor := 1;
   for ID in Monkey_Ind'First .. Last_Monkey loop
      if Mod_Factor< Integer'Last / Monkeys_2 (ID).Div_Test_Value then
         Mod_Factor := Mod_Factor * Monkeys_2 (ID).Div_Test_Value;
      else
         Error := True;
      end if;
   end loop;

   for Round in 1 .. 10000 loop

      for ID in Monkey_Ind'First .. Last_Monkey loop
         Inspect_Items_2 (Monkeys_2 (ID), Mod_Factor, Error);
         declare
            M_True : Item_Vector :=
              Monkeys_2 (Monkeys_2 (ID).Throw_To_If_True).Items;
            M_False : Item_Vector :=
              Monkeys_2 (Monkeys_2 (ID).Throw_To_If_False).Items;
         begin
            if Length (M_True) + Length (Monkeys_2 (ID).Items) <= M_True.Capacity
              and then
                Length (M_False) + Length (Monkeys_2 (ID).Items) <= M_False.Capacity
            then
               Throw_Items (Monkeys_2 (ID).Items, M_True, M_False,
                            Monkeys_2 (ID).Div_Test_Value);
               Monkeys_2 (Monkeys_2 (ID).Throw_To_If_True).Items := M_True;
               Monkeys_2 (Monkeys_2 (ID).Throw_To_If_False).Items := M_False;
            end if;
         end;
      end loop;
   end loop;

   declare
      Max1, Max2 : Natural := 0;
   begin
      for ID in Monkey_Ind'First .. Last_Monkey loop
         if Monkeys_2 (ID).Items_Examined >= Max1 then
            Max2 := Max1;
            Max1 := Monkeys_2 (ID).Items_Examined;
         elsif Monkeys_2 (ID).Items_Examined >= Max2 then
            Max2 := Monkeys_2 (ID).Items_Examined;
         end if;
      end loop;

      if Max2 = 0 or else
        Long_Long_Integer (Max1) < Long_Long_Integer'Last / Long_Long_Integer (Max2)
      then
         Total_2 := Long_Long_Integer (Max1) * Long_Long_Integer (Max2);
      else
         Error := True;
      end if;
   end;

   if Error then
      Put_Line ("There was an error.");
   else
      Put_Line ("The total for Part 1 is: " & Total_1'Image);
      Put_Line ("The total for Part 2 is: " & Total_2'Image);
   end if;


end;

with Ada.Text_IO;
with Ada.Float_Text_IO;

procedure Main is

   type Float_Array is array (Integer range <>) of Float;
   type Feature is new Float range 0.0 .. 1.0;
   type Feature_Array is array (Integer range <>) of Feature;

   procedure Read (Name : String; Result : out Float_Array; Last : in out Integer; Min : in out Float; Max : in out Float) is
      use Ada.Text_IO;
      use Ada.Float_Text_IO;
      F : File_Type;
   begin
      Open (F, In_File, Name);
      loop
         exit when End_Of_File (F);
         Last := Last + 1;
         Get (F, Result (Last));
         Skip_Line (F);
         Min := Float'Min (Min, Result (Last));
         Max := Float'Max (Max, Result (Last));
         exit when Last = Result'Last;
      end loop;
      Close (F);
   end;

   function Normalize (Value : Float; Min, Max : Float) return Float is ((Value - Min) / (Max - Min));

   procedure Normalize (Min : Float; Max : Float; Scale : Float; Result : in out Float_Array) is
   begin
      for E of Result loop
         E := Normalize (E, Min, Max) * Scale;
      end loop;
   end;

   procedure Put (Item : Feature_Array) is
      use Ada.Float_Text_IO;
      use Ada.Text_IO;
   begin
      for E of Item loop
         Put (Float (E), 3, 3, 0);
         New_Line;
      end loop;
   end;

   procedure Put (Item : Float_Array) is
      use Ada.Float_Text_IO;
      use Ada.Text_IO;
   begin
      for E of Item loop
         Put (E, 3, 3, 0);
         New_Line;
      end loop;
   end;

   procedure Read (Item : out Feature_Array; Last : in out Integer) with
     Pre => Feature_Array'Component_Size = Float_Array'Component_Size,
     Post => (for all E of Item (Item'First .. Last) => E'Valid);

   procedure Read (Item : out Feature_Array; Last : in out Integer) is
      Data : Float_Array (Item'Range) with Address => Item'Address;
      Min : Float := Float'Last;
      Max : Float := Float'First;
   begin
      Read ("f.ssv", Data, Last, Min, Max);
      Ada.Text_IO.Put_Line ("Before normalization.");
      Put (Data (Data'First .. Last));
      --Normalize (Min, Max, 2.0, Data (Data'First .. Last));
      Normalize (Min, Max, 1.0, Data (Data'First .. Last));
   end;

   F : Feature_Array (-5 .. 10);
   Last : Integer := F'First - 1;

begin

   Read (F, Last);
   Ada.Text_IO.Put_Line ("After normalization.");
   Put (F (F'First .. Last));

end;

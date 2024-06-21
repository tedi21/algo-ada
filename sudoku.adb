with Ada.Strings;
use Ada.Strings;

with Ada.Strings.Fixed; 
use Ada.Strings.Fixed;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ada.Strings.Maps;  
use Ada.Strings.Maps;

with Ada.Text_IO; 
use Ada.Text_IO; 

package body Sudoku is

    -- Algo Solver
    package Algo is new 
    Solver.Algo 
        (Value_Type => Positive, 
         Indice_Type => Coordinate_2D);
    use Algo;
    use Variables;
    use Domains;
    use Solutions;

    -- Problem Data
    type Row is array (Positive range <>) of Natural;
    type Matrix is array (Positive range <>) of access Row;
    N: Natural := 0;
    Grid: access Matrix := null;
    Solver: Solver_Type;

    -- Subprograms
    procedure Input(Problem: in Strings) is
        I: Positive := 1;
        J: Positive := 1;

        Whitespace : constant Character_Set :=
            To_Set (' ');
    begin
        N := Problem'Length;
        Grid := new Matrix(1..N);
        for I in Grid'Range loop
            Grid(I) := new Row(1..N);
            declare
                Source: constant String := To_String(Problem(I));
                F: Positive := 1;
                L: Natural := 1;
            begin
                for J in Grid(I)'Range loop
                    Find_Token
                        (Source  => Source,
                        Set     => Whitespace,
                        From    => L,
                        Test    => Outside,
                        First   => F,
                        Last    => L);

                    exit when L = 0;

                    Grid(I)(J) := Integer'Value (Source(F .. L));
                    
                    L := L + 1;
                end loop;
            end;
        end loop;
    end Input;

    function Test_Column(   Solver : in out Solver_Type;
                            C: in Coordinate_2D;
                            Value: in Positive)
            return Boolean is
        Sastified : Boolean := True;
        I : constant Positive := C.Y;
        J : constant Positive := C.X;
        K : Positive := 1;
    begin
        loop
            exit when K = N + 1 or not Sastified;
            if K /= I then
                Sastified := Solver.Exclude(Value, (X => J, Y => K));
            end if;
            K := K + 1;
        end loop;
        --Put_Line(Sastified'Image);
        return Sastified;
    end Test_Column;

    function Test_Line( Solver : in out Solver_Type;
                        C: in Coordinate_2D;
                        Value: in Positive)
            return Boolean is
        Sastified : Boolean := True;
        I : constant Positive := C.Y;
        J : constant Positive := C.X;
        K : Positive := 1;
    begin
        loop
            exit when K = N + 1 or not Sastified;
            if K /= J then
                Sastified := Solver.Exclude(Value, (X => K, Y => I));
            end if;
            K := K + 1;
        end loop;
        --Put_Line(Sastified'Image);
        return Sastified;
    end Test_Line;

    function Test_Square(   Solver : in out Solver_Type;
                            C: in Coordinate_2D;
                            Value: in Positive)
            return Boolean is
        Sastified : Boolean := True;
        I : constant Positive := C.Y;
        J : constant Positive := C.X;
        I1 : constant Natural := (((I - 1) / 3) * 3);
        J2 : constant Natural := (((J - 1) / 3) * 3);
        K1 : Positive;
        K2 : Positive;
    begin
        K1 := I1 + 1;
        loop
            exit when K1 > I1 + 3 or not Sastified;
            K2 := J2 + 1;
            loop
                exit when K2 > J2 + 3 or not Sastified;
                if K1 /= I or K2 /= J then
                    Sastified := Solver.Exclude(Value, (X => K2, Y => K1));
                end if;
                K2 := K2 + 1;
            end loop;
            K1 := K1 + 1;
        end loop;
        --Put_Line(Sastified'Image);
        return Sastified;
    end Test_Square;

    function Compare_Variable(  Var1: in Variable_Type;
                                Var2: in Variable_Type)
            return Boolean is
    begin
        return var1.Get_Domain_Size < Var2.Get_Domain_Size;
    end Compare_Variable;

    function Select_Value(Var: in Variable_Type)
            return Positive is
    begin
        return var.Get_Domain.First_Value;
    end Select_Value;

    procedure Initialize is
        Domain: Value_Array := (for K in 1 .. N => K);
        unused: Boolean;
    begin
        Solver.Set_Comparator(Function_Ptr => Compare_Variable'access);
        Solver.Set_Selector(Function_Ptr => Select_Value'access);

        Solver.Add_Constraint(Function_Ptr => Test_Column'access);
        Solver.Add_Constraint(Function_Ptr => Test_Line'access);
        Solver.Add_Constraint(Function_Ptr => Test_Square'access);

        for I in Grid'Range loop
            for J in Grid(I)'Range loop
                if Grid(I)(J) = 0 then
                    unused := Solver.Add_Variable(Values => Domain, 
                                                  Indice => (Y => I, X => J));
                else
                    unused := Solver.Add_Variable(Values => (1 => Grid(I)(J)), 
                                                  Indice => (Y => I, X => J));
                end if;
            end loop;
        end loop;
    end Initialize;

    function Solve
        return Boolean is 
    begin 
        return Solver.Solve;
    end Solve;

    procedure Show is
        Solution: Solution_Type := Solver.Get_Solution( 1 );
    begin
        Solution.Sort;
        for V of Solution loop
            Put(integer'image(V.Get_Value) & " ");
            if V.Get_Indice.X = N then
                New_Line;
            end if;
        end loop;
    end Show;

end Sudoku;
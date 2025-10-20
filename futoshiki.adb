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

with Ada.Containers.Vectors;
use Ada.Containers;
with Key_Values.Dependencies;

package body Futoshiki is

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

    package Coordinate_2D_Key is new 
    Key_Values
        (Key_Type => Coordinate_2D);
    package Coordinate_2D_Dependencies is new 
    Coordinate_2D_Key.Dependencies;
    use Coordinate_2D_Dependencies;
    use Dependencies_Maps;

	Inf_Constraints: Dependency_Map;
    Sup_Constraints: Dependency_Map;
    List_Constraints: Dependency_Vector;

    -- Subprograms
    procedure Input(Problem: in Strings) is
        I: Positive := 1;
        Y: Positive := 1;
    begin
        N := (Problem'Length + 1) / 2;
        Grid := new Matrix(1..N);
        while I <= Problem'Length loop
            Y := (I + 1) / 2;
            if (I mod 2) = 1 then
                Grid(Y) := new Row(1..N);
            end if;
            declare
                Source: constant String := To_String(Problem(I));
                F: Positive := 1;
                L: Natural := 1;
                J: Positive := 1;
                X: Positive := 1;
                Whitespace : constant Character_Set := To_Set (' ');               
                Chars: Unbounded_String;
            begin
                while J <= Grid(Y)'Length and L <= Source'Length loop
                    Find_Token
                        (Source  => Source,
                         Set     => Whitespace,
                         From    => L,
                         Test    => Outside,
                         First   => F,
                         Last    => L);

                    exit when L = 0;

                    X := (F + 3) / 4;

                    Chars := To_Unbounded_String(Source(F .. L));
                    if Chars = "<" then
                        Inf_Constraints.Insert((X, Y), (X + 1, Y));
                        Sup_Constraints.Insert((X + 1, Y), (X, Y));
                    elsif Chars = ">" then
                        Inf_Constraints.Insert((X + 1, Y), (X, Y));
                        Sup_Constraints.Insert((X, Y), (X + 1, Y));
                    elsif Chars = "^" then
                        Inf_Constraints.Insert((X, Y), (X, Y + 1));
                        Sup_Constraints.Insert((X, Y + 1), (X, Y));
                    elsif Chars = "v" then
                        Inf_Constraints.Insert((X, Y + 1), (X, Y));
                        Sup_Constraints.Insert((X, Y), (X, Y + 1));
                    else
                        Grid(Y)(X) := Integer'Value (Source(F .. L));
                        J := J + 1;
                    end if;
                    
                    L := L + 1;
                end loop;
            end;
            I := I + 1;
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

    generic
        type Val_Type is (<>);
    function Min(Val_1, Val_2: in Val_Type)
        return Val_Type;
        
    function Min(Val_1, Val_2: in Val_Type)
        return Val_Type is
    begin
        if Val_2 < Val_1 then
            return Val_2;
        end if;
        return Val_1;
    end Min;

    generic
        type Val_Type is (<>);
    function Max(Val_1, Val_2: in Val_Type)
        return Val_Type;
        
    function Max(Val_1, Val_2: in Val_Type)
        return Val_Type is
    begin
        if Val_2 > Val_1 then
            return Val_2;
        end if;
        return Val_1;
    end Max;

    function Max_Bound( Solver : in Solver_Type;
                        C: in Coordinate_2D;
                        List: in Elem_Vector)
        return Positive is

        package Value_Vectors is new
        Ada.Containers.Vectors
            (Index_Type   => Positive,
             Element_Type => Positive);
        subtype Value_Vector is Value_Vectors.Vector;

        function Min_Value is new Min(Positive);
        function Max_Value is new Max(Positive);

        Line: Value_Vector;
        Column: Value_Vector;
        Max: Positive;
        MinL, MinC: Positive := 9;
        MaxL, MaxC: Positive := 1;
    begin
        --Put_Line("Enter Max_Bound");
        for V of List loop
            Max := Solver.Get(V).Get_Domain.Last_Value;
            if C.X = V.X then
                Line.Append(Max);
            else
                Column.Append(Max);
            end if;
        end loop;
        for V of Line loop
            MinL := Min_Value(MinL, V);
            MaxL := Max_Value(MaxL, V);
        end loop;
        if Line.Length = 2 and then MinL = MaxL then
            MinL := MinL - 2;
        else
            MinL := MinL - 1;
        end if;
        for V of Column loop
            MinC := Min_Value(MinC, V);
            MaxC := Max_Value(MaxC, V);
        end loop;
        if Column.Length = 2 and then MinC = MaxC then
            MinC := MinC - 2;
        else
            MinC := MinC - 1;
        end if;
        --Put_Line("Quit Max_Bound");
        return Min_Value(MinL, MinC);
    end Max_Bound;

    function Min_Bound( Solver : in Solver_Type;
                        C: in Coordinate_2D;
                        List: in Elem_Vector)
        return Positive is

        package Value_Vectors is new
        Ada.Containers.Vectors
            (Index_Type   => Positive,
             Element_Type => Positive);
        subtype Value_Vector is Value_Vectors.Vector;

        function Min_Value is new Min(Positive);
        function Max_Value is new Max(Positive);

        Line: Value_Vector;
        Column: Value_Vector;
        Min: Positive;
        MinL, MinC: Positive := 9;
        MaxL, MaxC: Positive := 1;
    begin
        --Put_Line("Enter Min_Bound");
        for V of List loop
            Min := Solver.Get(V).Get_Domain.First_Value;
            if C.X = V.X then
                Line.Append(Min);
            else
                Column.Append(Min);
            end if;
        end loop;
        for V of Line loop
            MinL := Min_Value(MinL, V);
            MaxL := Max_Value(MaxL, V);
        end loop;
        if Line.Length = 2 and then MinL = MaxL then
            MaxL := MaxL + 2;
        else
            MaxL := MaxL + 1;
        end if;
        for V of Column loop
            MinC := Min_Value(MinC, V);
            MaxC := Max_Value(MaxC, V);
        end loop;
        if Column.Length = 2 and then MinC = MaxC then
            MaxC := MaxC + 2;
        else
            MaxC := MaxC + 1;
        end if;
        --Put_Line("Quit Min_Bound");
        return Max_Value(MaxL, MaxC);
    end Min_Bound;

    function Inequal(   Solver : in out Solver_Type;
                        C: in Coordinate_2D;
                        Value: in Positive)
            return Boolean is

        Sastified : Boolean := True;
        Var: Variable_Type;
        Cur: Dependencies_Maps.Cursor;
        K: Positive;
        New_Min, New_Max, Old_Min, Old_Max: Positive;
    begin
        --Put_Line("Enter to Inequal");
        for I of List_Constraints loop
            Var := Solver.Get(I);
            if not Var.Is_Instantiated then
                Cur := Inf_Constraints.Find(I);
                if Cur /= No_Element then
                    New_Max := Max_Bound(Solver, I, Element(Cur));
                    Old_Max := Var.Get_Domain.Last_Value;
                    K := New_Max + 1;
                    loop 
                        exit when K >= Old_Max + 1 or not Sastified;
                        Sastified := Solver.Exclude(K, I);
                        K := K + 1;
                    end loop;
                end if;
            end if;
            exit when not Sastified;
        end loop;
        if Sastified then
            for I of reverse List_Constraints loop
                Var := Solver.Get(I);
                if not Var.Is_Instantiated then
                    Cur := Sup_Constraints.Find(I);
                    if Cur /= No_Element then
                        New_Min := Min_Bound(Solver, I, Element(Cur));
                        Old_Min := Var.Get_Domain.First_Value;
                        K := Old_Min;
                        loop 
                            exit when K >= New_Min or not Sastified;
                            Sastified := Solver.Exclude(K, I);
                            K := K + 1;
                        end loop;
                    end if;
                end if;
                exit when not Sastified;
            end loop;
        end if;
        --Put_Line("Quit Inequal");
        return Sastified;
    end Inequal;

    function Compare_Variable(  Var1: aliased in Variable_Type;
                                Var2: aliased in Variable_Type)
            return Boolean is
    begin
        return var1.Get_Domain_Size < Var2.Get_Domain_Size;
    end Compare_Variable;

    function Select_Value(Var: aliased in Variable_Type)
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
        Solver.Add_Constraint(Function_Ptr => Inequal'access);

        List_Constraints := Topological_Sort(Inf_Constraints);

        for I in Grid'Range loop
            for J in Grid(I)'Range loop
                if Grid(I)(J) = 0 then
                    unused := Solver.Add_Variable(Values => Domain, 
                                                  Indice => (X => J, Y => I));
                else
                    unused := Solver.Add_Variable(Values => (1 => Grid(I)(J)), 
                                                  Indice => (X => J, Y => I));
                end if;
                --Put(integer'image(Grid(I)(J)) & " ");
            end loop;
            --New_Line;
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

end Futoshiki;

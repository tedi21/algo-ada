with Solver.Variables; 
with Solver.Constraints; 
with Solver.Comparators; 
with Solver.Selectors; 
with Solver.Solutions;
with Ada.Iterator_Interfaces;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;

generic
    type Value_Type is (<>);
    type Indice_Type is private;
    with function "<" (Left, Right : Indice_Type) return Boolean is <>;
    with function "=" (Left, Right : Indice_Type) return Boolean is <>;
package Solver.Algo is

    type Solver_Type is tagged private;
    type Solver_Access is access all Solver_Type;

    package Variables is new 
    Solver.Variables 
           (Value_Type => Value_Type, 
            Indice_Type => Indice_Type);
    use Variables;
    use Domains;

    function Add_Variable(  Solver: in out Solver_Type;
                            Values: in Value_Array;
                            Indice: in Indice_Type)
        return Boolean;

    package Constraints is new
    Solver.Constraints
           (Value_Type => Value_Type,
            Indice_Type => Indice_Type,
            Solver_Type => Solver_Type);
    use Constraints;

    procedure Add_Constraint(   Solver: in out Solver_Type;
                                Function_Ptr: in not null Constraints.Function_Type);

    package Comparators is new
    Solver.Comparators
           (Variable_Type => Variable_Type);
    use Comparators;

    procedure Set_Comparator( Solver: in out Solver_Type;
                            Function_Ptr: in not null Comparators.Function_Type);

    package Selectors is new
    Solver.Selectors
           (Variable_Type => Variable_Type,
            Value_Type => Value_Type);
    use Selectors;

    procedure Set_Selector( Solver: in out Solver_Type;
                            Function_Ptr: in not null Selectors.Function_Type);


    function Exclude(   Solver: in out Solver_Type;
                        Value: in Value_Type;
                        Indice: in Indice_Type)
        return Boolean;

    function Has_Been_Rejected_Before(  Solver: in Solver_Type;
                                        Indice: in Indice_Type)
        return Boolean;

    function Exists(    Solver: in Solver_Type;
                        Indice: in Indice_Type)
        return Boolean;

    function Get(    Solver: in Solver_Type;
                     Indice: in Indice_Type)
        return Variable_type;

    package Solutions is new
       Solver.Solutions
        (Variable_Type => Variable_type);
    use Solutions;

    function Get_Solution(  Solver: in Solver_Type;
                            I: in Positive)
        return Solution_Type;

    function Get_Solution_Size(Solver: in Solver_Type)
        return Ada.Containers.Count_Type;

    function Solve(Solver: in out Solver_Type)
        return Boolean;

    procedure Solve_All(Solver: in out Solver_Type);

private

    package Variables_Maps is new
       Ada.Containers.Ordered_Maps
        (Key_Type     => Indice_Type,
         Element_Type => Variable_Type);
    use Variables_Maps;
    subtype Variables_Map is Variables_Maps.Map;

    package Indices_Vectors is new
       Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Indice_Type);
    use Indices_Vectors;
    subtype Indices_Vector is Indices_Vectors.Vector;

    package Indices_Sets is new
       Ada.Containers.Ordered_Sets
        (Element_Type => Indice_Type);
    use Indices_Sets;
    subtype Indices_Set is Indices_Sets.Set;

    package Constraints_Vectors is new
       Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Constraint_Type);
    use Constraints_Vectors;
    subtype Constraints_Vector is Constraints_Vectors.Vector;

    package Solutions_Vectors is new
       Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Solution_type);
    use Solutions_Vectors;
    subtype Solutions_Vector is Solutions_Vectors.Vector;

    -- Data

    type Solver_Type is tagged record
        Variables: Variables_Map;
        Constraints: Constraints_Vector;
        Comparator: Comparator_Type;
        Selector: Selector_Type;
        Is_Solve_In_Progress: Boolean := False;
        Additional_Indices: Indices_Vector;
        Rejected_Indices: Indices_Set;
        Processed_Indices: Indices_Vector;
        Solutions: Solutions_Vector;
    end record; 

end Solver.Algo;

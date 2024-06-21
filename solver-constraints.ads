generic
    type Value_Type is (<>);
    type Indice_Type is private;
    type Solver_Type is tagged;
package Solver.Constraints is

    type Function_Type is access function(
                Solver: in out Solver_Type;
                Indice: in Indice_Type;
                Value: in Value_Type)
            return Boolean;

    type Constraint_Type is tagged private;

    -- Primitive Subprograms

    function Bind(Function_Ptr: in not null Function_Type)
        return Constraint_Type;
   
    function Apply(Constraint: in Constraint_Type;
                Solver: in out Solver_Type;
                Indice: in Indice_Type;
                Value: in Value_Type)
        return Boolean;

    function Is_Null(Constraint: in Constraint_Type)
        return Boolean;

private

    -- Data

    type Constraint_Type is tagged record
        Function_Ptr : Function_Type := null;
    end record; 

end Solver.Constraints;

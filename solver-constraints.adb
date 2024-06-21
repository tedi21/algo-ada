package body Solver.Constraints is

    function Bind(Function_Ptr: in not null Function_Type)
                    return Constraint_Type is
        (Constraint_Type'(Function_Ptr => Function_Ptr));

    function Apply(Constraint: in Constraint_Type;
                Solver: in out Solver_Type;
                Indice: in Indice_Type;
                Value: in Value_Type)
        return Boolean is
    begin
        return Constraint.Function_Ptr.all(Solver, Indice, Value);
    end Apply;

    function Is_Null(Constraint: in Constraint_Type)
        return Boolean is
    begin
        return Constraint.Function_Ptr = null;
    end Is_Null;

end Solver.Constraints;
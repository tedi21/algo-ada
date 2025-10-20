package body Solver.Selectors is

    function Bind(Function_Ptr: in not null Function_Type)
                    return Selector_Type is
        (Selector_Type'(Function_Ptr => Function_Ptr));

    function Select_Value(  Selector: in Selector_Type;
                            Var: aliased in Variable_Type)
        return Value_Type is
    begin
        return Selector.Function_Ptr.all(Var);
    end Select_Value;

    function Is_Null(Selector: in Selector_Type)
        return Boolean is
    begin
        return Selector.Function_Ptr = null;
    end Is_Null;

end Solver.Selectors;
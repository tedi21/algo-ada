package body Solver.Comparators is

    function Bind(Function_Ptr: in not null Function_Type)
                    return Comparator_Type is
        (Comparator_Type'(Function_Ptr => Function_Ptr));

    function Compare(   Comparator: in Comparator_Type;
                        Var1: in Variable_Type;
                        Var2: in Variable_Type)
        return Boolean is
    begin
        return Comparator.Function_Ptr.all(Var1, Var2);
    end Compare;

    function Is_Null(Comparator: in Comparator_Type)
        return Boolean is
    begin
        return Comparator.Function_Ptr = null;
    end Is_Null;

end Solver.Comparators;
generic
    type Variable_Type is Private;
package Solver.Comparators is

    type Function_Type is access function(
                Var1: in Variable_Type;
                Var2: in Variable_Type)
            return Boolean;

    type Comparator_Type is tagged private;

    -- Primitive Subprograms

    function Bind(Function_Ptr: in not null Function_Type)
        return Comparator_Type;
   
    function Compare(   Comparator: in Comparator_Type;
                        Var1: in Variable_Type;
                        Var2: in Variable_Type)
        return Boolean;

    function Is_Null(Comparator: in Comparator_Type)
        return Boolean;

private

    -- Data

    type Comparator_Type is tagged record
        Function_Ptr : Function_Type := null;
    end record; 

end Solver.Comparators;

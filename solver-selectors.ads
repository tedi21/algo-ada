generic
    type Variable_Type is Private;
    type Value_Type is Private;
package Solver.Selectors is

    type Function_Type is access function(
                Var: in Variable_Type)
            return Value_Type;

    type Selector_Type is tagged private;

    -- Primitive Subprograms

    function Bind(Function_Ptr: in not null Function_Type)
        return Selector_Type;
   
    function Select_Value(  Selector: in Selector_Type;
                            Var: in Variable_Type)
        return Value_Type;

    function Is_Null(Selector: in Selector_Type)
        return Boolean;

private

    -- Data

    type Selector_Type is tagged record
        Function_Ptr : Function_Type := null;
    end record; 

end Solver.Selectors;

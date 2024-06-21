with Ada.Containers; 
use Ada.Containers;

package body Solver.Solutions is

    function Has_Element(Position: in Cursor) 
            return Boolean is
    begin 
        return Variables_Vectors.Has_Element(Position.Data);
    end Has_Element;

    function Constant_Reference(Container: aliased in Solution_Type;
                                Position: in Cursor) 
            return Constant_Reference_Type is
        (Constant_Reference_Type'
            (Variable => Container.Data.Constant_Reference(Position.Data).Element));
   
    type Iterator_Type (Data : access constant Variables_Vectors.Vector) is new Solution_Iterator_Interfaces.Reversible_Iterator with null record;

    function First (Object: in Iterator_Type) 
            return Cursor is
        (Cursor'
            (Data => Object.Data.Iterate.First));

    function Last (Object: in Iterator_Type) 
            return Cursor is
        (Cursor'
            (Data => Object.Data.Iterate.Last));
    
    function Next ( Object: in Iterator_Type; 
                    Position: in Cursor) 
            return Cursor is
        (Cursor'
            (Data => Object.Data.Iterate.Next (Position.Data)));
    
    function Previous ( Object: in Iterator_Type; 
                        Position: in Cursor) 
            return Cursor is
        (Cursor'
            (Data => Object.Data.Iterate.Previous (Position.Data)));

    function Iterate(Container: in Solution_Type) return
                Solution_Iterator_Interfaces.Reversible_Iterator'Class is
        (Iterator_Type'
            (Data => Container.Data'access));

    procedure Reserve_Capacity( Container : in out Solution_Type;
                                Capacity  : in     Solution_Size) is
    begin
        Container.Data.Reserve_Capacity(Count_Type(Capacity));
    end Reserve_Capacity;

    function Length(Container: in Solution_Type) 
            return Solution_Size is
    begin 
        return Solution_Size(Container.Data.Length);
    end Length; 

    procedure Add(  Container: in out Solution_Type;
                    Variable: in Variable_Type) is
    begin
        Container.Data.Append(Variable);
    end Add;

    procedure Sort(Container: in out Solution_Type) is
        package Variables_Vectors_Sorting is new 
            Variables_Vectors.Generic_Sorting;
        use Variables_Vectors_Sorting;
    begin
        Sort(Container.Data);
    end Sort;

end Solver.Solutions;
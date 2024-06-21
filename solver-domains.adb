with Ada.Containers; 
use Ada.Containers;

package body Solver.Domains is

    function Create(Values: in Value_Array)
                return Domain_Type is
            Set : Value_Sets.Set;
    begin 
        for E of Values loop
            Set.Insert(E);
        end loop;
        return Domain_Type'(Data => Set);
    end Create; 

    function Has_Element(Position: in Cursor) 
            return Boolean is
    begin 
        return Value_Sets.Has_Element(Position.Data);
    end Has_Element;

    function Constant_Reference(Container: aliased in Domain_Type;
                                Position: in Cursor) 
            return Constant_Reference_Type is
        (Constant_Reference_Type'
            (Value => Container.Data.Constant_Reference(Position.Data).Element));
   
    type Iterator_Type (Data : access constant Value_Sets.Set) is new Domain_Iterator_Interfaces.Reversible_Iterator with null record;

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

    function Iterate(Container: in Domain_Type) return
                Domain_Iterator_Interfaces.Reversible_Iterator'Class is
        (Iterator_Type'
            (Data => Container.Data'access));

    function Length(Container: in Domain_Type) 
            return Domain_Size is
    begin 
        return Domain_Size(Container.Data.Length);
    end Length; 

    function Contains(  Container: in Domain_Type;
                        Value: in Value_Type) 
            return Boolean is
    begin
        return Container.Data.Contains(Value);
    end Contains;

    procedure Exclude(  Container: in out Domain_Type;
                        Value: in Value_Type) is
    begin 
        Container.Data.Exclude(Value);
    end Exclude; 

    procedure Clear(Container: in out Domain_Type) is
    begin 
        Container.Data.Clear;
    end Clear; 

    procedure Include(  Container: in out Domain_Type;
                        Value: in Value_Type) is
    begin 
        Container.Data.Include(Value);
    end Include; 

    function First_Value(Container: in Domain_Type)
            return Value_Type is
    begin
        return Container.Data.First_Element;
    end First_Value;

    function Last_Value(Container: in Domain_Type)
            return Value_Type is
    begin
        return Container.Data.Last_Element;
    end Last_Value;

end Solver.Domains;
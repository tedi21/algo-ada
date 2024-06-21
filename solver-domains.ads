with Ada.Containers.Ordered_Sets;
with Ada.Iterator_Interfaces; 

generic
    type Value_Type is (<>);
package Solver.Domains is

    type Value_Array is array (Positive range <>) of Value_Type;
    type Domain_Size is new Ada.Containers.Count_Type;
    type Domain_Type is tagged private
        with Constant_Indexing => Constant_Reference,
              Default_Iterator => Iterate,
              Iterator_Element => Value_Type;

    type Cursor is private;

    function Has_Element(Position: Cursor)
            return Boolean;

    package Domain_Iterator_Interfaces is
            new Ada.Iterator_Interfaces(Cursor, Has_Element);

    type Constant_Reference_Type
        (Value: not null access constant Value_Type) is null record
        with Implicit_Dereference => Value;

    -- Primitive Subprograms

    function Create(Values: in Value_Array)
                return Domain_Type;
   
    function Constant_Reference(Container: aliased in Domain_Type;
                                Position: in Cursor) 
            return Constant_Reference_Type;
   
    function Iterate(Container: in Domain_Type) 
            return Domain_Iterator_Interfaces.Reversible_Iterator'Class;

    function Length(Container: in Domain_Type)
            return Domain_Size;

    function Contains(  Container: in Domain_Type;
                        Value: in Value_Type) 
            return Boolean;

    procedure Exclude(  Container: in out Domain_Type;
                        Value: in Value_Type);

    procedure Clear(Container: in out Domain_Type);

    procedure Include(  Container: in out Domain_Type;
                        Value: in Value_Type);

    function First_Value(Container: in Domain_Type)
            return Value_Type;

    function Last_Value(Container: in Domain_Type)
            return Value_Type;
    
private
    package Value_Sets is new
       Ada.Containers.Ordered_Sets
        (Element_Type => Value_Type);

    -- Data

    type Cursor is record
        Data : Value_Sets.Cursor;
    end record;

    type Domain_Type is tagged record
        Data : aliased Value_Sets.Set;
    end record; 

end Solver.Domains;

with Ada.Containers.Vectors;
with Ada.Iterator_Interfaces; 

generic
    type Variable_Type is Private;
    with function "<" (Left, Right : Variable_Type) return Boolean is <>;
package Solver.Solutions is

    type Solution_Size is new Ada.Containers.Count_Type;
    type Solution_Type is tagged private
        with Constant_Indexing => Constant_Reference,
              Default_Iterator => Iterate,
              Iterator_Element => Variable_Type;

    type Cursor is private;

    function Has_Element(Position: Cursor)
            return Boolean;

    package Solution_Iterator_Interfaces is
            new Ada.Iterator_Interfaces(Cursor, Has_Element);

    type Constant_Reference_Type
        (Variable: not null access constant Variable_Type) is null record
        with Implicit_Dereference => Variable;

    -- Primitive Subprograms
  
    function Constant_Reference(Container: aliased in Solution_Type;
                                Position: in Cursor) 
            return Constant_Reference_Type;
   
    function Iterate(Container: in Solution_Type) 
            return Solution_Iterator_Interfaces.Reversible_Iterator'Class;

    procedure Reserve_Capacity( Container : in out Solution_Type;
                                Capacity  : in     Solution_Size);

    function Length(Container: in Solution_Type)
            return Solution_Size;

    procedure Add(  Container: in out Solution_Type;
                    Variable: in Variable_Type);

    procedure Sort(Container: in out Solution_Type);
    
private
    package Variables_Vectors is new
       Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Variable_Type);

    -- Data

    type Cursor is record
        Data : Variables_Vectors.Cursor;
    end record;

    type Solution_Type is tagged record
        Data : aliased Variables_Vectors.Vector;
    end record; 

end Solver.Solutions;

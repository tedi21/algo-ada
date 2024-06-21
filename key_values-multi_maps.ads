with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Iterator_Interfaces; 

generic
    type Elem_Type is Private;
package Key_Values.Multi_Maps is
    type Multi_Map_Size is new Ada.Containers.Count_Type;

    package Elem_Vectors is new
    Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Elem_Type);
    subtype Elem_Vector is Elem_Vectors.Vector;

    type Multi_Map is tagged private
        with Constant_Indexing => Constant_Reference,
            Default_Iterator => Iterate,
            Iterator_Element => Elem_Vector;

    type Cursor is private;

    No_Element : constant Cursor;

    function Key (Position : Cursor) 
            return Key_Type;

    function Element (Position : Cursor) 
            return Elem_Vector;

    function Has_Element(Position: Cursor)
            return Boolean;

    package Multi_Map_Iterator_Interfaces is
            new Ada.Iterator_Interfaces(Cursor, Has_Element);

    type Constant_Reference_Type
        (Elements: not null access constant Elem_Vector) is null record
        with Implicit_Dereference => Elements;

    -- Primitive Subprograms

    function Constant_Reference(Container: aliased in Multi_Map;
                                Position: in Cursor) 
            return Constant_Reference_Type;

    function Iterate(Container: in Multi_Map) 
            return Multi_Map_Iterator_Interfaces.Reversible_Iterator'Class;

    function Length(Container: in Multi_Map)
            return Multi_Map_Size;

    function Element (  Container: in Multi_Map;
                        Key: in Key_Type)
            return Elem_Vector;

    procedure Insert(   Container: in out Multi_Map;
                        Key: in Key_Type;
                        Elem: in Elem_Type);

    function Find(  Container: in Multi_Map;
                    Key: in Key_Type)
            return Cursor;

    function Contains(  Container: in Multi_Map;
                        Key: in Key_Type)
            return Boolean;
    
private

    use Elem_Vectors;
    package Elem_Maps is new
    Ada.Containers.Ordered_Maps
        (Key_Type     => Key_Type,
            Element_Type => Elem_Vector);

    -- Data

    type Cursor is record
        Data : Elem_Maps.Cursor;
    end record;

    No_Element : constant Cursor := Cursor'(Data => Elem_Maps.No_Element);

    type Multi_Map is tagged record
        Data : aliased Elem_Maps.Map;
    end record; 

end Key_Values.Multi_Maps;
package body Key_Values.Multi_Maps is

    function Key (Position : in Cursor) 
            return Key_Type is 
    begin 
        return Elem_Maps.Key(Position.Data);
    end Key;

    function Element (Position : Cursor) 
            return Elem_Vector is
    begin 
        return Elem_Maps.Element(Position.Data);
    end Element;

    function Has_Element(Position: in Cursor) 
            return Boolean is
    begin 
        return Elem_Maps.Has_Element(Position.Data);
    end Has_Element;

    function Constant_Reference(Container: aliased in Multi_Map;
                                Position: in Cursor) 
            return Constant_Reference_Type is
        (Constant_Reference_Type'
            (Elements => Container.Data.Constant_Reference(Position.Data).Element));

    type Iterator_Type (Data : access constant Elem_Maps.Map) is new Multi_Map_Iterator_Interfaces.Reversible_Iterator with null record;

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

    function Iterate(Container: in Multi_Map) return
                Multi_Map_Iterator_Interfaces.Reversible_Iterator'Class is
        (Iterator_Type'
            (Data => Container.Data'access));

    function Length(Container: in Multi_Map) 
            return Multi_Map_Size is
    begin 
        return Multi_Map_Size(Container.Data.Length);
    end Length; 

    use Elem_Maps;

    procedure Insert(   Container: in out Multi_Map;
                        Key: in Key_Type;
                        Elem: in Elem_Type) is
        Cur: constant Elem_Maps.Cursor := Container.Data.Find(Key);
    begin
        if Cur /= Elem_Maps.No_Element then
            Container.Data(Key).Append(Elem);
        else
            Container.Data.Insert(Key, To_Vector(Elem, 1));
        end if;
    end Insert;

    function Element (  Container: in Multi_Map;
                        Key: in Key_Type)
            return Elem_Vector is 
    begin
        return Container.Data.Element(Key);
    end Element;

    function Find(  Container: in Multi_Map;
                    Key: in Key_Type)
            return Cursor is
        (Cursor'
            (Data => Container.Data.Find (Key)));

    function Contains(  Container: in Multi_Map;
                        Key: in Key_Type)
        return Boolean is 
    begin
        return Container.Data.Contains(Key);
    end Contains;

end Key_values.Multi_Maps;
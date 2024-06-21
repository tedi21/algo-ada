with Ada.Containers.Ordered_Sets;

package body Key_Values.Dependencies is 

    package Key_Sets is new
    Ada.Containers.Ordered_Sets
        (Element_Type => Key_Type);
    subtype Key_Set is Key_Sets.Set;

    procedure Parse_Dependencies(Container: in Dependency_Map;
                                 Vertice: in Key_Type;
                                 Visited: in out Key_Set;
                                 Stack: in out Dependency_Vector) is
        use Dependencies_Maps;
        C: Cursor;
    begin
        Visited.Include(Vertice);
        C := Container.Find(Vertice);
        if C /= No_Element then
            for D of Element(C) loop
                if not Visited.Contains(D) then
                    Parse_Dependencies(Container, D, Visited, Stack);
                end if; 
            end loop; 
        end if;
        Stack.Append(Vertice);
    end Parse_Dependencies;

    function Topological_Sort(Container: in Dependency_Map)
            return Dependency_Vector is
        use Dependencies_Maps;
        Vertices: Key_Set;
        Visited: Key_Set;
        Sorted: Dependency_Vector;
    begin
        for C in Container.Iterate loop
            Vertices.Include( Key(C) );
            for E of Container.Constant_Reference(C) loop
                Vertices.Include( E );
            end loop;
        end loop;
        Sorted.Reserve_Capacity(Vertices.Length);
        for V of Vertices loop
            if not Visited.Contains(V) then
                Parse_Dependencies(Container, V, Visited, Sorted);
            end if;
        end loop;
        return Sorted;
    end Topological_Sort;

end Key_Values.Dependencies;
with Ada.Containers.Vectors;
with Key_Values.Multi_Maps;    

generic
package Key_Values.Dependencies is 

    package Dependencies_Maps is new 
        Multi_Maps(Key_Type);
    subtype Dependency_Map is Dependencies_Maps.Multi_Map;
    subtype Dependency_Map_Size is Dependencies_Maps.Multi_Map_Size;

    package Dependencies_Vectors is new
    Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Key_type);
    subtype Dependency_Vector is Dependencies_Vectors.Vector;

    function Topological_Sort(Container: in Dependency_Map)
            return Dependency_Vector;

end Key_Values.Dependencies;
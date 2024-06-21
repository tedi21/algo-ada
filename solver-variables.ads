with Solver.Domains;

generic
    type Value_Type is (<>);
    type Indice_Type is private;
    with function "<" (Left, Right : Indice_Type) return Boolean is <>;
package Solver.Variables is

    package Domains is new
       Solver.Domains
        (Value_Type => Value_Type);
    use Domains;

    type Variable_Type is tagged private;
    type Variable_Access is access all Variable_Type;

    -- Primitive Subprograms

    function Create(    Indice: in Indice_Type;
                        Values: in Value_Array)
                return Variable_Type;

    function Is_Instantiated(Var: in Variable_Type)
            return Boolean;

    function Is_Compromised(Var: in Variable_Type)
            return Boolean;

    function Get_Domain_Size(Var: in Variable_Type)
            return Domains.Domain_Size;

    function Get_Domain(Var: aliased in Variable_Type)
            return not null access constant Domains.Domain_Type;

    procedure Exclude(  Var: in out Variable_Type;
                        Value: in Value_Type);

    procedure Set(      Var: in out Variable_Type;
                        Value: in Value_Type);

    function Can_Be(    Var: in Variable_Type;
                        Value: in Value_Type) 
            return Boolean;

    function Get_Value(Var: in Variable_Type)
            return Value_Type;

    function Get_Indice(Var: in Variable_Type)
            return Indice_Type;

    function "<"(  V1: in Variable_Type;
                   V2: in Variable_Type) 
      return Boolean;

private

    -- Data

    type Variable_Type is tagged record
        Indice : Indice_Type;
        Domain : aliased Domains.Domain_Type;
    end record; 

end Solver.Variables;

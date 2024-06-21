
package body Solver.Variables is

    function Create(    Indice: in Indice_Type;
                        Values: in Value_Array)
                return Variable_Type is
        (Variable_Type'
            (Indice => Indice,
             Domain => Create(Values)));
   
    function Is_Instantiated(Var: in Variable_Type) 
            return Boolean is
    begin 
        return Var.Domain.Length = 1;
    end Is_Instantiated;

    function Is_Compromised(Var: in Variable_Type)
            return Boolean is
    begin 
        return Var.Domain.Length = 0;
    end Is_Compromised;

    function Get_Domain_Size(Var: in Variable_Type)
            return Domain_Size is
    begin 
        return Domain_Size(Var.Domain.Length);
    end Get_Domain_Size;

    function Get_Domain(Var: aliased in Variable_Type)
            return not null access constant Domain_Type is
    begin
        return Var.Domain'access;
    end Get_Domain;

    procedure Exclude(  Var: in out Variable_Type;
                        Value: in Value_Type) is
    begin
        Var.Domain.Exclude(Value);
    end Exclude;

    procedure Set(  Var: in out Variable_Type;
                    Value: in Value_Type) is
    begin
        Var.Domain.Clear;
        Var.Domain.Include(Value);
    end Set;

    function Can_Be(Var: in Variable_Type;
                    Value: in Value_Type) 
            return Boolean is
    begin
        return Var.Domain.Contains(Value);
    end Can_Be;

    function Get_Value(Var: in Variable_Type)
            return Value_Type is
    begin
        return Var.Domain.First_Value;
    end Get_Value;

    function Get_Indice(Var: in Variable_Type)
            return Indice_Type is
    begin
        return Var.Indice;
    end Get_Indice;

    function "<"(  V1: in Variable_Type; 
                   V2: in Variable_Type) 
      return Boolean is
    begin
        return V1.Indice < V2.Indice;
    end;

end Solver.Variables;
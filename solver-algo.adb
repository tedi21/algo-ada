with Ada.Containers;
use Ada.Containers;

with Ada.Text_IO; 
use Ada.Text_IO; 

package body Solver.Algo is

    package Variables_Access_Vectors is new
       Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Variable_Access);
    use Variables_Access_Vectors;
    subtype Variables_Access_Vector is Variables_Access_Vectors.Vector;

    package Variables_Vectors is new
       Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Variable_Type);
    use Variables_Vectors;
    subtype Variables_Vector is Variables_Vectors.Vector;

    type Backup_Type is record
        Processed_Indices: Indices_Vector;
        Remaining_Variables: Variables_Vector;
        Choice_Indice: Indice_Type;
        Choice_Value: Value_Type;
    end record; 

    package Backup_Vectors is new
       Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Backup_Type);
    use Backup_Vectors;
    subtype Backup_Vector is Backup_Vectors.Vector;

    procedure Sort_Variables(   Solver: in Solver_Type;
                                Processing_Variables: in out Variables_Access_Vector) is

        Comparator: Comparator_Type := Solver.Comparator;

        function Compare_Variables( Var1: in Variable_Access;
                                    Var2: in Variable_Access)
            return Boolean is 

            Is_Less: Boolean := False;
            Valid1: constant Boolean := not Var1.Is_Compromised and not Var1.Is_Instantiated;
            Valid2: constant Boolean := not Var2.Is_Compromised and not Var2.Is_Instantiated;
        begin
            if Valid1 and Valid2 then
                Is_Less := Comparator.Compare(Var1.all, Var2.all);
            elsif Valid1 then
                Is_Less := True;
            end if;
            return Is_Less;
        end Compare_Variables;

        package Variables_Access_Vectors_Sorting is new 
            Variables_Access_Vectors.Generic_Sorting
            (Compare_Variables);
        use Variables_Access_Vectors_Sorting;

    begin 
        Sort(Processing_Variables);
    end Sort_Variables;

    function Backtracking(  Solver: in out Solver_Type;
                            Backup: in out Backup_Vector)
        return Variables_Access_Vector is

        Save: Backup_Type := Backup.Last_Element;
        Processing_Variables: Variables_Access_Vector;
        C: Indices_Vectors.Cursor;
    begin
        -- Put_Line("Backtracking");
        Backup.Delete_Last;
        -- Copier les variables non viables
        C := Solver.Processed_Indices.To_Cursor(Integer(Save.Processed_Indices.Length + 1));
        while C /= Indices_Vectors.No_Element loop
            Solver.Rejected_Indices.Insert( Solver.Processed_Indices(C) );
            Solver.Variables.Delete( Solver.Processed_Indices(C) );
            C := Next(C);
        end loop;
        for I of Solver.Additional_Indices loop
            Solver.Rejected_Indices.Insert( I );
            Solver.Variables.Delete( I );
        end loop;
        -- Restaure les variables
        Processing_Variables.Reserve_Capacity(Save.Remaining_Variables.Length);
        for V of Save.Remaining_Variables loop
            Solver.Variables(V.Get_Indice) := V;
            Processing_Variables.Append( Solver.Variables.Reference(V.Get_Indice).Element );        
        end loop;
        Solver.Processed_Indices := Save.Processed_Indices;
        Solver.Additional_Indices.Clear;
        -- Elimine la valeur déjà analysée de la variable
        Solver.Variables(Save.Choice_Indice).Exclude(Save.Choice_Value);
        -- Put_Line("End Backtracking");
        return Processing_Variables;
    end Backtracking;

    function Select_Variable_And_Value( Solver: in Solver_Type;
                                        Processing_Variables: in out Variables_Access_Vector)
        return Backup_Type is

        Var: Variable_Access;
        Val: Value_Type;
        Save: Backup_Type;
    begin
        -- Put_Line("Select_Variable_And_Value");
        -- Trie des variables et sélection de la première variable
        if not Solver.Comparator.Is_Null then
            Solver.Sort_Variables(Processing_Variables);
        end if;
        Var := Processing_Variables.First_Element;
        Val := Var.Get_Value;
        -- Sélectionne la valeur
        if not Solver.Selector.Is_Null then
            Val := Solver.Selector.Select_Value(Var.all);
        end if;
        -- Copie du contexte
        Save.Processed_Indices := Solver.Processed_Indices;
        Save.Remaining_Variables.Reserve_Capacity(Processing_Variables.Length);
        for V of Processing_Variables loop
            Save.Remaining_Variables.Append(V.all);
        end loop;
        Save.Choice_Indice := Var.Get_Indice;
        Save.Choice_Value := Val;
        -- Définit le choix de la valeur
        Var.Set(Val);
        -- Put_Line("End Select_Variable_And_Value");
        return Save;
    end Select_Variable_And_Value;

    function Partition_And_Constrain(   Solver: in out Solver_Type;
                                        Processing_Variables: in out Variables_Access_Vector;
                                        Sastified: in out Boolean)
        return Boolean is

      Instantiated_Vars: Variables_Access_Vector;
      No_Instantiated_Vars: Variables_Access_Vector;
    begin
        -- Put_Line("Partition_And_Constrain");
        -- TANT QUE (Existe nouvelles variables instanciées) FAIRE 
        --   partitionner toutes les nouvelles variables qui sont instanciées parmi les variables du problème
        --   POUR CHAQUE (Nouvelles variables instanciées)
        --     POUR CHAQUE (Contraintes)
        --       Appliquer les contraintes pour réduire le domaine des variables constituant le problème
        loop
            -- Ajout des nouvelles variables créées pendant la recherche
            Processing_Variables.Reserve_Capacity(Solver.Additional_Indices.Length + Processing_Variables.Length);
            Solver.Processed_Indices.Reserve_Capacity(Solver.Additional_Indices.Length + Solver.Processed_Indices.Length);
            for I of Solver.Additional_Indices loop
                Processing_Variables.Append( Solver.Variables.Reference(I).Element );
                Solver.Processed_Indices.Append( I );
            end loop;
            Solver.Additional_Indices.Clear;

            -- Partition des variables
            Instantiated_Vars.Clear;
            No_Instantiated_Vars.Clear;
            for V of Processing_Variables loop
                if V.Is_Instantiated then
                    Instantiated_Vars.Append(V);
                else
                    No_Instantiated_Vars.Append(V);
                end if;
            end loop;

            -- Applique les contraintes
            for V of Instantiated_Vars loop
                for C of Solver.Constraints loop
                    Sastified := C.apply(Solver, V.Get_Indice, V.Get_Value);
                    exit when not Sastified;
                end loop;
                exit when not Sastified;
            end loop;

            -- Recommencer tant qu'il y a des nouvelles variables instanciées parmi celles non instanciées
            Processing_Variables := No_Instantiated_Vars;

            exit when not Sastified 
                      or else (
                         Solver.Additional_Indices.Is_Empty 
                         and then (No_Instantiated_Vars.Is_Empty or else Instantiated_Vars.Is_Empty));
        end loop;
        -- Put_Line("End Partition_And_Constrain");
        return Sastified and No_Instantiated_Vars.Length > 0;
    end Partition_And_Constrain;

    function Search_Solution(  Solver: in out Solver_Type;
                               Processing_Variables: in out Variables_Access_Vector;
                               Backup: in out Backup_Vector)
        return Boolean is

        Is_Solution: Boolean := False;
        Sastified: Boolean := true;
    begin
        -- Put_Line("Search_Solution");
        -- TANT QUE (la solution n'est pas trouvé et le problème est toujours résolvable) FAIRE 
        --   réduire le nombre de variable constituant le problème et vérifier la cohérence du problème
        --   si une solution partielle est trouvée, déterminer un point de choix
        --   sinon si une solution est trouvée, arrêter
        --   sinon si un point de choix existe, revenir en arrière (la résolution du problème est incohérente)
        --   sinon arrêter (le problème est insoluble)
        loop
            if Processing_Variables.Length > 0 then
                -- Réduit le problème en appliquant les contraintes
                if Solver.Partition_And_Constrain(Processing_Variables, Sastified) then
                    -- Mémorise le contexte et le choix pour le backtracking
                    Backup.Append(Solver.Select_Variable_And_Value(Processing_Variables));
                    -- Is_Solution = False, Sastified = True
                else
                    Is_Solution := Sastified;
                    --    Is_Solution = True, Sastified = True
                    -- or Is_Solution = False, Sastified = False
                end if;
            else
                Sastified := False;
                Is_Solution := False;
                -- Is_Solution = False, Sastified = False
            end if;
            -- Mauvais choix ou recherche d'une autre solution, réinitialise les variables
            if not Sastified and then Backup.Length > 0 then
                Processing_Variables := Solver.Backtracking(Backup);
                Sastified := True;
            end if;
            exit when Is_Solution or else not Sastified; 
        end loop;
        -- Put_Line("End Search_Solution " & Boolean'image(Is_Solution));
        return Is_Solution;
    end Search_Solution;

    function Intern_Solve(  Solver: in out Solver_Type;
                            Find_All: in Boolean)
        return Boolean is

        Next_Solution: Boolean := True;
        Processing_Variables: Variables_Access_Vector;
        Backup: Backup_Vector;
        Solution: Solution_Type;
    begin
        -- Put_Line("Intern_Solve");
        -- Démarrage de la recherche de solution
        Solver.Is_Solve_In_Progress := True;

        -- Initialisation des indices des variables utilisées pour la recherche de solution
        Solver.Processed_Indices.Clear;

        -- Initialisation des variables utilisées pour la recherche de solution
        Processing_Variables.Reserve_Capacity(Solver.Variables.Length);
        Solver.Processed_Indices.Reserve_Capacity(Solver.Variables.Length);
        for C in Solver.Variables.Iterate loop
            Processing_Variables.Append( Solver.Variables.Reference(C).Element );
            Solver.Processed_Indices.Append( Key(C) );
        end loop;

        -- TANT QUE (des solutions existent) FAIRE 
        --   rechercher une solution
        --   si une solution existe, mémoriser la solution et passer à la suivante
        --   sinon arrêter
        loop
            -- Recherche une solution
            if Solver.Search_Solution(Processing_Variables, Backup) then
                -- Mémorise la solution
                -- Recopie des variables dans l'ordre de leurs traitements
                Solution.Reserve_Capacity(Solution_Size(Solver.Processed_Indices.Length));
                for I of Solver.Processed_Indices loop
                    Solution.Add(Solver.Variables(I));
                end loop;
                Solver.Solutions.Append(Solution);
                Next_Solution := Find_All and then Backup.Length > 0;
            else
                Next_Solution := False;
            end if;
            exit when not Next_Solution;
        end loop;
        Solver.Is_Solve_In_Progress := False;
        -- Put_Line("End Intern_Solve");
        return Solver.Solutions.Length > 0;
    end Intern_Solve;

    function Add_Variable(  Solver: in out Solver_Type;
                            Values: in Value_Array;
                            Indice: in Indice_Type)
        return Boolean is

        Success: Boolean;
        C: Variables_Maps.Cursor;
    begin
        Solver.Variables.Insert(Key => Indice, 
                                New_Item => Create(Indice, Values), 
                                Position => C,
                                Inserted => Success);
        if Success and Solver.Is_Solve_In_Progress then
            Solver.Additional_Indices.Insert(Solver.Additional_Indices.Last, Indice);
        end if;
        return Success;
    end Add_Variable;

    procedure Add_Constraint(   Solver: in out Solver_Type;
                                Function_Ptr: in not null Constraints.Function_Type) is
    begin
        Solver.Constraints.Insert(Solver.Constraints.Last, Bind(Function_Ptr));
    end Add_Constraint;

    procedure Set_Comparator(   Solver: in out Solver_Type;
                                Function_Ptr: in not null Comparators.Function_Type) is
    begin
        Solver.Comparator := Bind(Function_Ptr);
    end Set_Comparator;

    procedure Set_Selector( Solver: in out Solver_Type;
                            Function_Ptr: in not null Selectors.Function_Type) is
    begin
        Solver.Selector := Bind(Function_Ptr);
    end Set_Selector;

    function Exclude(   Solver: in out Solver_Type;
                        Value: in Value_Type;
                        Indice: in Indice_Type)
        return Boolean is

        Cur: constant Variables_Maps.Cursor := Solver.Variables.Find(Indice);
        Success: Boolean := false;
    begin
        if Cur /= Variables_Maps.No_Element then
            if not Solver.Variables(Cur).Is_Instantiated and
               not Solver.Variables(Cur).Is_Compromised then
                Solver.Variables(Cur).Exclude(Value);
                Success := true;
            else
                Success := not Solver.Variables(Cur).Can_Be(Value);
            end if;
        end if;
        return Success;
    end Exclude;

    function Has_Been_Rejected_Before(  Solver: in Solver_Type;
                                        Indice: in Indice_Type)
        return Boolean is

        Cur: constant Indices_Sets.Cursor := Solver.Rejected_Indices.Find(Indice);
    begin
        return Cur /= Indices_Sets.No_Element;
    end Has_Been_Rejected_Before;

    function Exists(    Solver: in Solver_Type;
                        Indice: in Indice_Type)
        return Boolean is

        Cur: constant Variables_Maps.Cursor := Solver.Variables.Find(Indice);
    begin
        return Cur /= Variables_Maps.No_Element;
    end Exists;

    function Get(    Solver: in Solver_Type;
                     Indice: in Indice_Type)
        return Variable_type is

        Cur: constant Variables_Maps.Cursor := Solver.Variables.Find(Indice);
    begin
        return Solver.Variables(Cur);
    end Get;

    function Get_Solution(  Solver: in Solver_Type;
                            I: in Positive)
        return Solution_Type is
    begin
        return Solver.Solutions( I );
    end Get_Solution;

    function Get_Solution_Size(  Solver: in Solver_Type)
        return Ada.Containers.Count_Type is
    begin
        return Solver.Solutions.Length;
    end Get_Solution_Size;

    function Solve(Solver: in out Solver_Type)
        return Boolean is
    begin
        return Solver.Intern_Solve(false);
    end Solve;

    procedure Solve_All(Solver: in out Solver_Type) is
        unused: Boolean;
    begin
        unused := Solver.Intern_Solve(true);
    end Solve_All;
    
end Solver.Algo;

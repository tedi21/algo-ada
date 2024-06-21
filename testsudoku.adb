with Ada.Text_IO; 
use Ada.Text_IO; 

with ada.Strings.Unbounded;
use ada.Strings.Unbounded;

with Ada.Real_Time; 
use Ada.Real_Time;

with Sudoku;
use Sudoku;

procedure TestSudoku is 

    function "+" (Source: in String) return Unbounded_String
        renames To_Unbounded_String;

    Input_Array: constant Strings :=   (+"8 0 0 0 0 0 0 4 0",
                                        +"3 0 0 8 0 0 5 6 0",
                                        +"0 0 2 0 0 3 0 0 0",
                                        +"5 0 0 0 0 0 0 0 4",
                                        +"0 0 7 0 6 0 9 5 0",
                                        +"0 0 0 9 0 0 0 0 2",
                                        +"2 0 0 6 0 0 8 3 0",
                                        +"0 0 0 0 0 0 0 0 9",
                                        +"0 1 0 0 7 0 0 0 0");

   Start_Time, Stop_Time : Time;
   Elapsed_Time          : Time_Span;

begin 
    Put_Line ("Hello");
    
    Input (Input_Array);
    Initialize;
    Start_Time := Clock;
    if Solve then
        Stop_Time    := Clock;
        Elapsed_Time := Stop_Time - Start_Time;
        Put_Line ("Elapsed time: "
             & Duration'Image
                 (To_Duration (Elapsed_Time))
             & " seconds");
        Show;
    end if;
end TestSudoku;

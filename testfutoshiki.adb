with Ada.Text_IO; 
use Ada.Text_IO; 

with ada.Strings.Unbounded;
use ada.Strings.Unbounded;

with Ada.Real_Time; 
use Ada.Real_Time;

with Futoshiki;
use Futoshiki;

procedure TestFutoshiki is 

    function "+" (Source: in String) return Unbounded_String
        renames To_Unbounded_String;

    Input_Array: constant Strings :=   (+"0   0 < 0   0   0   4   5   7   3",
                                        +"                                 ",
                                        +"1   0   0   0 > 0   6 < 0 < 0   7",
                                        +"            ^                   v",
                                        +"0 > 0   0 < 4 > 0   7 < 0   0 < 0",
                                        +"                                 ",
                                        +"0   0   0 < 0   0   0   0   0   0",
                                        +"^                   v            ",
                                        +"0 < 0   5 > 0   0   0   0   0   0",
                                        +"v               v                ",
                                        +"0   0   0   0   0 < 0   0   0   0",
                                        +"                v                ",
                                        +"0   0   0   0   0   3 < 0   0   0",
                                        +"            v   v                ",
                                        +"0   0   0 < 7   0   9   0 < 5   2",
                                        +"                v           v    ",
                                        +"0   0   0   0   0 < 0   1   0   0");

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
end TestFutoshiki;

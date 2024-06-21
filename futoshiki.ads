with Coordinates;
use Coordinates;

with Solver.Algo;

with ada.Strings.Unbounded;

package Futoshiki is

    type Strings is array (Positive range <>) of ada.Strings.Unbounded.Unbounded_String;

    procedure Input(Problem: in Strings);
  
    procedure Initialize;

    function Solve
        return Boolean;

    procedure Show;

end Futoshiki;

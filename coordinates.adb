package body Coordinates is

   function "<"(  C1: in Coordinate_1D; 
                  C2: in Coordinate_1D) 
      return Boolean is
    begin
        return C1.X < C2.X;
    end;

   function "<"(  C1: in Coordinate_2D;
                  C2: in Coordinate_2D) 
      return Boolean is
    begin
        return C1.Y < C2.Y  
            or else 
               (C1.Y = C2.Y and Coordinate_1D(C1) < Coordinate_1D(C2));
    end;

end Coordinates;
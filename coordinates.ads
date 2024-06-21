package Coordinates is

   type Coordinate_1D is tagged record
      X : Positive;
   end record;

   function "<"(  C1: in Coordinate_1D; 
                  C2: in Coordinate_1D) 
      return Boolean;

   type Coordinate_2D is new Coordinate_1D with record
      Y : Positive;
   end record;

   function "<"(  C1: in Coordinate_2D;
                  C2: in Coordinate_2D) 
      return Boolean;

end Coordinates;
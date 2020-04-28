# 3DObject_Relation
Detect relationship of 3D objects based on their boundary coordinates

This is an interval-based 3D object relationship detection algorithm.

As any 3D object can be easily expressed as their boundaries on three coordinates 
(i.e. maximum and minimum on x coordinate, maximum and minimum on y coordinate, and maximum and minimum on z coordinate), 
the relationships between 3D objects can be first detected on each coordinate, 
then conclude based on results on all coordinates. 

Built upon package "intervals" 
3DObject_Relation takes four arguments: TableFrom, TableTo, ClosedFrom, ClosedTo; 
and returns a result table in the form of TableTo with two additional columns – “Within ID” and “Overlap ID”. 
Input table TableFrom and TableTo each has 6 columns, identifying the boundaries of the 3D objects 
(i.e. i.e. maximum and minimum on x coordinate, maximum and minimum on y coordinate, and maximum and minimum on z coordinate). 
ClosedFrom, and ClosedTo each is a two-element vector with either “TRUE” or “FALSE” required to indicate whether endpoints are included (“TRUE” for including endpoint)

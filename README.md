# 3DObject_Relation
Detect relationship of 3D objects based on their boundary coordinates

This is an interval-based 3D object relationship detection algorithm.

As any 3D object can be easily expressed as their boundaries on three coordinates 
(i.e. maximum and minimum on x coordinate, maximum and minimum on y coordinate, and maximum and minimum on z coordinate), 
the relationships between 3D objects can be first detected on each coordinate, 
then conclude based on results on all coordinates. 

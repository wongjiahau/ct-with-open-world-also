{

False && x   = False;
True  && x   = x;

(==) [] []         =  True;
(==) (x:xs) (y:ys) =  x==y && xs==ys;
(==) _ _           =  False;

overload
(==) = primEqInt;

overload
(==) = primEqChar;

overload
(==) = primEqFloat;

overload
(==) = primEqInteger;

f = [] == []

}
{
False && x   = False;
True  && x   = x;


(==) = primEqInt;

overload
(==) = primEqChar;

overload
(==) = primEqFloat;

overload
(==) = primEqInteger;

(==) :: [a] -> [a] -> Bool;
(==) [] []         =  True;
(==) (x:xs) (y:ys) =  x==y && xs==ys;
(==) _ _           =  False;

o a = 1;
overload o a = 1.0;
overload o a = if t a == [[a]] then [a] else [a];

t a = 1;
overload t a = 1.0;
overload t a = [o a]  
 

}


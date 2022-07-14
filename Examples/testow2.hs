{


assume (==) :: a -> a -> Bool;

data Arvore a = No a (Arvore a) (Arvore a) | Folha;


Folha         == Folha          = True;
(No a1 e1 d1) == (No a2 e2 d2)  = a1 == a2 && e1 == e2 && d1 == d2;
_             == _ = False;


False && x   = False;
True  && x   = x;


-- overload
-- (==) = primEqInt;

overload
(==) = primEqFloat;

overload
(==) [] []         =  True;
(==) (x:xs) (y:ys) =  x==y && xs==ys;
(==) _ _           =  False;

(-) = primMinusInt;

overload
(-) = primMinusFloat;


negate x = (coerce 0) - x;

foo1 x y = (coerce x) == y;

foo2 x y = [x] == y; 

foo3 x y = [x] == y && coerce x == 1.0;


neg True  = False;
neg False = True;

overload
neg 0 = coerce 0;
neg x   = -(coerce  x);  

foo4 x y = neg x == x && y == coerce x;

}
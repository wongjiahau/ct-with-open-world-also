{


assume (==) :: a -> a -> Bool;

data Arvore a = No a (Arvore a) (Arvore a) | Folha;


Folha         == Folha          = True;
(No a1 e1 d1) == (No a2 e2 d2)  = a1 == a2 && e1 == e2 && d1 == d2;
_             == _ = False;


False && x   = False;
True  && x   = x;


overload
(==) = primEqInt;

overload
(==) = primEqDouble;

overload
True == True = True;

overload
(==) = primEqChar;

overload
(==) [] []         =  True;
(==) (x:xs) (y:ys) =  x==y && xs==ys;
(==) _ _           =  False;




f x y = (coerce x) == y;

teste x y = x == y;

teste2 x y = x == y && g x; 

g '1' = True;

overload 
g True = True;


}
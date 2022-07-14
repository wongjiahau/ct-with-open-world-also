{

assume (==) :: a -> a -> Bool;

(==) = primEqChar;

overload 
(==) = primEqFloat;

foo1 = 1 == 2;

foo a b = (a == coerce 1, b) ;

}
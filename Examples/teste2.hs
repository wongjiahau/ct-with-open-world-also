{
data Bool = True | False;

data Tree a = Leaf a | Node (Tree a) (Tree a); 

(==) = primEqChar;

overload
[] == [] = False;
(x:xs) == (y:ys) = x == y && xs == ys;

overload
(==) (Leaf a) (Leaf b) = a == b;
(==) (Node a b) (Node c d) = a == c && b == d;
(==) _ _ = False;  

False && x   = False;
True  && x   = x;

}
{

--assume (==)::a -> a -> Bool;

overload
(==) = primEqInt;

overload

(==) = primEqChar;


overload
(==) [] []         =  True;
(==) (x:xs) (y:ys) =  x==y && xs==ys;
(==) _ _           =  False;


overload 
(==) (a, b) (c, d) = (a == c) && (b == d);

overload
(==) (a, b, c) (d, e, f) = (a == d) && (b == e) && (c == f);

{-
overload
(==) (a, b, c, d) (e, f, g, h) = (a == e) && (b == f) && (c == g) && (d == h);
-}

show 1 = "1";

overload
show 'a' = "a";

teste 'a' = 1;


overload 
teste 1 = 1.0;


f x y z w = (x == y, show (teste z),  show w); 


False && x   = False;
True  && x   = x;




}  
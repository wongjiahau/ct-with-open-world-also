{
data X = Z | W;

data Tree t = Leaf | Node t (Tree t) (Tree t);

data Ordering = LT | EQ | GT;

(==) LT LT = True; 
(==) EQ EQ = True;
(==) GT GT = True;
(==) _ _   = False;

otherwise    = True;

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

compare = primCmpChar;

overload
compare = primCmpInt;

overload
compare = primCmpFloat;

overload
compare = primCmpDouble;

overload
compare = primCmpInteger;


(<=) x  y = compare x y /= GT;
(<)  x  y = compare x y == LT;
(>=) x  y = compare x y /= LT;
(>)  x  y = compare x y == GT;

(/=) x  y    = not (x==y);

not True     = False;
not False    = True;


ins a [] = [a];
ins a (x:xs) = if a == x then x:xs else x:(ins a xs);
 

overload
ins x Leaf = Node x Leaf Leaf;
ins x (Node y l r)  
    | x == y = (Node y l r)
    | x <  y = Node y (ins x l) r 
    | otherwise = Node y l (ins x r);



{-
overload 
ins f y [] = [y];
ins f y (x:xs) = if f y x then x:xs else x:(ins f y xs);
-}

foo a x = ins a x;

foo2 a (x:xs) = ins a (x:xs);
 

x = ins Z [Z];

}

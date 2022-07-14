{

data Tree t = Nil | Node t (Tree t) (Tree t);


data Ordering = LT | EQ | GT;

(==) = primEqInt;

--overload
--(==) = primEqChar;

overload 
(==) LT LT = True;
(==) EQ EQ = True;
(==) GT GT = True;

data Maybe a = Nothing | Just a;


(==) Nothing  Nothing  = True;
(==) (Just x) (Just y) = x == y;
(==) _ _               = False;

data Either a b = Left a | Right b;
		 

either l r (Left x)  = l x;
either l r (Right y) = r y;


(==) (Left a) (Left b)   = a == b;
(==) (Right a) (Right b) = a == b;
(==) _ _ 		 = False;

compare (Left a) (Left b)   = compare a b;
compare (Right a) (Right b) = compare a b;
compare (Left a) (Right b)  = LT;
compare _ _                 = GT;

overload
compare = primCmpChar;

show LT = "LT";
show EQ = "EQ";
show GT = "GT";

overload
show (Left a)  = "Left " ++ show a;
show (Right a) = "Right" ++ show a;

[]     ++ ys      = ys;
(x:xs) ++ ys      = x : (xs ++ ys);

not True     = False;
not False    = True;
(/=) x  y    = not (x==y);

overload
compare = primCmpInt;

(<)  x  y = compare x y == LT;
(>)  x  y = compare x y == GT;


busca x Nil = Nil;
busca x (Node y t1 t2) | x < y = t1
                       | x > y = t2
                       | x == y = (Node x t1 t2);

overload 
(==) LT LT = True;
(==) EQ EQ = True;
(==) GT GT = True;

}
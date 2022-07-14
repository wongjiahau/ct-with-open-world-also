{

data Tree t = Leaf t | Node t (Tree t) (Tree t);

data Ordering = LT | EQ | GT;

otherwise    = True;

show LT = "LT";
show EQ = "EQ";
show GT = "GT";

(==) LT LT = True; 
(==) EQ EQ = True;
(==) GT GT = True;
(==) _ _   = False;

False && x   = False;
True  && x   = x;

--(||) False  x   = x;
--(||) True x   = True;

not True     = False;
not False    = True;

quotRem = primQrmInt;
div       = primDivInt;
quot      = primDivInt;
rem       = primDivInt;
mod       = primDivInt;

quotRem = primQrmInteger;
div       = primDivInteger;
quot      = primDivInteger;
rem       = primDivInteger;
mod       = primDivInteger;

otherwise = False;

-- Equality and Ordered classes ---------------------------------------------

(/=) x  y    = not (x==y);

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
compare = primCmpInteger;

overload 
(==) LT LT = True;
(==) EQ EQ = True;
(==) GT GT = True;



(<=) x  y = compare x y /= GT;
(<)  x  y = compare x y == LT;
(>=) x  y = compare x y /= LT;
(>)  x  y = compare x y == GT;

max x y   | x <= y      = y
          | otherwise   = x;

min x y   | x <= y      = x
          | otherwise   = y;


maior x y | x > y = 1
          | x == y = 0
          | x < y = 0;

menor x y | x < y = True
          | otherwise = False;

multiplo n x  = if x `mod` n == 0 then True else False;

crivo [] = [];
crivo (x:xs) = x:(crivo(filter' (not.multiplo x) xs)); 

filter' p xs = [ x | x <- xs, p x ];

busca x (Leaf y) = Leaf y;
busca x (Node y t1 t2) | x < y = t1
                       | x > y = t2
                       | x == y = (Node x t1 t2);


map2 f [] = [];
map2 f (x:xs) = (f x : map2 f xs);

mapTwice l = map2 (\x -> x * (coerce 2)) l;
mapNot l = map2 not l;

negate x        = (coerce 0) - x;

(+) = primPlusInt;
(*) = primMulInt;
(-) = primMinusInt;
(/) = primDivFloat;

(+) = primPlusInteger;
(*) = primMulInteger;
(-) = primMinusInteger;
(/) = primDivFloat;

negate x = 0.0 - x;
(+) = primPlusFloat;
(*) = primMulFloat;
(-) = primMinusFloat;
(/) = primDivFloat;


fromInteger = primIntegerToInt;


map f (Leaf x) = Leaf (f x);
map f (Node x t1 t2) = Node (f x) (map f t1) (map f t1);
   
overload map f xs = [ f x | x <- xs];

inc x = x + (coerce 1);


data Maybe a = Nothing | Just a;


(==) Nothing  Nothing  = True;
(==) (Just x) (Just y) = x == y;
(==) _ _               = False;

data Either a b = Left a | Right b;
		 

either l r (Left x)  = l x;
either l r (Right y) = r y;


show (Left a)  = "Left " ++ show a;
show (Right a) = "Right " ++ show a;

(==) (Left a) (Left b)   = a == b;
(==) (Right a) (Right b) = a == b;
(==) _ _ 		 = False;

compare (Left a) (Left b)   = compare a b;
compare (Right a) (Right b) = compare a b;
compare (Left a) (Right b)  = LT;
compare _ _                 = GT;


[]     ++ ys      = ys;
(x:xs) ++ ys      = x : (xs ++ ys);

}

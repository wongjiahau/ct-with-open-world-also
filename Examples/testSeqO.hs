{

data Seq t = Nil | Cons t (Seq (t, t));


[]     ++ ys      = ys;
(x:xs) ++ ys      = x : (xs ++ ys);


False && x   = False;
True  && x   = x;

negate x        = (coerce 0) - x;

(+) = primPlusInt;
(*) = primMulInt;
(-) = primMinusInt;
(/) = primDivFloat;

(+) = primPlusInteger;
(*) = primMulInteger;
(-) = primMinusInteger;
(/) = primDivFloat;

(+) = primPlusFloat;
(*) = primMulFloat;
(-) = primMinusFloat;
(/) = primDivFloat;

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
compare = primCmpInteger;

--len :: Seq a -> Int;
len Nil 		= 0;
len (Cons x s )	= (len x) + 2 * (len s);

--overload len 'a' = 'a';


overload len 1 = 0; 


overload 
len []     = 0;
len (x:xs) = (len x) + (len xs); 


--overload len (a, b) = (len a) + (len b);

pair f (x, y) = (f x, f y);



-- map' :: (a -> b) -> Seq a -> Seq b;
map' f Nil          = Nil;
map' f (Cons x xs)  = Cons (f x) (map' (pair f) xs);

overload map' f xs = [ f x | x <- xs ];

--gfold:: a -> (b -> a -> a) -> ((b,b) -> b) -> Seq b -> a;
gfold e f g Nil = e;
gfold e f g (Cons x xs) = f x (gfold e f g (map' g xs));

-- efold :: a -> (b -> a -> a) -> ((b,b) -> b) -> (c -> b) -> Seq c -> a;
efold e f g h Nil = e;
efold e f g h (Cons x xs) = f (h x) (efold e f g (g.pair h) xs);

sum' = gfold 0 (+) (uncurry (+));

l_test = len (Cons 1 (Cons (2, 3) Nil));

test1 = Cons 1 (Cons (2, 3) Nil);
test2 = Cons 1 (Cons (2, 3) (Cons ((4,5), (6,7)) Nil));

testS = Cons test1 (Cons (test2, test1) Nil);

conSeq x = Cons x Nil;

inc x = 1 + x;

overload inc x = 1.0 + x;

overload inc x = pair inc x;

--overload inc = map' inc;    

test x = len (conSeq (conSeq x));
test' = test testS;

incSeq = map' inc test1;

maps = map' inc;

testSat x = (maps (conSeq x), x == 1);

}

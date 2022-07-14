{
data Seq t = Nil | Cons t (Seq (t, t));

sum' = gfold 0 (+) (uncurry (+));

--len :: Seq a -> Int;
len Nil 		= 0;
len (Cons x s )	= 1 + 2 * (len s);

--map' :: (a -> b) -> Seq a -> Seq b;
map' f Nil          = Nil;
map' f (Cons x xs)  = Cons (f x) (map' (pair f) xs);

--gfold:: a -> (b -> a -> a) -> ((b,b) -> b) -> Seq b -> a;
gfold e f g Nil = e;
gfold e f g (Cons x xs) = f x (gfold e f g (map' g xs));

pair f (x, y) = (f x, f y);

--efold :: a -> (b -> a -> a) -> ((b,b) -> b) -> (c -> b) -> Seq c -> a;
efold e f g h Nil = e;
efold e f g h (Cons x xs) = f (h x) (efold e f g (g.pair h) xs);

--test1:: Num a => Seq a;
test1 = Cons 1 (Cons (2, 3) Nil);
test = Cons 1 (Cons (2, 3) (Cons ((4,5), (6,7)) Nil)); 


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

}

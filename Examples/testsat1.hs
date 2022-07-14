{
data Seq t = Nil | Cons t (Seq (t, t));


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


pair f (x, y) = (f x, f y);

-- map' :: (a -> b) -> Seq a -> Seq b;
map' f Nil          = Nil;
map' f (Cons x xs)  = Cons (f x) (map' (pair f) xs);

inc x = 1 + x;

overload inc x = 1.0 + x;

overload inc x = pair inc x;   

maps = map' inc


}

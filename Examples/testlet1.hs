{

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


k a b = a;

fst' (x, y) = x;

f x = let {z = fst' x} in z;


g1 x = let {f1 y = y + x} in f1 x;

f1 x = let {z = fst' x} in z 'a';

f2 y = let {g x = x} in (g y, g 1);

f3 y = let {g x = x} in g (k (g y, g 1) (g 'a'));   

f4 y = let {g x = x} in (g, g y);

f5 x = let {f = (\y -> (fst' x, y)); g y = (k x y, y)} in (f x, g); 

f6 x = let {g y = let {h = (fst' x, y)} in h} in g

}

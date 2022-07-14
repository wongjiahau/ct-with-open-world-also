{

data Seq t = Nil | Cons t (Seq (t, t));

(+) = primPlusInt;
(*) = primMulInt;

(+) = primPlusFloat;
(*) = primMulFloat;


len Nil 		= 0;
len (Cons x s )	= (len x) + 2 * (len s);


overload len 1 = 0; 


overload 
len []     = 0;
len (x:xs) = (len x) + (len xs); 


conSeq x = Cons x Nil;

test x = len (conSeq (conSeq x));
}
data Lista x = Cons x (Lista x) | Nill

class I a b where 
  (<==>) :: a -> b -> Bool

instance I Char Char where
  (<==>) '1' '1' = True

instance (I a a) => I (Lista a) (Lista a) where 
  (Cons x1 x1s) <==> (Cons x2 x2s) = (x1 <==> x2) && (x1s <==> x2s)
 
instance (I a a) => I [a] [a] where
  (x1:x1s) <==> (x2:x2s) = (x1 <==> x2) && (x1s <==> x2s)

k a b = a

class H a b where
  h:: a -> b 

instance (H a b) => H [a] b where
  h [a] = h a

instance (H a b) => H (Lista a) b where
   h (Cons a Nill) = h a;

instance H Char Char where
   h '1' = '1'

s a b = k (h b) ([(Cons (Cons [a] Nill) Nill)] <==> b)

s2 a b = [(Cons (Cons [a] Nill) Nill)] <==> b

x a = s a (Cons (Cons [a] Nill) Nill)

x2 = s2 '1' [(Cons (Cons ['1'] Nill) Nill)] 

--f = x '1'



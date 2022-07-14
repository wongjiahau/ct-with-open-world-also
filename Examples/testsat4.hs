{
data Lista x = Cons x (Lista x) | Nill;

(&&) True True = True;

(<==>) 1 1 = True;

overload '1' <==> '1' = True;

overload (Cons x1 x1s) <==> (Cons x2 x2s) = (x1 <==> x2) && (x1s <==> x2s);
 
overload (x1:x1s) <==> (x2:x2s) = (x1 <==> x2) && (x1s <==> x2s);

k a b = a;


h [a] = h a;

--h [a] = k (h a) (a==a);


overload h (Cons a Nill) = h a;

overload h 1 = 1;

s a b = k (h b) ([(Cons (Cons [a] Nill) Nill)] <==> b);

--s a b = k (h b) ( [[[[[a]]]]] <==> b);

x a = s a [(Cons (Cons [a] Nill) Nill)]

}


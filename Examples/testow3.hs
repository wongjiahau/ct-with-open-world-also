{

fst (a, b) = a;

overload 
fst (a, b, c) = a;

overload 
fst (a, b, c, d) = a;


g a = fst a;

f1 a b = g (a, b);
f2 a b c d = g (d, a, b, c);

map f xs = [ f x | x <- xs];

h1 = map fst;  

h2 a = h1 [(a,a), (a,a)];
   

f 1 b = (0, b);
f a b = (a, b);

overload 
f '1' b = ('0', b);
f a b   = (a, b);




}
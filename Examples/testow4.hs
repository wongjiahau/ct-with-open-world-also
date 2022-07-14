{


assume f::a -> b -> (a,b);

snd (a, b) = b;

f 1 b = (0, b);
f a b = (a, b);

overload 
f '1' b = ('0', b);
f a b   = (a, b);

h = 1;

overload
h = 'a';

g1 a = snd (f (snd (f 1 a)) a);

g a = snd (f h a);   


}
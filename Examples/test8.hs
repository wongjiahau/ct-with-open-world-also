{

f 0 = False;
f 1 = True;

overload 
f False = 0;
f True =  1;

overload 
f 'T' = True;
f 'F' = False;

overload 
f False = 'F';
f True  = 'T';

g x y = (f x, f y);

x = f 'T';

y = f True;
}
 

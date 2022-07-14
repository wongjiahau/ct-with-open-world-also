{




r (a, b) = 'a';
overload r "1" = 1;
overload r "a" = 'a';

f 1 = '1';
overload f 'a' = True;
overload f True = 1;
overload f 1 = True;
overload f (a, b) = True;

g s = f (r s);

p s = if g "s" then s else s  


}


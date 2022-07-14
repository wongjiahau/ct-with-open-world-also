{

f x = x + 1;

overload
(+) = primPlusInt;

overload
(+) = primPlusFloat;


f (a, b) = (f a, f b);


}

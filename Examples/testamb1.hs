{

(+) = primPlusInt;
(-) = primMinusInt;
(*) = primMulInt;
(+) = primPlusFloat;
(-) = primMinusFloat;
(*) = primMulFloat;



show 1 = "1";

overload show 1.0 = "1.0";

map f xs = [ f x | x <- xs];

mul a b = map (mul a) b;

overload
mul 1 b = b;
mul 0 _ = 0.0;
mul a b = b + (mul (a - 1) b);

overload     
mul b 1 = b;
mul _ 0 = 0.0;
mul b a = b + (mul (a - 1) b);

overload
mul 1.0 b = b;
mul 0.0 _ = 0;
mul a b = b + (mul a (b - 1));

overload
mul a 1.0  = a;
mul _ 0.0  = 0;
mul a b = a + (mul (a - 1) b);


mul::Int -> Int -> Int;
mul a b = a * b;

mul::Float -> Float -> Float;
mul a b = a * b;


--f1 = mul (mul 1  1.0)  1;

f2 = mul 1 (mul 1 []) 

}
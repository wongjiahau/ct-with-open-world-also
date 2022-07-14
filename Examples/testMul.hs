{
assume mul:: a -> b -> c;

(*) = primMulInt;

overload
(*) = primMulDouble;

overload 
(*) = primMulFloat;


mul 1 1.0 = 1.0;

overload
mul 1 1.0 = 1;

overload
mul a (x:xs) = mul a x:mul a xs;

f b x y = if b then mul x [y] else y;

}
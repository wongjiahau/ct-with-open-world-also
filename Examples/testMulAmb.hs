{

-- assume (*):: a -> b -> c;

(==) = primEqInt;

overload
(==) = primEqChar;

overload
(==) = primEqFloat;

overload
(==) = primEqDouble;


(*) 1 1.0 = 1.0;

overload
(*) 1 1 = 1;

overload 
(*) 1.0 1.0 = 1.0;

overload -- Detecta ambiguidade Ok!
(*) 1.0 1.0 = 1;


g1 a b c = a * b * c; 

g2 =  (coerce 1 * coerce 2 * coerce 3) == 1;  


}
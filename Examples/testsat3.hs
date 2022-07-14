{


data Boleano = Verdadeiro | Falso;

(==) = primEqInt;
overload
(==) = primEqChar;
overload
(==) = primEqFloat;
overload
(==) = primEqInteger;

False && x   = False;
True  && x   = x;

(==) Nothing  Nothing  = True;
(==) (Just x) (Just y) = x == y;
(==) _ _               = False;

k a b = a;

h [a] = h a;

overload h Falso = Falso;

s a b = k (h b) ([a] == b)


}


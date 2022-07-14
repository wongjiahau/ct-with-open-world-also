{ 
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

fst' (a, b) = a;

h = (\(a,b) -> a,
     let {f x = let {g = fst' h x}
                  in g}
        in f ("String",'*') == "String");

{-h' = (\(a,b) -> a,
     let {f x = let {g = fst' h' x}
                  in g}
        in f ("String",'*') == '*')-}


}

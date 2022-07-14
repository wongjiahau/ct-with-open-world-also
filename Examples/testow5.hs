{ 
assume (==) :: a -> a -> Bool;


f a = 1 == a;

g a b = a == b;


(==) = primEqChar;

overload 
True  == True = True;
False == False = True;
_     == _     = False;

}




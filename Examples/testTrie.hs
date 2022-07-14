{ 
data Key = Atom Int | Pair (Key,Key);
data Trie a = Empty | Branch ([(Int,a)],Trie (Trie a));

(==) [] []         =  True;
(==) (x:xs) (y:ys) =  x==y && xs==ys;
(==) _ _           =  False;

overload
(==) = primEqInt;

overload
(==) = primEqChar;

overload
(==) = primEqFloat;

overload
(==) = primEqInteger;

False && x   = False;
True  && x   = x;



find (Branch ((c,a):l,_), Atom d) = if d == c then a else find (Branch (l, Empty), Atom d);
find (Branch (_,t), Pair (p, q)) = find (find (t, p), q)  
}

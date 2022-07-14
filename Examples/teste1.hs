{
[]     ++ ys      = ys;
(x:xs) ++ ys      = x : (xs ++ ys);

show 0 = "0";

overload 
show 'a' = "a";

overload
show [] = "";
show (x:xs) = show x ++ show xs;

overload 
show (a, b) = "(" ++ show a ++ "," ++ show b ++ ")"


}
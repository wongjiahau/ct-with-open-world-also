{
data Maybe a = Nothing | Just a;


(>>) p q  = p >>= \_ -> q;

-- fail s  = error s;


[]     ++ ys      = ys;
(x:xs) ++ ys      = x : (xs ++ ys);


(x:xs) >>= f = f x ++ (xs >>= f);

[]     >>= f = [];

return x     = [x];

-- fail s       = [];

(>>=) = primbindIO;

return = primretIO;
 


(Just x)  >>= k = k x;
Nothing >>= k = Nothing;

return        = Just;

fail s        = Nothing;

m = do { c <- getChar; return c};

m2 = do {c <- getChar; c' <- getChar; let {s = (c:[c'])}; return s}


}

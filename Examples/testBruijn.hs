{
data Bind a = Zero | Succ a;
data Term a = Var a | App (Term a, Term a) | Abs (Term (Bind a));




 --lift :: (Term a, a) -> Term (Bind a);
lift (Var y, x)      = if x == y then (Var Zero) else (Var (Succ y));
lift (App (u, v), x) = App (lift (u, x), lift (v, x)); 
lift (Abs t, x) = Abs (lift (t, Succ x));

abstract (t, x) = Abs (lift (t, x));

reduce (Abs s, t) = subst' (s, t); 

--subst' ::  (Term (Bind a), Term a) -> Term a;
subst' (Var Zero, t)     = t;
subst' (Var (Succ x), t) = Var x;
subst' (App (u, v), t)   = App (subst' (u, t), subst' (v, t));
subst' (Abs s, t)        = Abs (subst' (s, term Succ t));

term f (Var x)      = Var (f x);
term f (App (u, v)) = App (term f u, term f v);
term f (Abs t)      = Abs (term (bind f) t);

--bind :: (a -> b) -> Bind a -> Bind b;
bind f Zero = Zero;
bind f (Succ x) = Succ (f x);

False && x   = False;
True  && x   = x;

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


}

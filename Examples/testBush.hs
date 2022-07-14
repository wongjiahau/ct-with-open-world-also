{
data Bush a = Nil | Cons a (Bush (Bush a));

--map' :: (a -> b) -> Bush a -> Bush b;
map' f Nil = Nil;
map' f (Cons x xs) = Cons (f x) (map' (map' f) xs);


--gfold :: a -> (b -> a -> a) -> (a -> b) -> (b -> b) -> Bush b -> a;
gfold e f g h Nil = e;
gfold e f g h (Cons x xs) = f x ((gfold e f g h.map' (g.gfold e f g h.map' h)) xs) 
}
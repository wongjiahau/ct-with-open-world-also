{
data Exception
  = ArithException      ArithException
  | ArrayException      ArrayException
  | AssertionFailed     String
  | AsyncException      AsyncException
  | BlockedOnDeadMVar
  | Deadlock
  | DynException        Dynamic
  | ErrorCall           [Char]
  | ExitException       ExitCode
  | IOException 	IOException	-- IO exceptions (from 'ioError')
  | NoMethodError       [Char]
  | NonTermination
  | PatternMatchFail    [Char]    
  | RecConError         [Char]
  | RecSelError         [Char]
  | RecUpdError         [Char];

-- Maybe type ---------------------------------------------------------------

data Maybe a = Nothing | Just a;


(==) Nothing  Nothing  = True;
(==) (Just x) (Just y) = x == y;
(==) _ _               = False;

compare Nothing Nothing = EQ;
compare (Just x) (Just y) | x == y    = EQ
                          | x == y    = LT
                          | otherwise = GT;
compare Nothing _       = LT;
compare _ Nothing       = GT;

show Nothing  = "Nothing";
show (Just a) = "Just"; 
 
maybe n f Nothing  = n;
maybe n f (Just x) = f x;

fmap f Nothing  = Nothing;
fmap f (Just x) = Just (f x);

isJust Nothing = False;
isJust _       = True;

isNothing Nothing = True;
isNothing _       = False;

fromJust Nothing  = error "Maybe.fromJust: Nothing";
fromJust (Just x) = x;

fromMaybe d x = case x of {Nothing -> d;Just v  -> v};

maybeToList  Nothing   = [];
maybeToList  (Just x)  = [x];

listToMaybe []        =  Nothing;
listToMaybe (a:_)     =  Just a;

catMaybes ls = [x | (Just x) <- ls];

mapMaybe _ []     = [];
mapMaybe f (x:xs) =
 let {rs = mapMaybe f xs} in
 case f x of
  {Nothing -> rs;
   Just r  -> r:rs};


-- Equality and Ordered classes ---------------------------------------------

(/=) x  y    = not (x==y);

(==) = primEqInt;

overload
(==) = primEqChar;

overload
(==) = primEqFloat;

overload
(==) = primEqDouble;

overload
(==) = primEqInteger;


compare = primCmpChar;

overload
compare = primCmpInt;

overload
compare = primCmpFloat;

overload
compare = primCmpDouble;

overload
compare = primCmpInteger;


(<=) x  y = compare x y /= GT;
(<)  x  y = compare x y == LT;
(>=) x  y = compare x y /= LT;
(>)  x  y = compare x y == GT;

max x y   | x <= y      = y
          | otherwise   = x;

min x y   | x <= y      = x
          | otherwise   = y;

data Ordering = LT | EQ | GT;

otherwise    = True;

show LT = "LT";
show EQ = "EQ";
show GT = "GT";

(==) LT LT = True; 
(==) EQ EQ = True;
(==) GT GT = True;
(==) _ _   = False;

False && x   = False;
True  && x   = x;

--(||) False  x   = x;
--(||) True x   = True;

not True     = False;
not False    = True;
-- Character type -----------------------------------------------------------
isSpace c =  c == ' ';

isUpper c =  (c >= 'A') && (c <= 'Z');

isLower c =  (c >= 'a') &&  (c <= 'z');

isDigit c =  (c >= '0') &&  (c <= '9');

isAlpha c =  (isUpper c)  ||  (isLower c);

isAlphaNum c =  (isAlpha c)  ||  (isDigit c);

-- Some standard functions --------------------------------------------------
fst (x,_)       = x;

snd (_,y)       = y;

curry f x y     = f (x,y);

uncurry f p     = f (fst p) (snd p);

id    x         = x;

const k _       = k;

flip f x y      = f y x;

($) f x           = f x;

until p f x     = if p x then x else until p f (f x);

asTypeOf        = const;

error s         = throw (ErrorCall s);

undefined       = error "Prelude.undefined";


-- Numeric classes ----------------------------------------------------------

negate x        = (coerce 0) - x;

(+) = primPlusInt;
(*) = primMulInt;
(-) = primMinusInt;
(/) = primDivInt;

(+) = primPlusInteger;
(*) = primMulInteger;
(-) = primMinusInteger;
(/) = primDivInteger;

(+) = primPlusFloat;
(*) = primMulFloat;
(-) = primMinusFloat;
(/) = primDivFloat;

(+) = primPlusDouble;
(*) = primMulDouble;
(-) = primMinusDouble;
(/) = primDivDouble;



-- Standard list functions {PreludeList} ------------------------------------


(==) [] []         =  True;
(==) (x:xs) (y:ys) =  x==y && xs==ys;
(==) _ _           =  False;


[]     ++ ys      = ys;
(x:xs) ++ ys      = x : (xs ++ ys);


head (x:_)        = x;

last [x]          = x;
last (_:xs)       = last xs;

tail (_:xs)       = xs;

init [x]          = [];
init (x:xs)       = x : init xs;

null []           = True;
null (_:_)        = False;


map f xs          = [ f x | x <- xs ];

filter p xs       = [ x | x <- xs, p x ];

concat            = foldr (++) [];

length            = foldl' (\n _ -> n + 1) 0;

(x:_)  !! 0       = x;
(_:xs) !! n | n>0 = xs !! (n-1);
(_:_)  !! _       = error "Prelude.!!: negative index";
[]     !! _       = error "Prelude.!!: index too large";


foldl f z []      = z;
foldl f z (x:xs)  = foldl f (f z x) xs;

foldl' f a []     = a;
foldl' f a (x:xs) = (foldl' f $! f a x) xs;

foldl1 f (x:xs)   = foldl f x xs;

foldr f z []      = z;
foldr f z (x:xs)  = f x (foldr f z xs);

foldr1 f [x]      = x;
foldr1 f (x:xs)   = f x (foldr1 f xs);


iterate f x       = x : iterate f (f x);

repeat x          = xs where {xs = x:xs};

replicate n x     = take n (repeat x);

cycle []          = error "Prelude.cycle: empty list";
cycle xs          = xs' where {xs'=xs++xs'};

take n _  | n <= 0  = [];
take _ []           = [];
take n (x:xs)       = x : take (n-1) xs;

drop n xs | n <= 0  = xs;
drop _ []           = [];
drop n (_:xs)       = drop (n-1) xs;


takeWhile p []       = [];
takeWhile p (x:xs)
	 | p x       = x : takeWhile p xs
	 | otherwise = [];

dropWhile p []       = [];
dropWhile p xs
	 | p (head xs) = dropWhile p (tail xs)
	 | otherwise   = xs;


span p []            = ([],[]);
span p xs
	 | p (head xs) = ((head xs):fst (span p (tail xs)), snd (span p (tail xs)))
	 | otherwise   = ([],xs);


scanl f q xs      = q : (case xs of {
			 []   -> [];
			 x:xs -> scanl f (f q x) xs});

splitAt n xs | n <= 0 = ([],xs);
splitAt _ []          = ([],[]);
splitAt n (x:xs)      = (x:(fst xs'),snd xs') where {xs'  = splitAt (n-1) xs};


break p              = span (not . p);


scanr f q0 []     = [q0];
scanr f q0 (x:xs) = f x (head qs) : qs
		    where {qs = scanr f q0 xs};

and        = foldr (&&) True;
or         = foldr (&&) False;

any p      = or  . map p;
all p      = and . map p;


unwords []	=  "";
unwords [w]	= w;
unwords (w:ws)	= w ++ ' ' : unwords ws;

unlines []      = [];
unlines (l:ls)  = l ++ '\n' : unlines ls;

reverse    = foldl (flip (:)) [];

elem              = any . (==);
--notElem           = all . (/=);

lookup k []       = Nothing;
lookup k ((x,y):xys)
      | k==x      = Just y
      | otherwise = lookup k xys;

sum               = foldl' (+) 0;
product           = foldl' (*) 1;

maximum           = foldl1 max;
minimum           = foldl1 min;

concatMap f       = concat . map f;

zipWith z (a:as) (b:bs)   = z a b : zipWith z as bs;
zipWith _ _      _        = [];

zip               = zipWith  (\a b -> (a,b));

overload
zip3              = zipWith3 (\a b c -> (a,b,c));

overload
zipWith3 z (a:as) (b:bs) (c:cs) = z a b c : zipWith3 z as bs cs;
zipWith3 _ _ _ _          = [];

words s    = case dropWhile isSpace s of {
		  "" -> [];
		  s' -> (fst s'' : words (snd s''))
			where {s'' = break isSpace s'}};

elemIndex x     = findIndex (x(==));

elemIndices x   = findIndices (x(==));

find p          = listToMaybe . filter p;


findIndex p     = listToMaybe . findIndices p;


findIndices p xs = [ i | (x,i) <- zip xs [0..], p x];


isPrefixOf [] _         =  True;
isPrefixOf _  []        =  False;
isPrefixOf (x:xs) (y:ys)=  x == y && isPrefixOf xs ys;

isSuffixOf x y          =  reverse x `isPrefixOf` reverse y;

nub l                   = nub' l []		-- '
  where {
    nub' [] _		= [];			-- '
    nub' (x:xs) ls				-- '
	| x `elem` ls   = nub' xs ls		-- '
	| otherwise     = x : nub' xs (x:ls)};	-- '



nubBy eq l              = nubBy' l []
  where{
    nubBy' [] _		= [];
    nubBy' (y:ys) xs
       | elem_by eq y xs = nubBy' ys xs
       | otherwise	 = y : nubBy' ys (y:xs)};

elem_by _  _ []		=  False;
elem_by eq y (x:xs)	=  x `eq` y || elem_by eq y xs;


{-
delete                  =  deleteBy (==);

deleteBy _  _ []        = [];
deleteBy eq x (y:ys)    = if x `eq` y then ys else (y : deleteBy eq x ys);
-}
--(\\)		        =  foldl (flip delete);

--union 			= unionBy (==);

--unionBy eq xs ys        =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs;

intersect               =  intersectBy (==);

intersectBy eq xs ys    =  [x | x <- xs, any (eq x) ys];

intersperse _   []      = [];
intersperse _   [x]     = [x];
intersperse sep (x:xs)  = x : sep : intersperse sep xs;

transpose []		 = [];
transpose ([]	: xss)   = transpose xss;
transpose ((x:xs) : xss) = (x : [h | (h:t) <- xss]) : transpose (xs : [ t | (h:t) <- xss]);


partition p xs = foldr (select p) ([],[]) xs;

select p x (ts,fs) | p x       = (x:ts,fs)
                   | otherwise = (ts, x:fs);
mapAccumL _ s []     	=  (s, []);
mapAccumL f s (x:xs) 	=  (s'',y:ys)
		           where {aux1 = f s x;
                                  aux2 = mapAccumL f s' xs;
                                  s'  = fst aux1;
                                  y   = snd aux1;
                                  s'' = fst aux2;
                                  ys  = snd aux2};


mapAccumR _ s []     	=  (s, []);
mapAccumR f s (x:xs) 	=  (s'',y:ys)
		           where {aux1 = f s x;
                                  aux2 = mapAccumR f s' xs;
                                  s'  = fst aux1;
                                  y   = snd aux1;
                                  s'' = fst aux2;
                                  ys  = snd aux2};



insert e ls = insertBy (compare) e ls;

insertBy _   x [] = [x];
insertBy cmp x (y:ys)
 = case cmp x y of
     {GT -> y : insertBy cmp x ys;
     _  -> x : (y:ys)};

maximumBy _ []		=  error "List.maximumBy: empty list";
maximumBy cmp xs	=  foldl1 max xs
			where
			   {max x y = case cmp x y of
					{GT -> x;
					_  -> y}};

minimumBy _ []		=  error "List.minimumBy: empty list";
minimumBy cmp xs	=  foldl1 min xs
			where{
			   min x y = case cmp x y of
					{GT -> y;
					_  -> x}};

genericLength []        =  0;
genericLength (_:l)     =  1 + genericLength l;

genericTake 0 _         =  [];
genericTake _ []        =  [];
genericTake n (x:xs) | n > 0  =  x : genericTake (n-1) xs;
genericTake _  _        =  error "List.genericTake: negative argument";

genericDrop 0 xs        =  xs;
genericDrop _ []        =  [];
genericDrop n (_:xs) | n > 0  =  genericDrop (n-1) xs;
genericDrop _ _		=  error "List.genericDrop: negative argument";

{-
genericSplitAt 0 xs     =  ([],xs);
genericSplitAt _ []     =  ([],[]);
genericSplitAt n (x:xs) | n > 0  =  (x:xs',xs'') where 
                               {aux = genericSplitAt (n-1) xs;
                                xs' = fst aux;
                                xs''= snd aux}; 
genericSplitAt _ _      =  error "List.genericSplitAt: negative argument";
-}

genericIndex (x:_)  0 = x;
genericIndex (_:xs) n 
 | n > 0     = genericIndex xs (n-1)
 | otherwise = error "List.genericIndex: negative argument.";
genericIndex _ _      = error "List.genericIndex: index too large.";

genericReplicate n x	=  genericTake n (repeat x);



}

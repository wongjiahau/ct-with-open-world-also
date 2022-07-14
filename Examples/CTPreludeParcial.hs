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


-- Ordering type ------------------------------------------------------------

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

quotRem = primQrmInt;
div       = primDivInt;
quot      = primDivInt;
rem       = primDivInt;
mod       = primDivInt;

quotRem = primQrmInteger;
div       = primDivInteger;
quot      = primDivInteger;
rem       = primDivInteger;
mod       = primDivInteger;

even n           =  n `rem` 2 == 0;
odd              =  not . even;

toEnum   = primIntToFloat;

{-
fromEnum = fromInteger . truncate;

truncate x        = fst (properFraction x);

properFraction x = if (n >= 0) then ((fromInteger m) * (fromInteger (b ^ n)), 0) else (fromInteger w, encodeFloat r n)
		   where { df = decodeFloat x; 
                           m  = fst df;
                           n  = snd df;
			   b  = floatRadix x;
                           qr = quotRem (fromInteger m) (b^(-n));
			   w  = fst qr;
                           r  = snd qr}; 
-}
(^) x 0  = 1;
(^) x n  = f x (n-1) x
		  where {f _ 0 y = y;
			 f x n y = g x n where {
				   g x n = if even n then g (x*x) (n`quot`2) else f x (n-1) (x*y)}};
(^) _ _           = error "Prelude.^: negative exponent";

--teste x n = if n >= 0 then x ^ n else recip (x ^ (negate n));



toEnum = primIntToInteger;

toEnum:: Int->Int;
toEnum = id;

succ x         = x + 1;

pred x         = x - 1;


-- Either type --------------------------------------------------------------

data Either a b = Left a | Right b;
		 

either l r (Left x)  = l x;
either l r (Right y) = r y;


show (Letf a)  = "Left " ++ show a;
show (Right a) = "Right " ++ show a;

compare (Left a) (Left b)   = compare a b;
compare (Right a) (Right b) = compare a b;
compare (Left a) (Right b)  = LT;
compare _ _                 = GT;

(==) (Left a) (Left b)   = a == b;
(==) (Right a) (Right b) = a == b;
(==) _ _ 		 = False;

-- Equality and Ordered classes ---------------------------------------------

(/=) x  y    = not (x==y);

(==) = primEqInt;

overload
(==) = primEqChar;

overload
(==) = primEqFloat;

overload
(==) = primEqInteger;


compare = primCmpChar;

overload
compare = primCmpInt;

overload
compare = primCmpFloat;

overload
compare = primCmpInteger;

overload 
(==) LT LT = True;
(==) EQ EQ = True;
(==) GT GT = True;



(<=) x  y = compare x y /= GT;
(<)  x  y = compare x y == LT;
(>=) x  y = compare x y /= LT;
(>)  x  y = compare x y == GT;

max x y   | x <= y      = y
          | otherwise   = x;

min x y   | x <= y      = x
          | otherwise   = y;

-- Numeric classes ----------------------------------------------------------

negate x        = 0 - x;

(+) = primPlusInt;
(*) = primMulInt;
(-) = primMinusInt;
(/) = primDivFloat;

(+) = primPlusInteger;
(*) = primMulInteger;
(-) = primMinusInteger;
(/) = primDivFloat;

negate x = 0.0 - x;
(+) = primPlusFloat;
(*) = primMulFloat;
(-) = primMinusFloat;
(/) = primDivFloat;


fromInteger = primIntegerToInt;

-- Character type -----------------------------------------------------------
isSpace c =  c == ' ';

isUpper c =  (c >= 'A') && (c <= 'Z');

isLower c =  (c >= 'a') &&  (c <= 'z');

isDigit c =  (c >= '0') &&  (c <= '9');

--isAlpha c =  (isUpper c)  ||  (isLower c);


--isAlphaNum c =  (isAlpha c)  ||  (isDigit c);

-- Maybe type ---------------------------------------------------------------

data Maybe a = Nothing | Just a;

(==) Nothing  Nothing  = True;
(==) (Just x) (Just y) = x == y;
(==) _ _               = False;

compare Nothing Nothing = EQ;
compare (Just x) (Just y) | x == y    = EQ
                          | x <= y    = LT
                          | otherwise = GT;
compare Nothing _       = LT;
compare _ Nothing       = GT;

show Nothing  = "Nothing";
show (Just a) = "Just"; 
 
maybe n f Nothing  = n;
maybe n f (Just x) = f x;

fmap f Nothing  = Nothing;
fmap f (Just x) = Just (f x);


 
pi                   = 4.0 * atan 1.0;
(**) x  y            = exp (log x * y);
logBase x y          = log y / log x;
sqrt x               = x ** 0.5;
tan x                = sin x / cos x;
sinh x               = (exp x - exp (-x)) / 2.0;
cosh x               = (exp x + exp (-x)) / 2.0;
tanh x               = sinh x / cosh x;
asinh x              = log (x + sqrt (x*x + 1.0));
acosh x              = log (x + sqrt (x*x - 1.0));
atanh x              = (log (1.0 + x) - log (1.0 - x)) / 2.0;

exp   = primExpFloat;
log   = primLogFloat;
sqrt  = primSqrtFloat;
sin   = primSinFloat;
cos   = primCosFloat;
tan   = primTanFloat;
asin  = primAsinFloat;
acos  = primAcosFloat;
atan  = primAtanFloat;

floatRadix  _ = primFloatRadix;
floatDigits _ = primFloatDigits;
--floatRange  _ = (primFloatMinExp, primFloatMaxExp);
encodeFloat = primFloatEncode;
decodeFloat = primFloatDecode;
isNaN       _ = False;
isInfinite  _ = False;
isDenormalized _ = False;
isNegativeZero _ = False;
isIEEE      _ = False;



exponent x        = if (fromInteger (fst m))==0 then 0 else (snd m) + floatDigits x
			where {m = decodeFloat x}; 


significand x     = encodeFloat m - (floatDigits x)
			where {m = fst (decodeFloat x)}; 
    

scaleFloat k x    = encodeFloat (fst m) ((snd m) + k)
	     	     where {m = decodeFloat x};



atan2 y x
    | x>0           = atan (y/x)
    | (x==0.0) && (y>0.0)   = pi / 2.0
    | (x<0.0) && (y>0.0)    = pi + atan (y/x)
    | ((x<=0.0) && (y<0.0)) &&
      ((x<0.0) && (isNegativeZero y)) &&
      ((isNegativeZero x) && (isNegativeZero y)) = negate (atan2 (negate y) x)
    | (y==0.0) && ((x<0.0) && (isNegativeZero x)) = pi    -- must be after the previous test on zero y
    | (x==0.0) && (y==0.0)  = y     -- must be after the other double zero tests
    | otherwise     = x + y; -- x or y is a NaN, return a NaN (via +)

-- Standard list functions {PreludeList} ------------------------------------


(==) [] []         =  True;
(==) (x:xs) (y:ys) =  x==y && xs==ys;
(==) _ _           =  False;

compare []     (_:_)  = LT;
compare []     []     = EQ;
compare (_:_)  []     = GT;

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

--elem              = any . (==);
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

zip3              = zipWith3 (\a b c -> (a,b,c));

zipWith3 z (a:as) (b:bs) (c:cs) = z a b c : zipWith3 z as bs cs;
zipWith3 _ _ _ _          = [];

words s    = case dropWhile isSpace s of {
		  "" -> [];
		  s' -> (fst s'' : words (snd s''))
			where {s'' = break isSpace s'}};





}
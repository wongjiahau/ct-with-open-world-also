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
{-
digitToInt c
 | isDigit c		=  fromEnum c - fromEnum '0'
 | c >= 'a' && c <= 'f' =  fromEnum c - fromEnum 'a' + 10
 | c >= 'A' && c <= 'F' =  fromEnum c - fromEnum 'A' + 10
 | otherwise	        =  error ("Char.digitToInt: not a digit " ++ show c);
-}

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

-- Standard list functions {PreludeList} ------------------------------------


(==) [] []         =  True;
(==) (x:xs) (y:ys) =  x==y && xs==ys;
(==) _ _           =  False;


[]     ++ ys      = ys;
(x:xs) ++ ys      = x : (xs ++ ys);



null []           = True;
null (_:_)        = False;


map f xs          = [ f x | x <- xs ];

filter p xs       = [ x | x <- xs, p x ];

concat            = foldr (++) [];

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


-- Numeric classes ----------------------------------------------------------

fromInteger = primIntegerToInt;


pi                   = (coerce 4.0) * atan (coerce 1.0);
(**) x  y            = exp (log x * y);
logBase x y          = log y / log x;
sqrt x               = x ** (coerce 0.5);
tan x                = sin x / cos x;
sinh x               = (exp x - exp (-x)) / (coerce 2.0);
cosh x               = (exp x + exp (-x)) / (coerce 2.0);
tanh x               = sinh x / cosh x;
asinh x              = log (x + sqrt (x*x + (coerce 1.0)));
acosh x              = log (x + sqrt (x*x - (coerce 1.0)));
atanh x              = (log ((coerce 1.0) + x) - log ((coerce 1.0) - x)) / (coerce 2.0);

exp   = primExpFloat;
log   = primLogFloat;
sin   = primSinFloat;
cos   = primCosFloat;
asin  = primAsinFloat;
acos  = primAcosFloat;
atan  = primAtanFloat;

exp   = primExpDouble;
log   = primLogDouble;
sin   = primSinDouble;
cos   = primCosDouble;
asin  = primAsinDouble;
acos  = primAcosDouble;
atan  = primAtanDouble;
          

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

{-
significand x     = encodeFloat m - (floatDigits x)
			where {m = fst (decodeFloat x)}; -}
    

scaleFloat k x    = encodeFloat (fst m) ((snd m) + k)
	     	     where {m = decodeFloat x};



atan2 y x
    | x> 0.0           = atan (y/x)
    | (x== 0.0) && (y>0.0)   = pi / 2.0
    | (x< 2.0) && (y> 2.0)   = pi + atan (y/x)
    | ((x<= 2.0) && (y< 2.0)) &&
      ((x< 2.0) && (isNegativeZero y)) &&
      ((isNegativeZero x) && (isNegativeZero y)) = negate (atan2 (negate y) x)
    | (y== 0.0) && ((x< 0.0) && (isNegativeZero x)) = pi    -- must be after the previous test on zero y
    | (x== 0.0) && (y== 0.0)  = y     -- must be after the other double zero tests
    | otherwise     = x + y; -- x or y is a NaN, return a NaN (via +)

-- Enum


toEnum           = primIntToChar;

fromEnum         = primCharToInt;

--enumFrom:: Char -> [Char];
--enumFrom c       = map toEnum [fromEnum c .. fromEnum ('\255')];

--enumFromThen :: Char -> Char -> Char -> [Char];
--enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last)]
--		       where {last = if d < c then minBound else '\255' };


succ x                = x + 1;
pred x                = x - 1;



enumFromTo x y        = map toEnum [ fromEnum x .. fromEnum y ];
enumFromThen x y      = map toEnum [ fromEnum x, fromEnum y ..];

--enumFromThen :: Int -> Int -> Int -> [Int];
--enumFromThenTo x y z  = map toEnum [ fromEnum x, fromEnum y .. fromEnum z ];


--enumFrom c       = map toEnum [fromEnum c .. fromEnum (primMaxInt)];


toEnum:: Int -> Int;
toEnum               = id;
fromEnum:: Int -> Int;
fromEnum             = id;

minBound = '\0';

maxBound = '\255';

minBound = primMinInt;

maxBound = primMaxInt;


toEnum         = primIntToInteger;

fromEnum       = primIntegerToInt;

fromEnum True  = 1;
fromEnum False = 0;

toEnum 1 = True;
toEnum 0 = False;

range (c,c')      = [c..c'];
}

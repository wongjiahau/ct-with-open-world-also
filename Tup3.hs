module Tup3 where

{- I wish this was not necessary, and fst etc. could be overloaded,
   but I don't want to create a class with "first" instead of fst... -}

-----------------------------------------------------------------------------

fst3 (a,_,_) = a
snd3 (_,b,_) = b
thd3 (_,_,c) = c

-----------------------------------------------------------------------------

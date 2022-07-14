module SimpleType where 

import Id

data SimpleType  = TVar Tyvar
           | TCon Tycon
           | TAp  SimpleType SimpleType
           | TGen Int
             deriving Eq

data Tyvar = Tyvar Idv IneqIndexes deriving Eq

data Tycon = Tycon Id deriving Eq

type IneqIndexes = [IneqIndex]   -- tyvar "fresh index" (sequence of inequality indexes)
type IneqIndex   = Int           -- To understand what's an inequality index, read the paper:
                                 --   http://www.dcc.ufmg.br/~camarao/sup/sup.pdf

------------------------------------------------------------------------------
{- instance Show SimpleType where
   show (TVar t) = show t
   show (TCon t) = show t
   show (TAp t1 t2) = "("++ show t1 ++ show t2 ++ ")"
   show (TGen i) = "g"++(show i) -}  

instance Show SimpleType where
  show = impstype 

instance Show Tyvar where
  show (Tyvar v l) = v ++ if null l then "" else "^" ++ show l 

instance Show Tycon where
  show (Tycon t) = show t

-----------------------------------------------------------------------------

tUnit 		= TCon (Tycon (toid "()"))
tBool 		= TCon (Tycon (toid "Bool"))
tOrdering 	= TCon (Tycon (toid "Ordering"))
tException 	= TCon (Tycon (toid "Exception"))
tChar	        = TCon (Tycon (toid "Char"))
tInt		= TCon (Tycon (toid "Int"))
tInteger	= TCon (Tycon (toid "Integer"))
tFloat		= TCon (Tycon (toid "Float"))
tDouble		= TCon (Tycon (toid "Double"))
tRatio		= TCon (Tycon (toid "Ratio"))
tRational	= TAp tRatio tInteger
tList 		= TCon (Tycon (toid "[]"))
-----------------------------------------------------------------------------

------------------------------------------------------------------------------


-----------------------------------------------------------------------------
tArrow     = TCon (Tycon (toid "(->)"))

infixr 4 `fn`
fn		:: SimpleType -> SimpleType -> SimpleType
a `fn` b	= TAp (TAp tArrow a) b



tTuple2		= TCon (Tycon (toid "(,)"))
tTuple3 	= TCon (Tycon (toid "(,,)"))
tTuple4		= TCon (Tycon (toid "(,,,)"))
tTuple5		= TCon (Tycon (toid "(,,,,)"))
tTuple6 	= TCon (Tycon (toid "(,,,,,)"))
tTuple7 	= TCon (Tycon (toid "(,,,,,,)"))

-----------------------------------------------------------------------------

tIOResult	= TCon (Tycon (toid "IOResult"))
tIO		= TCon (Tycon (toid "IO"))
tIOError	= TCon (Tycon (toid "IOError"))
tFilePath	= TAp tList tChar

----------------------------------------------------------------------------

tString		= TAp tList tChar

-----------------------------------------------------------------------------


data Defaults = I {-Integer-} | D {-Double-}

-----------------------------------------------------------------------------
impstype (TAp t1 t2) = if t1 == (TCon (Tycon (toid "[]"))) then "[" ++ impstype t2 ++ "]" else
                       impstype2 t1 t2 
impstype (TVar t)    = show t
impstype (TCon t)    = show t
impstype (TGen i)    = [toEnum (i + 97)] ++ ""
   
impstype2 (TAp t1 t2) t3 = if t1 == (TCon (Tycon (toid "(->)"))) then impstypep t2 ++ " -> " ++ impstype t3 else 
                           if t1 == (TCon (Tycon (toid "(,)"))) then "(" ++ impstype t2 ++ "," ++ impstype t3 ++ ")" else
                           impstype3 t1 t2 t3
impstype2 t1 t2         =  impstype t1 ++ " " ++ impstype t2

impstype3 (TAp t1 t2) t3 t4 = if t1 == (TCon (Tycon (toid "(,,)"))) then "(" ++ impstype t2 ++ "," ++ impstype t3 ++ "," ++ impstype t4 ++")" else 
                              impstype t1 ++ " " ++ impstype t2 ++ " " ++ impstype t3 ++ " " ++ impstype t4
impstype3 t1 t2 t3 = impstype t1 ++ " " ++ impstype t2 ++ " " ++ impstype t3

impstypep t@(TAp (TAp t1 t2) t3) = if t1 == (TCon (Tycon (toid "(->)"))) then "(" ++ impstype t ++ ")" else impstype t 
impstypep t                      = impstype t



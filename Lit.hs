module Lit where

import SimpleType
import Pred
import TIMonad
import Id
import Assump
import Type
import Subst

data Literal = LitInt   Integer
             | LitChar  Char
             | LitRat   Rational
             | LitStr   String
             | LitFloat Double
             deriving (Eq, Show)

tiLit            :: Literal -> TI (Constrained SimpleType, TypCtx)
tiLit (LitChar  _) = return ([]:=>tChar,[])
tiLit (LitInt   _) = return ([]:=>tInt, [])
tiLit (LitFloat _) = return ([]:=>tFloat, [])
{-
tiLit (LitInt n)  = do v <- freshTVar
                       let nn = toid (show n)
                           qt = [Constraint (toid (show n), v, False)] :=> v
                       return (qt, [nn:>:(CW, quantify (tv qt) qt)], 
                                   [nn:>:(CW, toType tInteger),
                                    nn:>:(CW, toType tInt)])

-}
tiLit (LitRat r)  = do v <- freshTVar
                       let nr = toid (show r)
                           qt = [Constraint (nr, v, False)] :=> v
                       return (qt, [nr:>:(CW, quantify (tv qt) qt)])
tiLit (LitStr _)  = return ([]:=>TAp tList tChar,[])


-----------------------------------------------------------------------------

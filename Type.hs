module Type where

import SimpleType
import Pred
import Subst
import SubsPred

import Data.List ((\\))

data Type = Forall (Constrained SimpleType) deriving (Eq, Show)

quantify      :: [Tyvar] -> Constrained SimpleType -> Type
quantify vs qt = Forall (apply s qt)
  where vs' = [ v | v <- tv qt, v `elem` vs ]
        s   = zip vs' (map TGen [0..])


quantifyParc q = apply s q
  where vs' = [ v | v <- tv q]
        s   = zip vs' (map TGen [0..])

 


toType      :: SimpleType -> Type
toType t     = Forall ([] :=> t)

iType ([] :=> t) = impstype t
iType (p  :=> t) = "{" ++ impConstraint p ++ "} => " ++ impstype t

-----------------------------------------------------------------------------


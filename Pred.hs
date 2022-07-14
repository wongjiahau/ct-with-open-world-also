module Pred where

import List (union,intersect, (\\))
import Id
import SimpleType
import Subst

data Constrained t = [Pred] :=> t          -- notation: k:=>t  -- constrained simple type
                     deriving (Eq, Show)

data Pred   = Constraint Constraint 
              deriving (Eq, Show)

type Constraint = (Id, SimpleType, Bool)  -- boolean indicates "removable" ...
                                          -- ... from k:=>t, if tvs in k do not appear in t

-----------------------------------------------------------------------------

class World a where
  open_world :: a -> Bool

instance World Pred where
  open_world (Constraint _)    = False

closed_or_resolved_open k   = (not $ open_world k) || (null $ tv k)

-----------------------------------------------------------------------------
eqC (Constraint (i, t,_)) (Constraint (i', t',_)) = i==i' && (isRenaming t t')

-----------------------------------------------------------------------------
class Dom a where
  dom:: a -> [Id]

instance Dom Pred where
  dom (Constraint (o,_,_)) = [o]
  
instance Dom a => Dom [a] where
  dom = foldr (union . dom) [] 

rng (Constraint (_,t,_)) = [t]

-----------------------------------------------------------------------------
class Removable a where
  removable:: a -> a

instance Removable Pred where
  removable k@(Constraint (_,_,True)) = k
  removable (Constraint (o,t,False))  = Constraint (o,t,True)

instance Removable a => Removable [a] where
  removable = map removable

isRemovable (Constraint (_,_,True)) = True
isRemovable _                       = False

-----------------------------------------------------------------------------
impConstraint [] = ""
impConstraint ((Constraint (i, t, _)):[]) = (show i) ++ "::"++ impstype t
impConstraint ((Constraint (i, t, _)):xs) = (show i) ++ "::"++ impstype t ++ ", " ++ impConstraint xs 

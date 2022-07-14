module SubsPred where

import Pred
import SimpleType
import Subst
import Unify
import List (union)

instance Subs t => Subs (Constrained t) where
  apply      s (ps :=> t) = apply s ps :=> apply s t
  tv           (ps :=> t) = tv ps `union` tv t

instance Subs Pred where
  apply s (Constraint (o,t,b)) = Constraint (o,apply s t,b)
  tv (Constraint (o,t,b))      = tv t

-----------------------------------------------------------------------------

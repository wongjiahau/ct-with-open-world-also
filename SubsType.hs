module SubsType where

import Type
import Subst
import List (intersect)

instance Subs Type where
  apply s (Forall qt) = Forall (apply s qt)
  tv (Forall qt)      = tv qt

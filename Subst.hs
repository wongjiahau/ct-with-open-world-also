module Subst where

import SimpleType
import Data.List(nub, intersect, union)

type Subst  = [(Tyvar, SimpleType)]

domain = map fst

nullSubst  :: Subst
nullSubst   = []

(+->)      :: Tyvar -> SimpleType -> Subst
u +-> t     = [(u, t)]

infixr 4 @@
(@@)       :: Subst -> Subst -> Subst
s1 @@ s2    = [ (u, apply s1 t) | (u,t) <- s2 ] ++ s1

merge      :: Subst -> Subst -> Maybe Subst
merge s1 s2 = if agree then Just s else Nothing
 where s     = s1 ++ s2
       agree = all (\v -> apply s1 (TVar v) ==
                          apply s2 (TVar v))
                   (domain s1 `intersect` domain s2)

mergeAll :: [Maybe Subst] -> Maybe Subst
mergeAll = foldr cons (Just nullSubst)
  where cons (Just s) (Just s') = merge s s'
        cons _        _         = Nothing

-----------------------------------------------------------------------------
class Subs t where
  apply :: Subst -> t -> t
  tv    :: t -> [Tyvar]

instance Subs SimpleType where
  apply s (TVar u)  =   
                    case lookup u s of
                       Just t  -> t
                       Nothing -> TVar u
  apply s (TAp l r) =  (TAp (apply s l) (apply s r))
  apply s t         =  t

  tv (TVar u)  = [u]
  tv (TAp l r) = tv l `union` tv r
  tv t         = []

instance Subs a => Subs [a] where
  apply s     = map (apply s)
  tv          = nub . concat . map tv

instance (Subs a, Subs b) => Subs (a,b) where
  apply s (a,b) = (apply s a, apply s b)
  tv      (a,b) = tv a `union` tv b

instance (Subs a, Subs b, Subs c) => Subs (a,b,c) where
  apply s (a,b,c) = (apply s a, apply s b, apply s c)
  tv      (a,b,c) = tv a `union` tv b `union` tv c

instance (Subs a, Subs b, Subs c) => Subs (i,a,b,c) where
  apply s (i,a,b,c) = (i,apply s a, apply s b, apply s c)
  tv      (i,a,b,c) = tv a `union` tv b `union` tv c


-----------------------------------------------------------------------------
isRenaming :: SimpleType -> SimpleType -> Bool
isRenaming t1 t2 = (quantify t1) == (quantify t2) 
  where
    quantify t = let s = zip (tv t) (map TGen [0..]) in apply s t 
     
      



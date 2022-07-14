module Unify where

import SimpleType
import Subst

infixr 4 @@@
(@@@)       :: Subst -> Subst -> Subst
s1 @@@ s2    =  [ (u, apply1 s1 t) | (u,t) <- s2 ] ++ s1

class Substit t where
  apply1 :: Subst -> t -> t
  
instance Substit SimpleType where
  apply1 s (TVar u)  = 
                    case lookup u s of
                       Just t  -> t
                       Nothing -> TVar u
  apply1 s (TAp l r) = (TAp (apply1 s l) (apply1 s r))
  apply1 s t         =  t


instance Substit a => Substit [a] where
  apply1 s     =   (map (apply1 s))

instance (Substit a, Substit b) => Substit (a,b) where
  apply1 s (a,b) =   (apply1 s a, apply1 s b)

instance (Substit a, Substit b, Substit c) => Substit (a,b,c) where
  apply1 s (a,b,c) =   (apply1 s a, apply1 s b, apply1 s c)


class MGU t where
  mgu :: (t,t) -> Maybe Subst

varBind :: Tyvar -> SimpleType -> Maybe Subst

instance MGU SimpleType where
  mgu (TAp l r,  TAp l' r') = 
                              do s1 <- (mgu (l,l'))
                                 s2 <- mgu ((apply1 s1 r) ,  (apply1 s1 r'))
                                 Just (s2 @@@ s1)
  mgu (t,        TVar u   ) =  (varBind u t)
  mgu (TVar u,   t        ) =  (varBind u t)
  mgu (TCon tc1, TCon tc2 )
             | tc1==tc2     =  (Just nullSubst)
  mgu (t,t')                =   Nothing 

unifyFails (t1,t2) =  mgu (t1,t2) == Nothing

instance (Substit t, MGU t) => MGU [t] where
  mgu (x:xs, y:ys) = 
                     do s1 <- (mgu (x,y))
                        s2 <- mgu ((apply1 s1 xs), (apply1 s1 ys))
                        return ( (s2 @@@ s1))
  mgu ([]  , []  ) =  return  (  nullSubst)
  mgu _            =   Nothing

varBind u t | t == TVar u   = Just nullSubst
            | u `elem` tv t = Nothing
            | otherwise     = Just (u +-> t)

class Match t where
  match :: (t,t) -> Maybe Subst

instance Match SimpleType where
  match (TAp l r , TAp l' r') = do sl <- match (l,l')
                                   sr <- match (r,r')
                                   merge sl sr
  match (TCon tc1, TCon tc2 )
     | tc1==tc2               = Just nullSubst
  match (TVar u  , t        ) = varBind u t -- was Just (u +-> t) 
  match (t1      , t2       ) = Nothing


instance Match t => Match [t] where
  match (ts,ts') = mergeAll (zipWith (curry match) ts ts')

-----------------------------------------------------------------------------

class MatchOk t where
  matchOk :: (t,t) -> Bool

instance MatchOk SimpleType where
  matchOk (t1, t2) = case match (t1,t2) of
                          Just _  -> True
                          Nothing -> False --error ("Matching:  " ++ (show t1) ++ "\n" ++ 
                                          --          "with    :  " ++ (show t2))
                                      
instance MatchOk t => MatchOk [t] where
  matchOk (ts,ts') =  and (zipWith (curry matchOk) ts ts')

-----------------------------------------------------------------------------
class Unify t where
  unify :: (t,t) -> Subst

instance Unify SimpleType where
  unify (t,t') =  (case mgu (t,t') of
    Nothing -> error ("unification: trying to unify\n" ++ (show t) ++ "\nand\n" ++
                      (show t'))
    Just s  -> s)

instance (Substit t, Unify t) => Unify [t] where
  unify (x:xs, y:ys) =   (s2 @@@ s1) 
     where s1 = unify (x,y)
           s2 = unify (apply1 s1 xs, apply1 s1 ys)
  unify ([]  , []  ) =  nullSubst
  unify _            = error ("unification: lists with different lengths")

-----------------------------------------------------------------------------




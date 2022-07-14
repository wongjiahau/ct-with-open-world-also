module TIMonad where

import Id
import SimpleType
import Subst
import Unify
import Pred
import Type
import List ((\\))
import Debug

-----------------------------------------------------------------------------
newtype TI a = TI (FreshIndex -> (FreshIndex,a))

type FreshIndex = (Int, Int)             -- last fresh tyvar index so far

instance Monad TI where
  return x   = TI (\i -> (i,x))
  TI m >>= f = TI (\i -> let (i',x) = m i; TI fx = f x in  fx i')

runTI      	:: TI a -> a
runTI (TI m) 	= result where (_,result) = m (0, 1) 

freshTVar	:: TI SimpleType
freshTVar  	= TI (\(i, ii) -> let v = Tyvar (enumId i) [] in ((i+1, ii),TVar v))

freshInst (Forall t) = do ts <- mapM (\_ -> freshTVar) [0..maxTGen (-1) t]
                          return (inst ts t)

freshPred (Constraint (i, t, b)) = do ts <- mapM (\_ -> freshTVar) [0..maxTGen (-1) t]
                                      return (Constraint (i, inst ts t, b))  

freshNum	:: TI Int
freshNum  	= TI (\(i,ii) -> ((i,ii+1),ii))
------------------------------------------------------------------------------
class SupInst a where
   supInst::[Tyvar] -> Int -> a -> TI a


instance SupInst SimpleType where
   supInst lbvs n (TAp l r) = do t1 <- supInst lbvs n l 
                                 t2 <- supInst lbvs n r
                                 return $ TAp t1 t2
   supInst lbvs n (t@(TVar tv@(Tyvar v l)))
     | tv `elem` lbvs	 = return t
     | otherwise      	 = return $ TVar $ Tyvar v (n:l)
   supInst _    _ t         = return t

instance SupInst a => SupInst [a] where
   supInst ts n = mapM (supInst ts n)

instance SupInst a => SupInst (Constrained a) where
   supInst ts n (k :=> t) = do k' <- supInst ts n k
                               t' <- supInst ts n t
                               return (k':=>t')

instance SupInst Pred where
  supInst ts n (Constraint (i, t, b)) = do t' <- supInst ts n t
                                           return (Constraint (i, t', b)) 
  
-----------------------------------------------------------------------------
class MaxTGen a where
  maxTGen:: Int -> a -> Int

instance MaxTGen SimpleType where
  maxTGen n (TAp t1 t2) = maxTGen (maxTGen n t1) t2
  maxTGen n (TGen n')   = max n n'
  maxTGen n _           = n

instance MaxTGen a => MaxTGen [a] where
  maxTGen n = foldr f n 
    where f a n' = max (maxTGen n a) n'
instance MaxTGen a => MaxTGen (Constrained a) where
  maxTGen n (p:=>t) = maxTGen (maxTGen n p) t  

instance MaxTGen Pred where
  maxTGen n (Constraint (_, t, _)) = maxTGen n t

-----------------------------------------------------------------------------
class Instantiate t where
  inst  :: [SimpleType] -> t -> t
instance Instantiate SimpleType where
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts (TGen i)  = ts !! i
  inst ts t         = t
instance Instantiate a => Instantiate [a] where
  inst ts           = map (inst ts)
instance Instantiate a => Instantiate (Constrained a) where
  inst ts (p:=>t) = inst ts p:=> inst ts t 
instance Instantiate Pred where
  inst ts (Constraint (i, t, b)) = Constraint (i, inst ts t, b) 

-----------------------------------------------------------------------------

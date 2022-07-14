module Lcg (lcg,lcgn,lcgn',lcgn2, lcgi) where

import Type
import TIMonad
import Id
import SimpleType
import Pred
import Subst

lcg:: Id -> [Type] -> TI (Constrained SimpleType)
lcg i scs = do (qt, _) <- lcg' i scs []; return qt

-----------------------------------------------------------------------------
type Gen = [((SimpleType,SimpleType),Tyvar)]

lcg':: Id -> [Type] -> Gen -> TI (Constrained SimpleType, Gen)
lcg' i [sc]    s = do qt <- freshInst sc; return (qt,s)
lcg' i (sc:scs) s = do qts     <- mapM freshInst scs
                       let ts  =  map (\ (k:=>t) -> t) qts
                       (t',s') <- lcgn' ts s
                       ps:=>t0 <- freshInst sc
                       (t,s'') <- lcgp t0 t' s'
                       return ([Constraint (i,t,False)] :=> t,s'')
lcg' i []      _ = error "Lcg: empty list"

-----------------------------------------------------------------------------
lcgp:: SimpleType -> SimpleType -> Gen -> TI (SimpleType,Gen)
lcgp t1 t2 s = 
  case lookup (t1,t2) s of
    Just a  -> return (TVar a, s)
    Nothing -> lcgp' t1 t2 s

lcgp':: SimpleType -> SimpleType -> Gen -> TI (SimpleType, Gen)
lcgp' t1@(TVar (Tyvar _ _))   t2                    s = do TVar a <- freshTVar; return (TVar a, ((t1,t2),a):s)
lcgp' t1                    t2@(TVar (Tyvar _ _))   s = do TVar a <- freshTVar; return (TVar a, ((t1,t2),a):s)
lcgp' t1@(TCon (Tycon id1)) t2@(TCon (Tycon id2)) s
  | id1==id2  = return (t1,s)
  | otherwise = do TVar a <- freshTVar
                   return (TVar a, ((t1,t2),a):s)
lcgp' t1@(TAp t1a t1r)      t2@(TAp t2a t2r)      s =
  do (ta,s1) <- lcgp t1a t2a s
     (tr,s2) <- lcgp t1r t2r s1
     return (TAp ta tr, s2)
lcgp' t                     t'                    s = do TVar a <- freshTVar; return (TVar a, ((t,t'),a):s)
   
-----------------------------------------------------------------------------
lcgn:: [SimpleType] -> TI SimpleType
lcgn ts = do (t, _) <- lcgn' ts []; return t

lcgn':: [SimpleType] -> Gen -> TI (SimpleType,Gen)
lcgn' [t]        s = return (t,s)
lcgn' [t1, t2]   s = lcgp t1 t2 s
lcgn' (t1:t2:ts) s = do (ta,s1) <- lcgn' ts s; (tb,s2) <- lcgp t1 t2 s1; lcgp ta tb s2

lcgn2:: [SimpleType] -> Gen -> TI (SimpleType,Gen)
lcgn2 [t]        s = return (t,s)
lcgn2 [t1, t2]   s = lcgp t1 t2 s
lcgn2 (t1:t2:ts) s = do (ta,s1) <- lcgp t1 t2 s
                        if var ta then return (ta, s1)
                         else 
			    do 
                               (tb, s2) <- lcgn2 ts s1
                               lcgp ta tb s2     

	where
           var (TVar _) = True
           var _        = False
-----------------------------------------------------------------------------
	       
lcgS::	[SimpleType] -> [Subst] -> TI [SimpleType]
lcgS ts ss = do (ts,_) <- lcgS' ts ss []; return ts

lcgS' []      _  s = return ([],s)
lcgS' ts      [] s = return (ts,s)
lcgS' (t: ts) ss s =
 do (t1,s1) <- lcgn' ([ apply sj t | sj <- ss]) s
    (ti,s2)  <- lcgS' ts ss s1
    return (t1: ti, s2)

-----------------------------------------------------------------------------

lcgi:: [SimpleType] -> TI (Maybe SimpleType)
lcgi [] = return Nothing
lcgi ts = do t <- lcgn ts; return (Just t)
module Pat where

import Id
import SimpleType
import Pred
import Type
import Assump
import TIMonad
import Lit
import Unify
import Subst
import Data.List (union)
import Tup3
import Debug

data Pat        = PVar Id
                | PLit Literal
                | PCon Assump [Pat]
                | PWildcard
                | PAs  Id Pat
--                | PNpk Id Integer
                | PLazy Pat
                deriving (Eq, Show)

tiPat :: TypCtx -> Pat -> TI (Constrained SimpleType, TypCtx)

tiPat _ (PVar i) 		    = do v <- freshTVar
                    		         return ([]:=>v, [i:>:(LB, toType v)])

tiPat _ (PLit l)          	  = tiLit l

tiPat g0 (PCon (i:>:(_,sc)) pats) = do (ps,ts, g)  <- tiPats g0 pats
                                       t'         <- freshTVar
                                       (qs:=>t1)  <- freshInst sc
                                       ((qs1:=>t2), _) <- tc i g0
                                       let s1 = unify (t1, t2)
                                           s  = unify (apply s1 t2, foldr fn t' ts)  
                                           ss = s @@ s1                                         
                                       return (apply ss ps `union` apply ss qs1 :=> apply ss t', apply ss g)

tiPat _ PWildcard  		= do v <- freshTVar
				     return ([]:=>v, [])

tiPat g0 (PAs i pat) 		= do (ps:=>t,g) <- tiPat g0 pat
				     return (ps:=>t, g |+ [i:>:(CW, toType t)])

tiPat g0 (PLazy p)		= tiPat g0 p

tiPats :: TypCtx -> [Pat] -> TI ([Pred], [SimpleType], TypCtx)
tiPats g pats 			= do qtsgshs <- mapM (tiPat g) pats
				     let qts = map fst qtsgshs
                                         gs  = map snd qtsgshs 
				         (pss,ts) = unzip (map (\ (ps:=>t) -> (ps,t)) qts)
				         ps = bigUnion pss
				     return (ps, ts, bigUnion gs)
-----------------------------------------------------------------------------

idsInPats = foldr ((**) . idsInPat) [] 
  where (**) :: [Id] -> [Id] -> [Id]
        []     ** ids = ids
        [i]    ** ids = i:ids
        (i:is) ** ids = is ** (i:ids)

idsInPat (PVar id)     = [id]
idsInPat (PLit _)      = []
idsInPat (PCon _ pats) = idsInPats pats
idsInPat (PWildcard)   = []
idsInPat (PAs id pat)  = [id] ++ idsInPat pat
idsInPat (PLazy pat)   = idsInPat pat

--------------------------------------------------------------------------------

module Assump where

import Id  
import List((\\))
import SimpleType
import Subst
import SubsType
import Lcg
import TIMonad
import Type
import Pred 
import List hiding (find)
import Unify
import Sing (sing)

import Debug

data Assump = Id :>: (Kind_of_defining_occurrence, Type)  -- notation: i :>: (kdo, sc)
     deriving (Eq, Show)

data Kind_of_defining_occurrence = OW | CW | LB deriving (Eq, Show) 
                               -- open, closed, or lambda-bound defining occurrences
                               -- open assumptions are "assumed to exist somewhere, in an open world 
                               --   (are not "checked for satisfaction")
                               -- closed assumptions come from given definitions 
                               -- lambda bound vars have a single type... 
                               --   (types in different applied occurrences need to be unified)

type TypCtx  = [Assump]             -- notation: g
type SatCtx  = [Assump]             -- notation: h

-----------------------------------------------------------------------------

data ICt = Id :|: Constrained SimpleType deriving Show -- contextualized constrained type assumption

ict_id (i:|:_)    = i
cict_id (ict:^:_) = ict_id ict

--typCtx	:: CICt -> Assump 
--typCtx' ((i :|: ct) :^: (g, _)) =  i :>: (CW, quantify (tv ct \\ tv (lambda_bound_assumptions g))  ct)
typCtx'  ((i :|: ct) :^: g) =  i :>: (CW, quantify (tv ct)  ct)
typCtx'' ((i :|: ct) :^: g) =  i :>: (CW, Forall ct)

toTypCtx = map typCtx'' 

close = map typCtx' 

closeA = map (\(i:>:(ot, Forall t)) -> i :>: (ot, quantify (tv t) t)) 
--------------------------------------------------------------------------------
class Ids a where
  getId :: a -> Id 

instance Ids ICt where
  getId (i:|:_) = i

instance Ids Assump where
  getId (i:>:_) = i 

instance Ids a => Ids (Contextualized a) where
  getId (a:^:_) = getId a 


instance Subs ICt where
  apply s (i :|: ct) = i :|: apply s ct
  tv      (i :|: ct) = tv ct
 
instance Dom ICt where
  dom (i :|: _) = [i]

{-
instance PPrint ICt where
  pprint (i :|: ct) = (text (show i) <+> text ":|:") $$ nest 2 (pprint ct)

-}
ctypes_of	:: Id -> [ICt] -> [Constrained SimpleType]
ctypes_of i icts = [ ct | (i':|:ct) <- icts, i'==i ]

types_of	:: Id -> [ICt] -> [SimpleType]
types_of i icts = [ t | (i' :|: (_:=>t)) <- icts, i'==i ]

-----------------------------------------------------------------------------

instance World Assump where
  open_world (i :>: (OW, sc)) = True
  open_world _                = False

instance Subs Assump where
  apply s (i :>: (oc, sc)) = i :>: (oc, apply s sc)
  tv (i :>: (oc, sc))      = tv sc

findI	:: Id -> TypCtx -> [(Kind_of_defining_occurrence, Type)]
findI i g = [ (oc, sc) | (i' :>: (oc, sc)) <- g, i'==i ]

schemes_of i = (map snd) . findI i

tc     	:: Id -> TypCtx -> TI (Constrained SimpleType, TypCtx)
tc i g0 = do (k:=>t,g) <- tc' i g0 
             return (k:=>t, g)


tc' i g
  | null open_scs = if null closed_scs 
                       then do t <- freshTVar
                               let qt = [Constraint (i,t,False)]:=>t
                                   gi = [i :>: (CW, quantify [] qt)]
                               return (qt, gi)
                       else do qt <- lcg i closed_scs
                               if sing closed_scs 
                                  then let (oc, sc) = head i_scs in do qt <- freshInst sc; return (qt,[i:>:(oc,sc)])
                       	          else return (qt, []) -- [i :>: (CW, quantify (tv qt) qt)]
   | otherwise     = do qt <- freshInst open_i_sc; return (addConst i qt, {-[i :>: (OW, open_i_sc)]-} [])
  
   where (open_scs, closed_scs) = partition' openWorld snd i_scs
         i_scs                  = findI i g
         open_i_sc              = head open_scs -- for any i, there is always at most one open_i_sc
         addConst i (k:=>t)     = (unionBy eqC k [Constraint (i, t, True)]) :=> t
        

partition'     :: (a -> Bool) -> (a -> b) -> [a] -> ([b],[b])
partition' p f = foldr select ([],[])
		 where select x (ts,fs) | p x       = (f x:ts,fs)
			                | otherwise = (ts,f x:fs)

openWorld (OW,_) = True
openWorld _      = False

closeWord (CW,_) = True
closeWord _      = False

-----------------------------------------------------------------------------
-- 
-- as ! v is the restriction of a context as to a set of vars v,
-- giving only those assumptions with variables in v.
--
-- as |* v is the closure of restricting a context as to v, 
-- giving assumptions with vars in v or in any included assumption.
-----------------------------------------------------------------------------

(!) :: [Assump] -> [Id] -> [Assump]
as ! v = foldr f [] as
  where f (o :>: (oc, t)) = if o `elem` v then ((o :>: (oc, t)):) else id

as ||* v
   | null v    = []
   | otherwise = as' ++ (as ||* v')
  where as'         = as ! v 
        as''        = as \\ as' 
        v'          = dom ks \\ v
        ks          = [ k | (x:>: (_,Forall (k:=>_))) <- as']

-----------------------------------------------------------------------------
-- 
-- as |+ as' overrides assumptions of as with those in as' 
--
as |+ as' = as' ++ filter compl as
  where compl (i :>: __) = not (i `elem` (dom as'))

-----------------------------------------------------------------------------

instance Dom Assump where
  dom (i :>: _) = [i]

{-
-- cinsert as as' gives the subset (list) of "as'" having assumptions that 
-- "can" be inserted into "as" (can depends on failure of unification with
-- types of symbols already in "as")

cinsert::	[Assump] -> [Assump] -> [Assump]
cinsert as = filter (cins as)

cins::	[Assump] -> Assump -> Assump
cins as (i :>: (_, sc)) = cins' (closed,open) sc
  where (open,closed) = partition' openWorld snd (findI (i,as))

cins' (open,closed) sc =
  if null open then null closed || ((null $ tv sc) && all (canOv t) closed)
  else (runTI $ do (ps :=> t') <- freshInst (head open); return (match t' t)) /= Nothing
  where t = g_type sc

canOv t (Forall (_:=>t')) = (null $ tv t') && unifyFails (t,t')
-}
-----------------------------------------------------------------------------

bigUnion:: Eq a => [[a]] -> [a]
bigUnion = foldr (union) [] -- should be in List

-----------------------------------------------------------------------------

types_of_common_vars :: TypCtx -> TypCtx -> TI [(SimpleType, SimpleType)]
types_of_common_vars g0 g0' = do let freshInst' (i:>:(oc,sc)) = do ct <- freshInst sc; return (i:|:ct)
                                 g  <- mapM freshInst' g0
                                 g' <- mapM freshInst' g0' 
                                 return [ (t, t') | i :|: (k:=>t)   <- g, 
                                                    i':|: (k':=>t') <- g', i==i']

types_of_common_lambda_bound_vars :: TypCtx -> TypCtx -> [(SimpleType, SimpleType)]
types_of_common_lambda_bound_vars g g' = [ (t, t') | i :>: (_,Forall (_:=>t))  <- lambda_bound_assumptions g, 
                                                     i':>: (_,Forall (_:=>t')) <- lambda_bound_assumptions g', i==i']

lambda_bound_assumptions = filter (\ (i :>: as) -> lambda_bound_assumption as)

lambda_bound_assumption (LB,_) = True
lambda_bound_assumption _      = False

eq_m :: TypCtx -> TypCtx -> [(SimpleType, SimpleType)]
eq_m g g' = [ (t, t') | i :>: (_,Forall (_:=>t))  <- lambda_bound_assumptions g, 
                        i':>: (_,Forall (_:=>t')) <- lambda_bound_assumptions g', i==i', t/=t']

-----------------------------------------------------------------------------

(|-) 	:: TypCtx -> [Id] -> TypCtx
g |- is = filter (\ (i:>:_) -> i `notElem` is) g

-----------------------------------------------------------------------------
{-
instance PPrint Assump where
  pprint (i :>: (kdo,sc)) = (text (show i) <+> text ":>:") $$ nest 2 (pprint (kdo,sc))

instance PPrint Kind_of_defining_occurrence where
  pprint OW = text "OW"
  pprint CW = text "CW"
  pprint LB = text "LB"
-}
-----------------------------------------------------------------------------
types_of'	:: [(Id,SimpleType)] -> Id -> [SimpleType]
types_of' its i = [ t | (i',t) <- its, i'==i ]

-----------------------------------------------------------------------------
data Contextualized a = a :^: TypCtx 

instance (Show a) => Show (Contextualized a) where
  show (a:^:g) = show a ++" :^: " ++ show g ++ "\n\n"
   
type CSType           = Contextualized SimpleType
type CType            = Contextualized Type

type CICt             = Contextualized ICt
type CCtCtx           = [CICt]                    -- "contextualized" constrained typing context

type CAs              = Contextualized Assump     -- "contextualized" type assumption
type CCtx             = [CAs]                     -- "contextualized" typing context
-----------------------------------------------------------------------------
instance Subs a => Subs (Contextualized a) where
  tv      (a :^: g) = (tv a) ++ (tv g) 
  apply s (a :^: g) = (apply s a :^: apply s g)


{-
instance PPrint a => PPrint (Contextualized a) where
  pprint (a :^: (g,h)) = pprint a <+> text ":^:" <+> pprint (g,h)

-}
-----------------------------------------------------------------------------
--freshIct :: [Tyvar] -> ICt -> TI ICt
freshIct lbvt ((_:|: ct) :^: g) = do let ts = (tv ct `union` tv g) \\ lbvt
                                     ntv <- mapM (\_ -> freshTVar) [0..length ts]
                                     let s = zip ts ntv
                                     return (apply s ct, apply s g)

-------------------------------------------------------------------------------
impType [] = ""
impType ((i :>:(_,Forall t)):xs) = (show i) ++ "::" ++ iType t ++ "\n" ++ impType xs

iAssump (Forall t) = iType t 

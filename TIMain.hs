module TIMain where

import Char
import List((\\), intersect, union, nub, sortBy, deleteBy, unionBy, find)
import Id
import SimpleType
import Subst
import Pred
import Type
import Assump 
import TIMonad
import Lit
import Pat
import Unify
import Monad (foldM)
import Lcg (lcg)
import PreDefs (nilC, consC, trueC, falseC)
import Tup3
import Lcg
import Expr
import Sat
import Debug

-----------------------------------------------------------------------------
ap              = foldl1 Ap
evar v          = (Var v)
elit l          = (Lit l)
econst c        = (Const c)
elet g e        = foldr Let e (map toBg g)

toBg           :: [(Id, Maybe Type, [Alt])] -> BindGroup
toBg g          = ([(v, t, alts) | (v, Just t, alts) <- g ], 
                   [(v,alts)     | (v,Nothing, alts) <- g ])


eNil            = econst nilC
eCons x y       = ap [ econst consC, x, y ]

ecase d as      = elet [[ (toid "_case", 	Nothing, [([p],e) | (p,e) <- as]) ]] (ap [evar (toid "_case"), d])

eif c t f       = If c t f
-- eif c t f       = ecase c [(PCon trueC [], t),(PCon falseC [], f)] 

elambda alt     = elet [[ (toid "_lambda", 	Nothing, [alt]) ]] (evar (toid "_lambda"))

eguarded        = foldr (\(c,t) e -> eif c t e) efail

efail           = Const (toid "FAIL" :>: (CW, Forall ([] :=> TGen 0)))

esign e t       = elet [[ (toid "_val", Just t, [([],e)]) ]] (evar (toid "_val"))

eCompGuard e c  = eif e c eNil
eCompLet bgs c  = elet bgs c
eListRet e      = eCons e eNil

-----------------------------------------------------------------------------
tiExpr :: TypCtx -> Expr -> TI ((Constrained SimpleType, TypCtx), [CICt]) 
tiExpr g (Var i) 		= do {ty <- tc i g; return (ty,[])}
                                     

tiExpr g (Const (i:>:(oc, sc))) = do (k:=>t) <- freshInst sc
                                     ((k1:=>t1), _) <- tc i g
                                     let s  = unify (t, t1)
                                         k' = apply s k1
                                         g' = apply s g
                                         t' = apply s t
                                     return ((k':=>t', []), [])

tiExpr g (Lit l) 		= do {ty <- tiLit l; return (ty,[])}

tiExpr g0 (Ap e1 e2) 		= do ((k1:=>t1,g1), ctx1) <- tiExpr g0 e1
				     ((k2:=>t2,g2), ctx2) <- tiExpr g0 e2
				     a               <- freshTVar
				     let s1  = unifyMsg e1 (t1, fn t2 a)
                                         g1' = apply s1 g1
                                         g2' = apply s1 g2
                                         s2  = unify (unzip $ types_of_common_lambda_bound_vars g1' g2')
				         s   = s2 @@ s1
                                         k1' = apply s k1
				         k2' = apply s k2 
				         t   = apply s a
				         g   = apply s2 g1' `union` apply s2 g2'
				         k2r = k2' |* (tv (apply s t2)) {- k2r marked as removable -}
				         k   = (k1' `union` (k2' \\ k2r)) `union` (removable k2r)
                                     return ((k:=>t, g), apply s (ctx1 ++ ctx2))



tiExpr g0 (Let bg e)		= do n <- freshNum 
                                     let r   = getRen n bg
                                         bg' = rename r bg
                                         e'  = rename r e
                                     (_,ict) <- tiBindGroup' g0 bg' True
                                     ((t, g), ctx') <- tiExpr g0 e'
                                     let bg_is   = getBgIds bg'
                                         ict' = filter (\((i:|: _):^:_) -> i `elem` bg_is) ict
                                         gi   = bigUnion $ map (\ (_:^: g) -> g ) ict'
                                         lbtv = tv (lambda_bound_assumptions g)
                                     (s, g') <- uniReqInf lbtv ict' g
                                     let s1 = unify $ unzip $ types_of_common_lambda_bound_vars g  g'
                                         s2 = unify $ unzip $ types_of_common_lambda_bound_vars g  gi
                                         s3 = unify $ unzip $ types_of_common_lambda_bound_vars g' gi
                                         ss = s3 @@ s2 @@ s1 @@ s
                                     return ((apply ss t, apply ss g' `union` apply ss g), 
                                              apply ss (ict ++ ctx'))
  where
   uniReqInf _ _ [] = return (nullSubst, [])
   uniReqInf vs cctx (x:xs) = do (s,  g)  <- uri vs cctx x 
                                 (ss, gs) <- uniReqInf vs cctx xs 
                                 return (s @@ ss, g++gs)
   
   uri vs ict (i:>:(_,sc)) = do let i_ct = filter (\((i' :|: _) :^: _) -> i' == i) ict
                                case i_ct of
                                      []         -> return (nullSubst, []) 
                                      (i_ct':[]) -> do
                                                       (k':=>t') <- freshInst sc
                                                       (k:=>t,g) <- freshIct vs i_ct'
                                                       let s  = unifyMsg i (t', t)
                                                           g' = apply s g
                                                       return (s, g')
                                      _          -> error ("local overload is not allowed ("++show i++")") 
                                             



tiExpr g (Lam alt) 		= tiAlt g alt

--tiExpr g0 (If e e1 e2) 	= tiExpr g0 (eif e e1 e2)

tiExpr g0 (If e e1 e2)          = do ((k :=>t, g), ctx1) <- tiExpr g0 e
                                     ((k1:=>t1, g1), ctx2) <- tiExpr g0 e1
                                     ((k2:=>t2, g2), ctx3) <- tiExpr g0 e2
                                     let s0  = unifyMsg e (t, tBool)
                                         s1  = unifyMsg e1 (t1, t2)
                                         pts1 = types_of_common_lambda_bound_vars g g1
                                         pts2 = types_of_common_lambda_bound_vars g g2 
                                         pts3 = types_of_common_lambda_bound_vars g1 g2
                                     let ss1  = unify (unzip pts1)
                                         ss2  = unify (unzip pts2)
                                         ss3 = unify (unzip pts3)
                                         ss = (((ss3 @@ ss2) @@ ss1) @@ s1) @@ s0
                                         kk = (apply ss k) `union` (apply ss k1) `union` (apply ss k2)
                                         gg = (apply ss g) `union` (apply ss g1) `union` (apply ss g2)
                                     return ((kk :=> apply ss t1, gg), apply ss (ctx1 ++ ctx2 ++ ctx3))


-----------------------------------------------------------------------------

tiAlt :: TypCtx -> Alt -> TI ((Constrained SimpleType,TypCtx), [CICt])
tiAlt g0 (pats, e) 		=  do (k,ts,g1) <- tiPats g0 pats
				      ((k':=>t,g), ct) <- tiExpr (g0 |+ g1) e
                                      pts <- types_of_common_vars g g1
                                      let s = unify $ unzip $ pts
				      return ((apply s k `union` apply s k' :=> foldr fn (apply s t) (apply s ts), 
                                              apply s (g |- idsInPats pats)), ct) 

tiAlts :: TypCtx -> [Alt] -> SimpleType -> TI ((Constrained SimpleType,TypCtx), [CICt])
tiAlts g0 alts t0 		= do let f ((k:=>t,g), ctx) alt = do ((k':=>t',g'), ctx') <- tiAlt g0 alt
 				    				     let s = unify (t,t')             
				                                     return ((apply s k `union` apply s k' :=> apply s t', 
					    			              apply s g  `union` apply s g'), apply s (ctx ++ ctx'))
				     foldM f (([]:=>t0,[]),[]) alts
-----------------------------------------------------------------------------
tiImpl :: TypCtx -> [Impl] -> TI [CICt]
tiImpl g0 []                  = return []
tiImpl g0 ((i, alts):xs)      = do t <- freshTVar
                                   ((ct,g), ctx) <- tiAlts g0 alts t
                                   cts <- tiImpl g0 xs
                                   return ((((i :|: ct) :^: g):cts)++ctx)



tiExpl :: TypCtx -> [Expl] -> TI [CICt]
tiExpl g0 []                  = return []
tiExpl g0 ((i, te, alts):xs) = do t  <- freshTVar
                                  ((k:=>t',g), ctx) <- tiAlts g0 alts t
                                  (_:=>te') <- freshInst te
                                  case match (t', te') of
                                       Just s  -> do  
                                                    cts <- tiExpl g0 xs
                                                    return ((((i :|: apply s (k:=>t')) :^: g):cts)++ctx)
                                       Nothing -> error ("Signature "++ show te ++ " too general")

tiOW = map (\(i,t,_) -> i :>: (OW, t))  


-----------------------------------------------------------------------------
tiBindGroup :: TypCtx -> BindGroup -> Bool -> TI ([Assump],[Assump])
tiBindGroup g bg inner =
    do (gow, c_infTypings) <- tiBindGroup' g bg inner  
       g_inf <- remSelfC (toTypCtx (c_infTypings))
       --return g_inf
       return (gow, remNotOverConstraint (g++gow) g_inf)

tiBindGroup' :: TypCtx -> BindGroup -> Bool -> TI ([Assump], [CICt]) --Mudar p/ Este depois
tiBindGroup' g (annot_bg,infer_bg) inner = 
    do let infer_bg' = filter (not.isUpper.head.(\(Id i _,_) -> i)) infer_bg   
           const     = filter (isUpper.head.(\(Id i _,_) -> i)) infer_bg
           ow        = filter (\(_,_, p) -> p == []) annot_bg  
	   annot_bg' = filter (\(_,_, p) -> p /= []) annot_bg

       c_const  <- {-# SCC "Inf" #-} (tiImpl g const)  
       c_const' <- {-# SCC "Uni_ReqInf" #-} unify_inf_req g c_const
       let gc = g ++ (close c_const') ++ tiOW ow 
       c_annot  <- {-# SCC "Inf" #-} (tiExpl gc annot_bg')
       c_infer  <- {-# SCC "Inf" #-} (tiImpl gc infer_bg')
       let c_infTypings = c_annot ++ c_infer
       -- if debug "I" c_infTypings inner then return (c_infTypings)
       if inner then return ([], c_infTypings)
          else 
              do c_infTypings' <- {-# SCC "Uni_ReqInf" #-} (unify_inf_req gc c_infTypings)
                 return (tiOW ow, c_infTypings')
-----------------------------------------------------------------------------

unify_inf_req:: [Assump] -> [CICt] -> TI [CICt]
unify_inf_req g c_infTypings
  = do let (is, t_i, g_i) = (map (\(i:|:_) -> i) (map fstCons c_infTypings), map (\(_:|:kt) -> kt) (map fstCons c_infTypings), map sndCons c_infTypings)  
           all_g_i        = concat $ g_i
           itg            = verifyDefs (is ++ map getId g) is t_i g_i              
       tsReq_tsInf_vs <- foldM (getTs g itg) [] all_g_i
       s              <-  unif_and_app tsReq_tsInf_vs
       let t_i'  = map (apply s) t_i
           g_i' = map (apply s) g_i 
           icts  = map (\(i, t_i') -> i:|:t_i') (zip is t_i')
       return (zipWith (:^:) icts g_i')       
 where
   unif_and_app tsReq_tsInf_vs  = 
    do tsReq_tsInf_vs' <- supInf (zip [0..] tsReq_tsInf_vs)
       let s                = (unifyMsg' $ unzip43 tsReq_tsInf_vs')
           tsReq_tsInf_vs'' = (map (apply s) tsReq_tsInf_vs)
       if stop s ((map snd4 tsReq_tsInf_vs) ++ (map trd4 tsReq_tsInf_vs))
         then return s 
         else if {-# SCC "Circ" #-}(circular_dep s) 
                then error ("Cannot (semi-)unify inferred with required types\n" ++
                            "Well, one day we hope to give you better error messages... :-)" )
                else 
                    do s'  <- unif_and_app tsReq_tsInf_vs''
                       return (s' @@ s) 
   stop s ts = (null (domain (filter ren s) `intersect` tv ts))
 
   verifyDefs _ [] [] [] = []
   verifyDefs ids (i:is) (t:ts) (g:gs) = if null ig  then (i, (t, g)):verifyDefs ids is ts gs else error ("Undefined variable " ++ show (head ig) ++ " in function " ++ (show i)) 
     where 
        ig = nub (map getId (filter (\(_:>:(ot,_)) -> ot /= LB) g))   \\ ids 

ren ((Tyvar v _),(TVar (Tyvar v' _))) | v == v'   = False | otherwise = True  
ren _ = True                              
   
-----------------------------------------------------------------------------
supInf n_req_inf_gs = do ti' <- mapM (\(n, (_, _, ti, g)) -> supInst [] n ti) n_req_inf_gs 
                         let id  = map (fst4.snd) n_req_inf_gs
                             tr' = map (snd4.snd) n_req_inf_gs
                             gs' = map (fth4.snd) n_req_inf_gs
                         return (zip4 id tr' ti' gs')     

--getTs :: [Assump] -> [(Id,(Constrained SimpleType, [Assump]))] -> [(SimpleType,SimpleType,[Assump])] -> Assump -> TI [(Id, SimpleType,SimpleType,[Assump])]

getTs g i_ts pts (i:>:(ot,sc@(Forall (k :=> t)))) =
    do maybe_t <- getT (i,sc) 
       case maybe_t of
          Just (t', vs) -> return ((i,t,t',vs) : pts)
          Nothing       -> return pts 
 where
   getT (i,sc) = do if ot == LB then return Nothing
                       else
                         do let g'  = findI i g
                                gow = filter (\(x,_) -> x == OW) g'
                            if null gow then
                               do let tgs = map snd (filter (\(i', _) -> i' == i) i_ts)
                                  case tgs of
                                       []                -> if (null g') && (not $ isDigit (head (show i)))  
                                                               then error ("Undefined variable "++ (show i))
                                                               else return (Nothing) 
                                       (((k:=>t'),g):[]) -> return (Just (t',lambda_bound_assumptions g))
                                       tgs               -> over i sc t tgs
                               else return Nothing   

   over i sc t tgs = do let scs = map (\(t', g') -> quantify (tv t' \\ tv (lambda_bound_assumptions g')) t') tgs 
                        ts' <- mapM freshInst scs
                        let ftgs  =  zip ts' tgs 
                            ftgs' = filter (\((_ :=> t'), _) -> not $ unifyFails (t',t)) ftgs
                        if null ftgs' 
                           then error ("There is not instance of "++ 
                                        show i ++ " that unify with " ++ show t)
                           else
                             if length ftgs' == 1 then return (Just ((\(k:=>t) -> t) (fst $ snd $ head ftgs'),lambda_bound_assumptions $ snd $ snd $ head ftgs'))  -- Necessita verificar recursao polimorfica e sobrecarga????                     
                                else  
                                  do let sc_u = map (\(_,(t', g')) -> quantify (tv t' \\ (tv $ lambda_bound_assumptions g')) t') ftgs'
                                         lbg  = concat $ map (\(_,(_, g')) -> lambda_bound_assumptions g') ftgs'  
                                     (k :=> tu') <- lcg i sc_u
                                     if quantify (tv (k:=>tu') \\ tv lbg) (k:=>tu') == sc
                                        then return Nothing
                                        else return (Just (tu', lbg))
 

-----------------------------------------------------------------------------

circular_dep	:: Subst -> Bool
circular_dep    = fst . foldr circ (False,[])

circ		:: (Tyvar,SimpleType) -> (Bool,Subst) -> (Bool,Subst)
circ _     (True,s)  = (True,s)
circ (u,t) (False,s) 
  | xi (TVar u) t'   = (True,s)
  | otherwise        = (False,(u,t'):s)
  where t' = apply' s t
        apply' s u@(TVar (Tyvar v (i:l)))  = 
           case lookup (Tyvar v l) s of
                Just t  -> t
                Nothing -> u 
        apply' s (TAp l r) = TAp (apply' s l) (apply' s r)
        --apply' s t         = t

        apply' s t =  apply s t 

xi (TAp l r) (TAp l' r') = xi l l' || xi r r'
xi (TVar v)  (TAp l r)   = xi' v l || xi' v r
xi _          _          = False

xi' v           (TAp l r)            = xi' v l || xi' v r
xi' (Tyvar v l) (TVar (Tyvar v' l')) = v==v' && l `subStr` l'
xi' _           _                    = False

subStr	 		:: IneqIndexes -> IneqIndexes -> Bool
subStr [] _             = True
subStr l (_:l')         = l == l' || l `subStr` l'
subStr _  _             = False

remConstraints (a, b) = (map (\(k :=> t) -> t) a, map (\(k :=> t) -> t) b)  

fstCons (a :^: b) = a
sndCons (a :^: b) = b


---------------------------------------------------------------------------------------------

fth4 (_, _, _, x) = x
trd4 (_, _, x, _) = x
snd4 (_, x, _, _) = x
fst4 (x, _, _, _) = x


trd' (_, _, x) = x
snd' (_, x, _) = x
fst' (x, _, _) = x

unzip32 :: [(a,b,c)] -> ([a],[b])
unzip32 = foldr (\(a,b,_) ~(as,bs) -> (a:as, b:bs)) ([], [])

unzip43 = foldr (\(a,b,c,_) ~(as,bs,cs) -> (a:as, b:bs, c:cs)) ([], [], [])

zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a, b, c, d):zip4 as bs cs ds
zip4 _ _ _ _ = []


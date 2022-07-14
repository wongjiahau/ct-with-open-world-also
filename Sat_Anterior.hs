module Sat where

import Char
import Id
import SimpleType
import Type
import Pred
import Assump
import TIMonad
import Monad (foldM)
import Subst
import Lcg
import Unify
import List 
import Sing (sing)
import Maybe
--import Trace
import Debug


-----------------------------------------------------------------------------------------------
nonOverlapping _ []     = []
nonOverlapping g (a:as) = if null over
                             then (a:nonOverlapping g as)
                             else
                               error ("Overlapping\n" ++ impType [a] ++ impType over) 
                         
  where
    over = filter (\a' -> typ a' /= typ a && mgu (typ a', typ a) /= Nothing) g
    typ (_:>:(_, Forall(_:=>t))) = t 

remSelfC []     = return []
remSelfC (a:as) = do let i   = getId a
                         ds  = takeWhile (\a' -> getId a' == i) as  
                         es  = dropWhile (\a' -> getId a' == i) as
                         ds' = nonOverlapping (a:ds) (a:ds)
                     ds'' <- mapM (remSelfC' ds') ds'
                     es'  <- remSelfC es
                     return (ds'' ++ es') 
                     
  

remSelfC' g a@(i:>:(ot, Forall kt@(k:=>t))) = 
   do (_:=>tf) <- freshInst (quantify (tv kt) kt)
      let ki = filter (\(Constraint (i', t', _)) -> i /= i' ||  (t /= t' && (not $ polyRec (g \\ [a]) t' tf))) k
      return (i:>:(ot, Forall (ki :=>t)))

 


polyRec g t tf = matchOk (tf, t) && (foldl (&&) True (map (\(_:>:(_, Forall (_:=> t'))) -> mgu (tf, t') == Nothing) g))

simplify = map simpl 
  where 
    simpl (i:>:(ot, Forall (k:=>t))) = let (k':=>t') = rem k:=>t in 
                                           if {-# SCC "Amb2" #-} (ambiguity k' (tv t')) then error ("Function " ++ (show i) ++ " is ambiguity:\n" ++ iAssump (quantify  (tv (k' :=> t')) (k' :=> t')))  
                                              else (i:>:(ot, Forall (rem k':=>t')))
    rem = filter (\(Constraint (_, t, _)) -> not $ null (tv t)) 

satAssump g0 gow a@(i:>:(ot, Forall t)) = do let k = getK' t
                                             if null k then return a
                                                else
                                                 do 
                                                    s'  <- sat (isOver g0) gow i t 
                                                    -- *** ALTERAR verificar projecao de restricoes e se for o caso fechar o mundo.
                                                    return (i:>:(ot, Forall (apply s' t)))

{-

closeOWAssump gow kt@(k:=>t) = do let k' = k |* tv t
				      ck = k \\ k'
				  if null ck then return kt 
                                     else
                                      do 
                                         				      
  

   

-}

satDrive g0 g = do g0'<- mapM freshA g0
                   g' <- mapM freshA g
                   --gr <- remSelfC g'  
                   let ga  = g' `union` g0'
		       gow = filter open_world ga
                   g''  <- {-# SCC "SAT" #-} (mapM (satAssump ga gow) g')
                   return g''     

freshA (i:>:(o, t)) = do t' <- freshInst t 
                         return (i:>: (o, Forall t'))



intersectS  [] sg         = return nullSubst
intersectS ((v, t):ss) sg = do let ts  = map snd (filter (\(v',_) -> v == v') ss)
                                   sn = filter (\(v',_) -> v /= v') ss
                               (t', sg') <- lcgn' (t:ts) sg
                               ss'       <- intersectS sn sg'
                               return ((v, t'):ss')



constraint :: SatC -> [Pred] 
constraint (c, _, _, _) = c

assumpC :: SatC -> [Assump]
assumpC (_, a, _, _) = a

constraintKI :: SatC -> [Pred] 
constraintKI (_, _, k, _) = k

xiC :: SatC -> Int 
xiC (_, _, _, i) = i

 
constraintP [] = []
constraintP (c@(Constraint (i, t,_)):cs) = [(fst ks)] ++ (constraintP (snd ks))
   where
       ks = tclose (tv t) [c] cs 
       

tclose _  cs [] = (cs, [])
tclose ts cs k  = if kp /= [] then tclose (ts `union` (tv kp)) (cs++kp) ko  
                  else (cs, ko)
   where
     kp = filter (\x -> ((tv x) `intersect` ts) /= []) k
     ko = filter (\x -> ((tv x) `intersect` ts) == []) k



sat g0 gow i kt@(k:=>t) = do let kow = filter (owConstraint gow) k
				 kps = constraintP (k \\ kow)
                             rs <- mapM (\k -> (satsO (k \\ kow, g0, [], 3))) (debug "Teste" (k \\ kow, kps) kps)
                             if any null rs then error ("Function " ++ show i ++ ": Constraint\n" ++ impConstraint k ++ "\nis not satisfiable")
			        else 
                                  do 
                                     ss <- mapM (\r -> intersectS (concat r) []) (amb1 i kt t rs)
                                     return (concat ss)


amb1 i kt t [] = []
amb1 i kt t (s:ss) = if {-# SCC "Amb1" #-} (repeated ts) then error ("Function " ++ (show i) ++ " is ambiguity*:\n" ++ iAssump (quantify  (tv kt) kt))
                     else (s:amb1 i kt t ss)
  where
        kts  = map (\s -> quantifyParc (apply s kt)) s
        ts   = map (\(k:=>t) -> t) (remEquals kts)
                                    
         

repeated []     = False
repeated (x:[]) = False
repeated (x:xs) = if (elem x xs) then True else repeated xs  

remEquals [] = []
remEquals (t:ts) = if elem t ts then remEquals ts else t:remEquals ts



type SatC = ([Pred], [Assump], [Pred], Int)


concatUnion x = foldr union [] x 

{-
satsOW gow (ks, g, ki, xi) = 
                do let kow = filter (owConstraint gow) ks
                   ss <- satsO (ks \\ kow, g, ki, xi)
                   return ss
-}


owConstraint gow (Constraint (i, t, _)) = (filter (\(i' :>: (OW, Forall (_:=>t'))) -> i == i') gow) /= []


    

satsO :: SatC -> TI [Subst]
satsO (ks, g, ki, xi) = 
                do if (null ks) then return [nullSubst]
                    else
                      do 
                         let kps = constraintP ks
                         rs <-  (mapM (\k -> (sats (k, g, ki, xi))) kps)
                         if any null rs then return []
                          else
                             do 
                                ss <- mapM (\r -> intersectS (concat r) []) rs
                                let ss' = compS [] ss
                                    rs' = map (\(s, r) -> map (s @@) r) (zip ss' rs)
                                return (concat rs')


compS _ [] = []
compS sa (s:ss) = ((foldr  (@@) nullSubst (sa++ss)):(compS (sa++[s]) ss))

sats :: SatC -> TI [Subst]
sats (ks, g, ki, xi) = 
               do if (null ks) then return [nullSubst]
                    else
                      do
                        s_gs <- satset (map (\x -> (x, g)) ks)
                        let (ss, gs) = unzip s_gs
                        if {- debug "GS" (impType $ concat gs)-} (null s_gs) then return []
                          else
                             do
                               let kga = map (\(s, g) -> (concatUnion (map (\a -> (filterR (getK (apply s a)))) g), g)) s_gs
                                   ss_sat  = (consSat' ks ss (g, ki, xi) kga)
                               if (null ss_sat)  then return []
                                  else
                                      do
                                         r <- mapM sats' ss_sat
                                         return (concat r) 
                                          
  
  where     
     sats' (sj, c_gs) = do ss <- satsO c_gs
                           return (map (@@ sj) ss)
                    

filterR xs = filter (\x -> tv x /= []) xs



consSat' ks ss gxk c_sat = let ss_sat = zip ss (map (contextSat ks gxk) c_sat) 
                           in map (\(s, Just x) -> (s, x)) (filter (\(_,x) -> x /= Nothing) ss_sat)

contextSat :: [Pred] -> ([Assump], [Pred], Int) -> ([Pred], [Assump]) -> (Maybe SatC)
contextSat ks (g, ki, xi) (k, a) =
                                if (anyStrictMatch k ki {- && isNotRenaming k ki -}) then  {- debug "CASO 1"  (impConstraint k, "***", impConstraint ki) -} (Just (k, gOtm, ki', xi))
                                   else
                                     if (anycMatch ki k) then  {- debug "CASO 2" (impConstraint k, "***", impConstraint ki, "RRRR", a) -}  (Just (k, gOtm \\\ a , ki', xi)) 
                                       else
                                         if (xi > 0) then   debug "****CASO 3" (impConstraint k, "***", impConstraint ki)   (Just (k, gOtm, ki', xi-1) )
                                            else
                                              debug "****CASO 4" ((anyStrictMatch k ki),impConstraint k, "***", impConstraint ki) Nothing
  where
    gOtm = if (constraintP k/= [] && all (eqCl ks) (constraintP k)) then  g \-\ ks else g
    ki' = unionBy eqC ki ks 
    
    match c g = case match' c g of
                  Just a  -> [a]
                  Nothing -> []
    match' (Constraint (i, t, _)) g = find (\(i':>:(_, Forall (_:=>t'))) -> i == i' && matchOk (t,t')) g 
    
    (\\\)  g []      = g 
    (\\\)  g (a:as)  = (deleteBy eqA a g) \\\ as

    (\\\\)  k []      = k 
    (\\\\)  k (c:cs)  = (deleteBy eqC c k) \\\\ cs


isAllRenaming [c]    ks = elem_by eqC c ks	
isAllRenaming (c:cs) ks = elem_by eqC c ks && isAllRenaming cs ks  


isNotRenaming [] _ = True
isNotRenaming _ [] = True
isNotRenaming cs ks = not (isAllRenaming cs ks)

elem_by _  _ []		=  False
elem_by eq y (x:xs)	=  x `eq` y || elem_by eq y xs


eqA (i1:>:(_, Forall (k1:=>t1))) (i2:>:(_, Forall (k2:=>t2))) = (i1 == i2) && (eqCl k1 k2) && (isRenaming t1 t2) 

eqCl [] []         = True
eqCl (x:xs) (y:ys) = (eqC x y) && (eqCl xs ys)
eqCl _ _           = False

remC = foldl (flip (deleteBy eqC))

cMatch (Constraint (i, t, _)) = any (\(Constraint (i', t', _)) -> i == i' && matchOk (t, t')) 
kMatch (Constraint (i, t, _)) = any (\(Constraint (i', t', _)) -> i == i' && matchOk (t', t))

anyStrictMatch _ []  = True
anyStrictMatch cs ks = any (strictMatch ks) cs
strictMatch ks (Constraint (i, t, _)) = any (\(Constraint (i', t', _)) -> i == i' && 
         ((matchOk (t,t') && (not $ isRenaming t t') && t /= t') || newConstructor t t')) ks


newConstructor (TCon (Tycon i)) (TCon (Tycon i')) = i /= i'
newConstructor (TAp t1 t2) (TAp t1' t2')          = (newConstructor t1 t1' || newConstructor t2' t2')
newConstructor (TVar v) _                         = False
newConstructor _        (TVar v)                  = False
newConstructor _        _                         = False
                                               

anyMatch (Constraint (i, t, _)) ks = all (\(Constraint (i', t', _)) -> i == i' && matchOk (t', t)) ks 

anycMatch _  [] = True
anycMatch k1 k2 = any (\c -> cMatch c k2) k1  

allMatch [] _  = False
allMatch k1 k2 = all (\x -> cMatch x k1) k2 

getK  (_:>:(_, Forall (k :=> _))) = k
getK' (k :=> _) = k


(\-\) g ks = (filter (\(_:>:(_, Forall (k:=>_))) -> not (all' (eqMatchCl ks) (constraintP k))) g)  \--\ ks

(\--\) g [] = g
(\--\) g (k':ks') = (filter (\(_:>:(_, Forall (k:=>_))) -> not (all' (eqMatchCl [k']) (constraintP k))) g)  \--\ ks'

eqMatchCl [] []         = True
eqMatchCl (x:xs) (y:ys) = (eqMatchC x y) && (eqMatchCl xs ys)
eqMatchCl _ _           = False

all' f l = l /= [] && all f l

eqMatchC (Constraint (i, t,_)) (Constraint (i', t',_)) = i==i' && matchOk (t', t)   





----------------------------------------------------------------
satset:: [(Pred, [Assump])] -> TI [(Subst, [Assump])] 
satset []             = return []
satset ((Constraint (i, t, _), gi):k_gs) = do gu  <- eqId i gi
                                              let s_g = unifs gu 
                                              if null s_g then return []
                                                 else 
                                                   do                                                       
                                                      s_g' <- mapM (f k_gs) s_g
                                                      return (concat s_g') 
  where
    eqId i gi      = do let gs = filter (\(i':>:_) -> i == i') gi
                        gs' <- mapM freshA (closeA gs)
                        return  gs'
                        
    f k_gs (s, gu)  = do if null k_gs then return [(s, gu)] 
                            else 
                              do
                                s_gs <- satset (zip (apply s (map fst k_gs)) (map snd k_gs)) 
                                return (conSat s gu s_gs) 
    
    conSat:: Subst -> [Assump] -> [(Subst, [Assump])] -> [(Subst, [Assump])]    
    conSat s g sgs = if null sgs then [] 
                        else
                          let s' = map (@@ s) (map fst sgs)
                              g' = map (g`union`) (map snd sgs)
                          in  zip  s' g' 

    unifs gs = let f a@(_ :>: (_, Forall (_:=>t'))) = case mgu (t, t') of
                                                           Just s  ->  [(s,[a])]
                                                           Nothing ->  [] 
               in concat (map f gs)
   
-----------------------------------------------------------------------------------------------
compareId i1 i2 | getId i1 == getId i2  = EQ
                | getId i1 <= getId i2  = LT
		| otherwise             = GT

sortId is = sortBy compareId is 


isnotOver [] = []
isnotOver (gi:gis)  = if null d then (gi:isnotOver gis) else isnotOver e
   where 
     i = getId gi
     d = takeWhile (\gi' -> getId gi' == i) gis  
     e = dropWhile (\gi' -> getId gi' == i) gis

isnotOW g = filter (not.open_world) g

idsOW g    = map getId (filter open_world g) 

isnotOverOW g = isnotOW (isnotOver (sortId g))   
  
isOver [] = []
isOver gs = if length d == 1 then isOver e else d ++ isOver e
   where 
     i = getId (head gs)
     d = takeWhile (\gi' -> getId gi' == i) gs  
     e = dropWhile (\gi' -> getId gi' == i) gs



remNotOverConstraint g icts = map (removeK (removableK (idsOW g) (isnotOverOW icts') [])) icts'
  where
     ra    = filter (\(_:>:(_, Forall (k:=>t))) -> null k) (isnotOverOW g)
     icts' = sortId (map (removeK (map getId ra)) (removeCons icts))
     
removableK iow ds rs = if null nrs then [] else is ++ removableK iow nds' nrs
  where nrs  = filter (\((i:>:(CW, Forall (k :=> _)))) -> (null k || (\(Constraint (i',_,_)) -> i == i') (head k) && not  (elem i iow) )) ds
        is   = (map  getId nrs)
        nds  = filter (\((_:>:(_, Forall (k :=> _)))) -> not $ null k) ds 
        nds' = map (removeK is) nds
   
removeK is (i:>:(b, Forall (k :=> t))) = (i:>:(b, Forall (k' :=> t)))  
  where
    k' = filter (\(Constraint (i,_,_)) -> not (i `elem` is)) k

removeCons = map (\(i:>:(b, Forall (k :=> t))) -> (i:>:(b, Forall ((filter (\(Constraint (i,_,_)) -> not.isUpper.head $ show i) k) :=> t))))     

---------------------------------------------------------------------------------------------------
moveConstraints g = do --g' <- remSelfC g
                       let gno  = isnotOver g 
                       mConstraints gno g
                          



mConstraints gno []                               = return []
mConstraints gno ((ia :>: (o, Forall (ka :=> ta))):gs) = do (_, s, kn)   <- moveK [ia] (ia, ta) gno ka
                                                            let t' = apply s ta
                                                            gs' <- mConstraints gno gs
                                                            return ((ia :>: (o, Forall (kn :=> t'))):gs') 
   
moveK :: [Id] -> (Id, SimpleType) -> [Assump] -> [Pred] -> TI ([Id], Subst, [Pred])
moveK _ _ _ [] = return ([], nullSubst, [])
moveK is (ia, ta) gno (k@(Constraint (i, t, b)):ks) = 
                            do 
                               (is', ss', ks')   <- moveK' is (ia, ta) gno k
                               (is'',ss'', ks'') <- moveK (union is is') (ia, ta) gno (apply ss' (ks))
                               return (is'', ss'' @@ ss', union ks' ks'')
   where 
      find :: Id -> [Assump] -> Maybe (Constrained SimpleType)
      find i [] = Nothing
      find i ((i' :>: (o, Forall kt)):as) = if i == i' then Just kt else find i as 

      moveK' is (ia, ta) gno ke@(Constraint (i, t, b)) =
        do case (find i) gno of
                Nothing -> return (is, nullSubst, [ke]) -- it's overloading 
                Just kt -> do                           -- it's not overloading 
                             if elem i is then return (is, nullSubst, [])
                                else
                                   do (s, k)   <- moveK'' t kt
                                      (_:=>ta') <- freshInst (quantify [] ([] :=> ta))  
                                      let ki = filter ((\(Constraint (i', t', _)) -> ia /= i' || unifyFails (t', ta'))) k
                                      (is', s', ks'') <- moveK (i:is) (ia, ta) gno (apply s ki) -- ??????? Olhar aproveitar os que ja foram movidos  
                                      return (i:is, s @@ s', ks'')
                                          
                                               
moveK'' tc kt = do let ty = quantify [] kt
                   (k :=> t) <- freshInst ty
                   let s = unify (tc, t) 
                   return (s, k)


--------------------------------------------------------------------------------------------------------
-- 
-- k <|> v is the restriction of a set of constraints k to a set of tvars v
--        It keeps only typings which have type variables that are in v.
--
-- k |* v is the closure of restricting a set of constraints k to v. 
--        It keeps typings that are in v or in any typing kept "earlier".
-----------------------------------------------------------------------------

(<|>) :: [Pred] -> [Tyvar] -> [Pred]
[]                   <|> v = []
[Constraint (o,t,b)] <|> v = if null ((tv t) `intersect` v) then [] else [Constraint (o,t,b)]
(k1:ks)              <|> v = ([k1] <|> v) `union` (ks <|> v)

(|*) :: [Pred] -> [Tyvar] -> [Pred]
k |* v 
  | null (v' \\ v) = k'
  | otherwise      = k |* v'
  where (k', v')   = (k <|> v, tv k')
-----------------------------------------------------------------------------
ambiguity k v = if k |* v `equal` k then False else True
   where
     equal k k' = all ((flip elem) k) k' 
 

{-

ambiguity:: ([Pred],[Tyvar]) -> [Pred]
-- ambiguity k vs gives an error if k - k |* vs contains a non removable constraint. 
-- Otherwise returns k |* vs. 

ambiguity (k,vs) = 
  if all isRemovable (k \\ k') then k'
  else error ("ambiguity: constraint " ++
              (show (map (\ (Constraint (o,t,b)) -> (o,t)) (filter (not . isRemovable) (k \\ k')))) ++ 
              " should have been resolved")
                           
  where k' = k |* vs
-}
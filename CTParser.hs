import Parser hiding (char, space, spaces)
import ParseToken   ( semiSep1, commaSep1
                    , whiteSpace, lexeme
                    , symbol, identifier, reserved
                    , natural, charLiteral, stringLiteral
                    , float, isReservedOp,reservedOp 
                    )
import Char
import ParseExpr
import TIMonad   
import Expr 
import Debug
import TIMain
import TIProg
import Assump
import Id
import Pat
import Pred
import Type
import SimpleType
import PreDefs
import Subst
import Lit
import System

exprTable = [[binary "." AssocRight], 
             [negprefix "-"],
             [binary "*" AssocLeft, binary "/" AssocLeft, binary "`div`" AssocLeft, binary "`rem`" AssocLeft, binary "`mod`" AssocLeft, binary "`quot`" AssocLeft],
             [binary "+" AssocLeft, binary "-" AssocLeft],
             [binCon ":" AssocRight, binary "++" AssocRight],
             [binary "==" AssocNone, binary "/=" AssocNone, binary "<=" AssocNone, binary ">=" AssocNone, binary "<" AssocNone, binary ">" AssocNone], 
             [binary "&&" AssocRight],
             [binary "||" AssocRight],
             [binary ">>" AssocLeft, binary ">>=" AssocRight]] 

binary n a = Infix (try (do {symbol n; return binOp})) a
                where binOp x y = Ap (Ap (Var (justId n)) x) y

binCon n a = Infix (try (do {symbol n; return binOp})) a
                where binOp x y = Ap (Ap (Const (toid ":":>:(CW, quantify [Tyvar "v1" []] ([] :=> ((TVar (Tyvar "v1" [])) `fn` TAp tList (TVar (Tyvar "v1" [])) `fn` TAp tList (TVar (Tyvar "v1" [])))))))  x) y

negprefix n =  Prefix (try (do {symbol n; return unOp}))
                where unOp x = Ap (Var (toid "negate")) x

justId (c:cs) = if c /= '`' then toid (c:cs) else toid (rem cs)
	      where rem (c:cs) = if c == '`' then "" else (c:rem cs)

 
type Programa = (String, [String], BindGroup)

-----------------------------------------------------------
-- 
-----------------------------------------------------------
parseML0File :: String -> IO (Either ParseError Programa)
parseML0File fname =
    parseFromFile module' fname

parseML0 sourceName source =
    parse module' sourceName source

       
-- testing
testFile fname
    = do result <- parseML0File (fname)
         case result of
            Left err         -> putStr ("parse error at: " ++ show err)
            Right (bg) -> putStr (show bg)

testFileWrite fname
    = do result <- parseML0File (fname)
         case result of
            Left err         -> putStr ("parse error at: " ++ show err)
            Right (bg) -> writeFile "saida.txt" (show bg)

testFile2 fname
    = do result <- parseML0File (fname)
         case result of
            Left err         -> putStr ("parse error at: " ++ show err)
            Right (bg) -> putStr (show (tiProgram (defs) (trd' bg)))

ct fname
    = do result <- {-# SCC "Sintax" #-} (parseML0File (fname))
         case result of
            Left err         -> putStr ("parse error at: " ++ show err)
            Right (bg) -> putStr (impType (tiProgram (defs) (trd' bg)))

ct' fname
    = do result <- parseML0File (fname)
         case result of
            Left err         -> putStr ("parse error at: " ++ show err)
            Right (bg) -> putStr (impType (tiProgram2 (defs) (trd' bg)))


ct'' fname
     = do result <- parseML0File (fname)
          case result of
             Left err         -> putStr ("parse error at: " ++ show err)
             Right (bg) -> putStr (impType (tiProgram3 (defs) (trd' bg)))


main = do a <- getArgs
          ct (head a)
          --x <- getLine
	  putStr "ok"
                    		
-----------------------------------------------------------
-- GRAMMAR ELEMENTS
-----------------------------------------------------------    
module' = do {reserved "module"; m <- modid; es <- exports; reserved "where"; b <- body; return (m, es, b)}
      <|> do {b <- body; return ("Main", [], b)}

body = try (do {symbol "{"; ids <- impdecls; symbol ";"; ds <- topdecls; symbol "}"; return (concatBg ds)})
       <|> try (do {symbol "{"; ids <- impdecls; symbol "}"; return ([],[])}) 
       <|> do {symbol "{"; ds <- topdecls; symbol "}"; return (concatBg ds)}

impdecls = do {d <- impdecl; ds <- maybeImpdecls; return (d:ds)}

maybeImpdecls = try (do {symbol ";"; ds <-impdecls; return (ds)})
                <|> return []

exports = do {symbol "("; e <-export; es <- maybeExports; symbol ")"; return (e:es)}
          <|> return []

maybeExports = do {symbol ","; e <-export; es <- maybeExports; return (e:es)}
               <|> return []

topdecls = do {d <- topdecl; ds <- maybeTopdecls; return (d++ds)}
 
maybeTopdecls = do {symbol ";"; ds <-topdecls; return ds}
                <|> return []


topdecl =   do {reserved "data"; sp <- simpletype; symbol "="; cons <- constrs; return (concat (map (apCon sp) cons))}
            <|> do {reserved "assume"; vs <- vars; symbol "::"; t <-type'; return (((map (\(i, t) -> (i, Just (quantify (tv t) t), [])) (map (\x -> (toid x, ([] :=> t))) vs)))++[(toid "", Nothing, [])])} 
            <|> do {d <- decl; return d}
            <?> "declaration"

decl = try (do {ed <-over; (i,ps) <- funlhs; r <- rhs; return (ed++[(i, Nothing, [(ps, r)])])})
       <|> do {over; g <- gendecl; return (map (\(i, p) -> (i, Just (quantify (tv [retT p]) p), [])) g)}
       <?> "declaracao"
       -- <|> try (do {p <- pati; r <-rhs; return ([], [([p], r)]})
	 

cdecl = try (do {(i,ps) <- funlhs; r <- rhs; return ([], [(i, [(ps, r)])])})
       <|> try (do {g <- gendecl; return (map (\(i, p) -> (i, quantify (tv [retT p]) p, [])) g, [])})



decls = do {symbol "{"; d <- decl; ds <- maybedecls; symbol "}"; return (concatBg (d++ds))}
        <|> return ([], [])
        
maybedecls = do {symbol ";"; d <-decl; ds <- maybedecls; return (d++ds)}
             <|> return []

over = do {reserved "overload"; return [(toid "", Nothing, [])]}
       <|> return []


-- *** Colocar contexto na declaracao ****
gendecl = do {vs <- vars; symbol "::"; t <-type'; return (map (\x -> (toid x, ([] :=> t))) vs)}
-- <|> do {f <- fixity; dig <- maybedigit; o <- ops; return f}
          <|> return []

maybedigit = do {d <- digit; return d}
              <|> return '0'

ops     = do {o <- op; os <- maybeops; return (o:os)}

maybeops = do {symbol ","; os <- ops; return os}
           <|> return []

vars     = do {v <- var; vs <- maybevars; return (v:vs)}

maybevars = do {symbol ","; vs <- vars; return vs}
            <|> return []

fixity = do {reserved "infixl"; return ""}
         <|> do {reserved "infixr"; return ""}
         <|> do {reserved "infix"; return ""}

types = try (do {symbol "("; t <- type'; ts <- maybetypes; symbol ")"; return (t:ts)})
        <|> do {symbol "("; symbol ")"; return []}

maybetypes = do {symbol ","; t <- type'; ts <- maybetypes; return (t:ts)}
             <|> return []

type' = buildExpressionParser [[Infix (do {symbol "->"; return (\x y -> x `fn` y)}) AssocRight]] btype   

atype = try (do {t <- gtycon; return t})
        <|> try (do {symbol "("; t1 <- type'; symbol ","; t2 <- type'; ts <- maybetypes; symbol ")"; return (apTuplet (t1:(t2:ts)))})
        <|> do {symbol "("; t <- type'; symbol ")"; return t}
        <|> do {symbol "["; t <- type'; symbol "]"; return (TAp tList t)}
        <|> do {t <- tyvar; return (TVar t)}
	
btype = do {t <- atype; ts <- maybeatypes; return (if (ts == []) then t else (foldl1 TAp (t:ts)))}

atypes = do {t <- atype; ts <- maybeatypes; return (t:ts)}
         <|> return []

maybeatypes = try (do {t <- atype; ts <- maybeatypes; return (t:ts)})
              <|> return [] 

tuple = do {symbol "("; t <- type'; symbol ","; ts <- maybetypes; symbol ")"; return t}

gtycon =  try (do {symbol "("; symbol ")"; return tUnit})
          <|> try (do {symbol "("; symbol "->"; symbol ")"; return tArrow})
          <|> try (do {symbol "("; symbol ","; n <- maybecomma; symbol  ")"; return (tTuplen n)})
          <|> do {symbol "["; symbol "]"; return tList}
          <|> do {t <- qtycon; return t}
       
maybecomma = do {symbol ","; n <- maybecomma; return (n+1)}
             <|> return 2 

qtycls = try (do {m <- modid; symbol "."; c <- tycls; return c})
         <|> do {c <- conid; return c}

simpletype = do {i <- conid; t <- tyvars; return (apSimpletype i t)}
             <?> "simletype"

constrs = do {c <- constr; cs <- maybeconstrs; return (c:cs)}
          <?> "end of declaration" 


newconstr = do {c <- con; t <- atype; return (TCon (Tycon (toid c)))}

maybeconstrs = do {symbol "|"; c <- constr; cs <- maybeconstrs; return (c:cs)}
               <|> return []

derive = do {reserved "deriving"; d <- dclass; return d}
         <|> return []

dclass = do {c <- qtycls; return [c]}
          <|> do {symbol "("; c <- qtycls; cs <- maybedclass; symbol ")"; return (c:cs)}

maybedclass = do {symbol ","; c <- qtycls;cs <- maybedclass; return (c:cs)}
               <|> return []   

funlhs :: Parser (Id,[Pat]) 
funlhs = try (do {i <- var; ps <- maybeapats; return (toid i, ps)})    
	 <|> try (do {p1 <- apat; v <- varop; p2 <-apat; return (toid v, p1:[p2])}) -- ??? verificar
         <|> do {symbol "("; (i, ps) <- funlhs; symbol ")"; p <- apat; ps1 <- maybeapats; return (i, ps++[p]++ps1)}           
--       <|> do {i <- var; p <- apat; ps <- maybeapats; return (i, p:ps)}

-- pat = do {v <- var; symbol "+"; i <- integer; return }
--       <|> do {p <- pati; return }

rhs :: Parser Expr
rhs = try (do {symbol "="; e <- exp0; ds <- wdecls; return (apWdecls e ds)})
       <|> do {es <- gdrhs; ds <- wdecls; return (apWdecls (eguarded es) ds)} 
        <?> "rhs" 

gdrhs = do {e0 <- gd; symbol "="; e1 <- exp0; es <- maybegdrhs; return ((e0, e1):es)}

maybegdrhs = try (do {es <- gdrhs; return es})
               <|> return []

gd = do {reservedOp "|"; e <- exp0; return e}   


wdecls = do {reserved "where"; ds <- decls; return (Just ds)}
         <|> return Nothing

exp0 = try (do {e <- expi; ms <- maybeExps; return (apExpr e ms)})
       <|> do {e <- exp10; ms <- maybeExps; return (apExpr e ms)}

maybeExps = try (do {o <- qop; e <- expi; ms <- maybeExps; return ((toid o, e):ms)})
            <|> return []
  
expi = buildExpressionParser exprTable exp10

exp10 =  do {reserved "let"; d <- decls; reserved "in"; e <- expi; return (Let d e)}
         <|> do {symbol "\\"; p <- apat; ps <- maybeapats; symbol "->"; e <- expi; return (Lam ((p:ps), e))}
         <|> do {reserved "if"; e1 <- expi; reserved "then"; e2 <- expi; reserved "else"; e3 <- expi; return (If e1 e2 e3)}
         <|> do {reserved "case"; e <- expi; reserved "of"; symbol "{"; as <- alts; symbol "}"; return (ecase e as)}
         <|> do {reserved "do"; symbol "{"; s <- stmts; symbol "}"; return s}
         <|> fexp
        
fexp = do {e <- aexp; f <- fexp'; if f == [] then (return e) else return (foldl1 Ap (e:f))}

fexp' = try (do {e <- aexp; es <- fexp'; return (e:es)})
        <|> return []
  
aexp =  try (do {symbol "("; e <- exp0; symbol ")"; return e})  
        <|> try (do {symbol "("; e <- exp0; symbol ","; es <- aexps; symbol ")"; return (apTuple (e:es))})
        <|> try (do {symbol "("; o <- qop; symbol ")"; return (Var (toid o))}) -- Verificar ????
        <|> try (do {symbol "["; e <- exp0; es1 <- maybeaexp; reservedOp ".."; es2 <- aexps; symbol "]"; return (apList' (e:es1 ++ es2))})
        <|> try (do {symbol "["; e <- exp0; symbol "]"; return (apList e)})
        <|> try (do {symbol "["; e <- exp0; symbol ","; es <- aexps; symbol "]"; return (apList' (e:es))})
        <|> try (do {symbol "["; e <- exp0; symbol "|"; e' <- qual e; symbol "]"; return (e')})
        <|> try (do {c <- gcon; return (Var (toid c))})
--        <|> do {c <- gcon; return (Const (c :>: (CW, toScheme (TVar (Tyvar "t0" [])))))}
        <|> do {i <- qvar; return (Var (toid i))} 
        <|> do {l <- literal; return (Lit l)}
        <?> "Expression"

aexps = do {e <- exp0; es <- maybeaexp; return (e:es)}
        <|> return []

maybeaexp = do {symbol ","; e <- exp0; es <- maybeaexp; return (e:es)}
            <|> return []

alts = do {as <- sepBy1 alt (symbol ";"); return as}

alt = do {p <- pati; reservedOp "->"; e <- exp0; ds <- wdecls; return (p, apWdecls e ds)}
      
qual e = try (do {p <- apat; reservedOp "<-"; l <- exp0; e' <- maybequal e; return (Let ([], [(toid "ok", [([p], e'), ([PWildcard], Const nilC)])]) (Ap (Ap (Var (toid "_concatMap")) (Var (toid "ok"))) l))})
         <|> try (do {reserved "let"; d <- decls; e' <- maybequal e; return (Let d e')})
         <|> do {b <- exp0; e' <- maybequal e; return (If b e' (Const nilC))}         
         <|> return (apList e)

maybequal e = do {symbol ","; e' <-qual e; return (e')}
              <|> qual e  
    
pati = do {p <- pat10; m <- maybepati; return (apPat2 p m)}
       

maybepati = do {c <- qconop; p <- pati; return (Just (c, p))}
           <|> return Nothing


stmts = do {(p, s) <- stmt; s'<- maybestmts; return (apStmt p s s')}

maybestmts = do {symbol ";"; (p, s) <-stmt; s'<- maybestmts; return (Just (apStmt p s s'))}
             <|> return Nothing

stmt = try (do {p <- pati; reservedOp "<-";  e <- exp0; return (Just p, Right e)})
       <|> try (do {reserved "let"; d <- decls; return (Nothing, Left d)})
       <|> try (do {e <- exp0; return (Nothing, Right e)})
               
-- ??? arrumar precedencia



pat10 =  try (do{c <- gcon; p <- apat; ps <- maybeapats; return (apPat c (p:ps) (p:ps))})
         <|> do {p <- apat; return p}

apat =  try (do {symbol "("; p <- pati; symbol ")"; return p})
        <|> try (do {symbol "("; p1 <- pati; symbol ","; p2 <- pati; ps <- maybepats; symbol ")"; return (apTuplep (p1:(p2:ps)))})
        <|> try (do {i <- var; return (PVar (toid i))})
        <|> try (do {c <- gcon; return (PCon (toid c:>:(CW, quantify [Tyvar "v1" []] ([]:=> (TVar (Tyvar "v1" []))))) [])})
        <|> do {l <- literal; return (PLit l)}        
        <|> do {symbol "_"; return PWildcard}
        <|> do {symbol "["; p1 <- pati; ps <- maybepats; symbol "]"; return (apListp (p1:ps))}
--        <|> do {symbol "~"; a <- apat; return}

maybeapats = do {a <- apat; as <- maybeapats; return (a:as)}
              <|> return []

maybepats = do {symbol ","; p <- pati; ps <- maybepats; return (p:ps)}
             <|> return []

gcon =  try (do {symbol "("; symbol ")"; return "()"})
        <|> try (do {symbol "("; symbol ","; n <- maybecomma; symbol  ")"; return (tuplen n)})
        <|> do {symbol "["; symbol "]"; return "[]"}
        <|> do {t <- qcon; return t}
       
varids = do {v <- varid; vs <- varids; return (v:vs)}
         <|> return []

tyvar = do {i <- varid; return (Tyvar i [])}

tyvars = do {v <- tyvar; vs <- tyvars; return (v:vs)}
         <|> return [] 

tycon = do {i <- conid; return (TCon (Tycon (toid i)))} 

literal = try (do {l <- float; return (LitFloat l)})
           <|> do {l <- natural; return (LitInt l)}  
           <|> do {l <- charLiteral; return (LitChar l)}
           <|> do {l <- stringLiteral; return (LitStr l)}
           
--------------------------------------------------------------------------------------------------
-- Provisorio
-------------------------------------------------------------------------------------------------           
constr = try (do {t1 <- atype; c <- conop; t2 <- atype; return (c, (t1:[t2]), [])})
         <|> try (do {c <- con; symbol "{"; f <- filddecl; fs <- maybefilddecls; symbol "}"; return (c,map snd (f++fs), map fst (f++fs))})
         <|> try (do {c <- con; ts <- atypes; return (c, ts, [])})
         

filddecl = do {v <- vars; symbol "::"; t <-type'; return (zip v (repeat t))}  

maybefilddecls = do {symbol ","; f <- filddecl; fs <- maybefilddecls; return (f++fs)}
                 <|> return []  
   
export = do {whiteSpace; i <- identifier; return i}
modid  = conid
impdecl = do {reserved "import"; m <- modid; return m} <?> "impdecl"
idecl = do {v <- var; return v}
tycls = conid
var = varid <|> do {symbol "("; v <- varsym; symbol ")"; return v} 


----------------- Acrecentar Modid -----------------
qtycon = tycon

qconsym = consym 

qconid = conid 

qvarop = try $ do {symbol "`"; i <- qvarid; symbol "`"; return i} 
        <|> try qvarsym

qconop = try $ do {symbol "`"; c <- qconid; symbol "`"; return c}
         <|> try gconsym

qvarsym = varsym

qvarid = varid
----------------------------------------------------
con = do {symbol "("; c <- consym; symbol ")"; return c}
       <|> conid
 
op = try (varop)
      <|> conop

qop = try (qvarop)
      <|> try (qconop)

varop = try $ do {symbol "`"; i <- varid; symbol "`"; return i} 
        <|> try varsym


conop = try $ do {symbol "`"; c <- qconid; symbol "`"; return c}
         <|> try consym
 
qcon = do {symbol "("; c <- gconsym; symbol ")"; return c}
       <|> qconid

gconsym = try (qconsym)
          <|> do {symbol ":"; return ":"}  

consym = try $ do {symbol ":"; s <- syms; 
                    if (isReservedOp (":"++s))
                      then unexpected ("reserved operator " ++ show (":"++s))
                     else return (":"++s)}
 
varsym = try $ do {s <- sym; ss <- symcolons;
                     if (isReservedOp (s++ss))
                      then unexpected ("reserved operator " ++ show (s++ss))
                     else return (s++ss)} 

syms = do {s <- sym; ss <- maybesyms; return (s++ss)}

maybesyms = do {s <-sym; ss <- maybesyms; return (s++ss)}
            <|> return []

symcolons = do {s <- sym; ss <-symcolons; return (s++ss)}
            <|> do {s <- symbol ":"; ss <-symcolons; return (s++ss)}
            <|> return []

sym = symbol "!" <|> symbol "#" <|> symbol "$" <|> symbol "%" <|> symbol "&" <|> symbol "*" 
      <|> symbol "+" <|> symbol "." <|> symbol "/" <|> symbol "<" <|> symbol "=" <|> symbol ">" 
      <|> symbol "?" <|> symbol "@" <|> symbol "\\" <|> symbol "^" <|> symbol "-" <|> symbol "~" 
      <|> symbol "^" <|> symbol "|"  

qvar = var

varid = try $ do{ whiteSpace; name <- identifier;
          if (isUpper (head name) )
          then unexpected ("Constructor " ++ show (name))
          else return (name)
         }

conid = try $ do{ whiteSpace; name <- identifier;
          if (isLower (head name) )
          then unexpected ("Variable " ++ show (name))
          else return (name)
         }

--------------------------------------------------------------------------------------------------
-- Auxiliar
--------------------------------------------------------------------------------------------------
tupc 2 = tup2C
tupc 3 = tup3C
tupc 4 = tup4C
tupc 5 = tup5C
tupc 6 = tup6C
tupc 7 = tup7C

tuplen 2 = "(,)"
tuplen 3 = "(,,)"
tuplen 4 = "(,,,)"
tuplen 5 = "(,,,,)"
tuplen 6 = "(,,,,,)"
tuplen 7 = "(,,,,,,)"

tTuplen 2 = tTuple2
tTuplen 3 = tTuple3
tTuplen 4 = tTuple4
tTuplen 5 = tTuple5
tTuplen 6 = tTuple6
tTuplen 7 = tTuple7




genField _ _ _ _ [] []            = []    
genField as p len i (t:ts) (v:vs) = (toid v, Nothing, [([genPat as p len], Var (toid "x"))]):genField as (p+1) len i ts vs

genPat as p len = PCon as ((replicate (p-1) PWildcard) ++ [PVar (toid "x")] ++ (replicate (len-p) PWildcard))

apCon t (i,[],_)   = [(toid i, Nothing, [([], Const (toid i :>: (CW, quantify (tv t) ([] :=>  t))))])]
apCon t (i,ts,[])  = [(toid i, Nothing, [([], Const (toid i :>: (CW, quantify (tv (t:ts)) ([] :=> (foldr1 fn (ts++[t]))))))])]
apCon t (i,ts, vs) =  (toid i, Nothing, [([], Const as)]): (genField as 1 (length vs) i ts vs) 
  where as = (toid i :>: (CW, quantify (tv (t:ts)) ([] :=> (foldr1 fn (ts++[t])))))


apSimpletype i [] = TCon (Tycon (toid i))
apSimpletype i t =  foldl1 TAp ((TCon (Tycon (toid i))):t')
                    where t' = map TVar t 
adContext p (p1 :=> t) = (p ++ p1) :=> t

retT (_:=> t) = t

-- Define o tipo de um construtor que aparece em um padrao, uma vez que o tipo sera quantificado usamos
-- um nome qualquer para o tipo.
tcPat n (_:[]) = (TVar (Tyvar ("t"++ (show n)) [])) `fn` (TVar (Tyvar "t0" [])) 
tcPat n (_:xs) = ((TVar (Tyvar ("t"++ (show n))[])) `fn` (tcPat (n+1) xs))

-- ***** CW ou OW ???? *****
apPat i ps = PCon (toid i:>: (CW, quantify (tv t) ([] :=> t)))
          where t = tcPat 1 ps 

apPat2 p1 (Just (c, p2)) = apPat c (p1:[p2]) (p1:[p2])
apPat2 p Nothing = p

apStmt (Just p) (Right s) (Just s') = Ap (Ap (Var $ toid ">>=") s) (Lam ([p], s'))
apStmt Nothing  (Right s) (Just s') = Ap (Ap (Var $ toid ">>") s) s'
apStmt _ (Right s) Nothing          = s     
apStmt _ (Left d) (Just s')         = Let d s'
apStmt _ (Left _) Nothing           = error "Last generator in do {...} must be an expression" 


apWdecls e Nothing = e
apWdecls e (Just ds) = (Let ds e)


apExpr e [] = e
apExpr e ((o,e'):s) = apExpr (Ap (Ap (Var o) e) e') s 

apTuple (es) = foldl1 Ap ((Var (toid (tuplen (length es)))):es) 

apTuplet (ts) = foldl1 TAp ((TCon (Tycon (toid (tuplen (length ts))))):ts) 

apTuplep ps = PCon (tupc (length ps)) ps

apListp [p]      = PCon consC [p, PCon nilC []]
apListp (p:ps)   = PCon consC [p, apListp ps] 

apList e = Ap (Ap (Const consC) e) (Const nilC) 

apList' es = foldr1 (\x y -> Ap (Ap (Const consC) x) y) (es++[Const nilC])   

concatBg [] = ([], [])
concatBg ((i,Nothing, a):ds) = if null (show i) then concatBg ds 
                                 else let (es, is) = concatBg (dropBg i ds)
                                      in (es, (i,a ++ (concatAlt $ takeBg i ds)):is)

concatBg ((i,Just t, a):ds) = let (es, is) = concatBg (dropBg i ds)
                              in ((i,t, a ++ (concatAlt $ takeBg i ds)):es, is)
  


takeBg i ds  = takeWhile (\(i', mt, a') -> i == i' && mt == Nothing) ds
dropBg i ds  = dropWhile (\(i', mt, a') -> i == i' && mt == Nothing) ds
concatAlt ds = concat (map trd' ds)


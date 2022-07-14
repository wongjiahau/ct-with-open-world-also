module TIProg where

import Assump
import TIMonad
import TIMain
import Pred
import Expr
import Sat

import Debug

type Program = BindGroup

tiProgram3 :: TypCtx -> Program -> TypCtx
tiProgram3 g0 bg = runTI $ do {(gow, gi) <- tiBindGroup g0 bg False; return (closeA gi)}

tiProgram2 :: TypCtx -> Program -> TypCtx
tiProgram2 g0 bg = runTI $ do {(gow,gi) <- tiBindGroup g0 bg False; g <- moveConstraints gi; return (closeA g)}

tiProgram :: TypCtx -> Program -> TypCtx
tiProgram g0 bg = runTI $ tiProgram' g0 bg 

tiProgram' g0 bg = do (gow, gi) <- (tiBindGroup g0 bg False)
                      g  <- {-# SCC "MoveCos" #-} (moveConstraints gi)
                      gs <-  satDrive (g0++gow) g
                      let gs' = {-# SCC "Simpl" #-} (simplify gs)
                      return (closeA gs')
  


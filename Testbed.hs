-----------------------------------------------------------------------------
module Testbed(module Id,
               module SimpleType, module Pred, module Type,
               module Subst, module Unify,
               module Assump,
               module Lit, module Pat, module TIMain, module TIProg,
               test, save) where

import Id
import SimpleType
import Pred
import Type
import Subst
import Unify
import Assump
import Lit
import Pat
import TIMain
import TIProg
import Expr
import Monad (filterM)

test       :: [Assump] -> BindGroup -> IO ()
test g bg = putStr $ render $ vcat $ map pprint $ reverse $ 
              tiProgram g bg

save       :: String -> [Assump] -> BindGroup -> IO ()
save f g bg
            = writeFile (f ++ ".hs")
              ("module " ++ f ++ " where\n\n\ 
               \import Testbed\n\n\ 
               \defns" ++ f ++ "\n" ++
               render
                (text " = " <+>
                  brackets (fsep (punctuate comma (map pprint g')))) ++ "\n")
              where g' = reverse $ tiProgram g bg

-----------------------------------------------------------------------------
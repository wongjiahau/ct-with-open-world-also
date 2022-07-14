module TokenDef (tokenDef) where

import StdTokenDef

tokenDef    = haskell
                { reservedOpNames= ["::","=","->","@","\\", "<-", "..", ".", "|"]
                , reservedNames  = ["let","in","case","of","if","then","else",
                                    "data","type", "where", "overload"] }

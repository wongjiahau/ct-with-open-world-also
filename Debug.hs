-----------------------------------------------------------------------------
-- Debug:	Debug functions
-----------------------------------------------------------------------------

module Debug where
import Debug.Trace

-----------------------------------------------------------------------------
-- This module contains definitions that do not appear in the
-- typeset version of the paper.

debug            :: (Show a) => String -> a -> b -> b
debug msg val res = trace (msg ++ " = " ++ show val ++ "\n") res

-----------------------------------------------------------------------------
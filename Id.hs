module Id where

type Idv = String
 
data Id  = Id String Int deriving (Eq, Ord)

instance Show Id where
  show (Id s n) = if n == 0 then s else "let" ++ (show n) ++ "_" ++ s  


--instance PPrint Id where
--  pprint i = text (show i) 

toid s = Id s 0

isGlobal (Id _ n) = n == 0  

isLocal (Id _ n) = n /= 0  
 
enumId  :: Int -> Idv
enumId n = "v" ++ show n 

-----------------------------------------------------------------------------
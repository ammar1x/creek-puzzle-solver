
module Utils
( comb ) where

import Data.List

{-
    The code below is from Stackoverflow.com
    Unfortunately, I didn't save the link.
-}

comb 0 lst = [[]]
comb n lst = do
    (x:xs) <- tails lst
    rest <- comb (n-1) xs
    return $ x : rest

module Main where

import Logger

main :: IO ()
main = withLog $ \h -> do
    dl <- doDebugLog h "kek-cheburek"
    de <- doErrorLog h "ERROR, ALERT"
    
    pure ()
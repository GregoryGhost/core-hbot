module Main where

import Logger

main :: IO ()
main = do
    h <- genLogFile
    dl <- doDebugLog h "kek-cheburek"
    de <- doErrorLog h "ERROR, ALERT"

    closeLog h

    pure dl
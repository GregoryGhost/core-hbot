module Main where

import Logger

import Control.Concurrent

main :: IO ()
main = withLog $ \h -> do
    dl <- doDebugLog h "kek-cheburek"
    de <- doErrorLog h "ERROR, ALERT"
    threadDelay 10000000

    pure ()
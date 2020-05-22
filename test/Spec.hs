{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.QuickCheck.Instances
import HBot.Core.BasicCmd
import HBot.Core.Cmd
import Data.Void

prop_sumInvariant a b = sum_ab == sum_ba
 where 
    sum_ba = sum2 b a
    sum_ab = sum2 a b   

sum2 :: Double -> Double -> Double
sum2 a b = a + b

main :: IO ()
main = do
    -- quickCheckWith stdArgs { maxSuccess = 1000} prop_sumInvariant
    -- putStrLn "done!"
    --TODO: write some test cases
    -- let botCmd = BotCmd { cmd = Help, args = [] :: [String] }
    -- test <- eval botCmd :: IO BasicCmd
    -- ePrint test
    
    -- let kekCmd = BotCmd { cmd = Kek, args = [] :: [String] }
    -- kek <- eval kekCmd :: IO BasicCmd
    -- ePrint kek
    print . getHelp . cmd $ BotCmd { cmd = Help, args = [] :: [String] }

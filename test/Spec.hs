{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.QuickCheck.Instances
import HBot.Core.BasicCmd
import HBot.Core.Cmd

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
    putStrLn =<< interpret botCmd Test where
        botCmd = BotCmd { cmd = Help, args = [] }

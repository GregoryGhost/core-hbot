module HBot.Core.Cmd where

data Cmd = Test | Help | Echo | LastCmd deriving Show
instance Read Cmd where
    readsPrec _ input = toReadS parsed where 
        toReadS x = case x of (Just x) -> [(x, "")]; _ -> [];
        parsed :: Maybe Cmd
        parsed = case input of
            "/test" -> Just Test
            "/help" -> Just Help
            "/echo" -> Just Echo
            "/lst_cmd" -> Just LastCmd
            _ -> Nothing

    
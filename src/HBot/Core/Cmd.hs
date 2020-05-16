{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module HBot.Core.Cmd where
import Text.Read
import Control.Monad.IO.Class

data BasicCmd = Test | Help | Echo | LastCmd deriving Show

--TODO: rewrite on BotCmdParser
instance Read BasicCmd where
    readsPrec _ input = toReadS parsed where 
        toReadS x = case x of (Just x) -> [(x, "")]; _ -> [];
        parsed :: Maybe BasicCmd
        parsed = case input of
            "/test" -> Just Test
            "/help" -> Just Help
            "/echo" -> Just Echo
            "/lst_cmd" -> Just LastCmd
            _ -> Nothing

--TODO: rewrite
--NOTE: draft impl for prototype
data ResponseError = TestError | TestError2
data ParseError = PError | PError2
data Response = String
type ParsedCmd c = Either ParseError с
type BotResponse = Either ResponseError Response
type ParsedPayload = Either ParseError Cmd
type SourceCmd = String
type SourcePayload = String
data Cmd = Cmd
    {
        getCmd :: SourceCmd
        , getArgs :: CmdArgs
    }
type CmdArg = String
type CmdArgs = [CmdArg]

data BotCmd c where
    BotCmd :: { cmd :: c, args :: CmdArgs} -> BotCmd c

class BotCmdParser c where
    parsePayload :: SourcePayload -> ParsedPayload
    getBotCmd :: SourceCmd -> ParsedCmd c
    
    parse :: SourcePayload -> ParsedCmd c
    parse source = do
        parsedPayload <- parsePayload source
        botCmd <- getBotCmd $ getCmd parsedPayload
        let cmdArgs = getArgs parsedPayload
        let parsedCmd = BotCmd { cmd = botCmd, args = cmdArgs }

        pure parsedCmd

class (BotCmdParser cmd) => BotInterpreter cmd where
    run :: MonadIO m => SourcePayload -> m BotResponse
    run source = do
        botCmd <- parse source
        result <- liftIO $ interpret botCmd

        pure result

    interpret :: BotCmd cmd -> IO BotResponse
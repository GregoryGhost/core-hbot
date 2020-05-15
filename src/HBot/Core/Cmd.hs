module HBot.Core.Cmd where

data BasicCmd = Test | Help | Echo | LastCmd deriving Show
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

type BotResponse = Either<ResponseError, Response>
type ParsedCmd = Either<ParseError, BotCmd>
type CmdArg = String
type CmdArgs = [CmdArg]
type SourceCmd = String
type SourcePayload = String
data Cmd = Cmd
    {
        getCmd :: SourceCmd
        , getArgs :: CmdArgs
    }

type ParsedPayload = Either<ParseErrorPayload, Cmd>

data BotCmd = BotCmd 
    {
        cmd :: InterpredCmd c => c
        , args :: CmdArgs
    }

class BotCmdParser cmd args where
    parseCmd :: SourcePayload -> ParsedCmd
    parseCmd source = do
        parsedPayload <- parsePayload source
        cmd <- readEither $ getCmd parsedPayload
        let cmdArgs = getArgs parsedPayload
        let parsedCmd = BotCmd { cmd = cmd, args = cmdArgs }

        pure parsedCmd

    parsePayload :: SourcePayload -> ParsedPayload

class (BotCmd cmd, Messanger client, BotCmdParser parser) 
    => BotInterpreter cmd client parser where
    
    run :: SourcePayload -> IO BotResponse
    run source = do
        botCmd <- parseCmd source
        result <- process botCmd

        pure result 

    process :: BotCmd -> IO BotResponse

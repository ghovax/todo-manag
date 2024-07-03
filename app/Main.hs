{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (IOException, catch)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Csv (FromRecord, ToRecord, toField, toRecord, (.!))
import qualified Data.Csv as Csv
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import qualified Data.Vector as V
import Options.Applicative
import Prettyprinter
import Prettyprinter.Render.Terminal
import System.Console.Terminfo
import System.Directory (doesFileExist, getHomeDirectory)
import System.Directory.Internal.Prelude (Handle, exitFailure, hClose)
import System.Exit (exitSuccess)
import System.IO (IOMode (..), SeekMode (..), hGetChar, hIsEOF, hPutStr, hSeek, hSetFileSize, openFile, stdout)

-- | Structured state for the TODO reminders with a message and a due date.
data TodoRem = TodoRem
  { msg :: Text,
    dueDate :: UTCTime
  }
  deriving (Show)

-- Define how to parse a CSV record into a TodoRem
instance FromRecord TodoRem where
  parseRecord :: Csv.Record -> Csv.Parser TodoRem
  parseRecord v
    | length v == 2 = TodoRem <$> v .! 0 <*> (v .! 1 >>= parseDateTime')
    | otherwise = fail "Expected two columns"
    where
      parseDateTime' :: String -> Csv.Parser UTCTime
      parseDateTime' = parseTimeM True defaultTimeLocale "%F %T %Z"

instance ToRecord TodoRem where
  toRecord (TodoRem m d) = V.fromList [toField m, toField (show d)]

-- CLI arguments parsing routines

data Cmd
  = AddRem TodoRem
  | RemoveRem Text
  | ListRems
  deriving (Show)

parseDateTime :: ReadM UTCTime
parseDateTime = do
  input <- str
  case parseTimeM True defaultTimeLocale "%F %T %Z" input of
    Just dateTime -> return dateTime
    Nothing -> readerError $ "Cannot parse date/time: " ++ input

addCmdParser :: Parser Cmd
addCmdParser =
  AddRem
    <$> ( TodoRem
            <$> strOption
              ( long "message"
                  <> short 'm'
                  <> metavar "MESSAGE"
                  <> help "Message to be reminded about"
              )
            <*> option
              parseDateTime
              ( long "datetime"
                  <> short 'd'
                  <> metavar "DATETIME"
                  <> help "Date and time to be reminded (YYYY-MM-DD HH:MM:SS format)"
              )
        )

removeCmdParser :: Parser Cmd
removeCmdParser =
  RemoveRem
    <$> argument
      str
      ( metavar "MESSAGE"
          <> help "Message to be removed"
      )

listCmdParser :: Parser Cmd
listCmdParser = pure ListRems

cmdParser :: Parser Cmd
cmdParser =
  subparser
    ( command "add" (info addCmdParser (progDesc "Add a new reminder"))
        <> command "remove" (info removeCmdParser (progDesc "Remove an existing reminder"))
        <> command "list" (info listCmdParser (progDesc "List all the existing reminders"))
    )

parseCmd :: ParserInfo Cmd
parseCmd =
  info
    (cmdParser <**> helper)
    ( fullDesc
        <> progDesc "Remind yourself of events by using this tool"
        <> header "todo-manag - Simple management of reminders"
    )

-- File-handling routines for creating/loading the database
-- and for reading+parsing/writing to the database itself

-- | Get the home directory path and append the filename.
getHomeFilePath :: String -> IO FilePath
getHomeFilePath fileName = do
  homeDir <- getHomeDirectory
  return (homeDir ++ "/" ++ fileName)

-- | Function to find and remove a TodoRem item by message.
removeTodoRemByMsg :: Text -> V.Vector TodoRem -> Maybe (V.Vector TodoRem)
removeTodoRemByMsg targetMsg vec =
  case V.findIndex (\todo -> msg todo == targetMsg) vec of
    Just idx ->
      let (left, right) = V.splitAt idx vec
       in Just (V.concat [left, V.tail right])
    Nothing -> Nothing

hGetContents' :: Handle -> IO String
hGetContents' h = do
  eof <- hIsEOF h
  if eof
    then
      return []
    else do
      c <- hGetChar h
      (c :) <$> hGetContents' h

stringToBS :: String -> BL.ByteString
stringToBS = BL.pack . map (toEnum . fromEnum)

readCSVFile :: Handle -> IO (Either String (V.Vector TodoRem))
readCSVFile handle = do
  csvData <- hGetContents' handle
  hSeek handle AbsoluteSeek 0
  return $ Csv.decode Csv.NoHeader (stringToBS csvData)

-- Function to write a vector of TodoRem to a CSV file
writeCSVFile :: Handle -> V.Vector TodoRem -> IO ()
writeCSVFile handle records = do
  hSetFileSize handle 0 -- Truncate the file to zero length
  let csvData = Csv.encode (V.toList records)
  System.IO.hPutStr handle (unpack csvData)

-- Routines for starting the program and doing IO

todoRemToDoc :: Int -> Int -> TodoRem -> Doc AnsiStyle
todoRemToDoc maxDateW msgLen (TodoRem remMsg date) =
  fillBreak msgLen (pretty remMsg)
    <> " "
    <> fillBreak maxDateW (pretty $ show date)

ppTodoRems :: Int -> [TodoRem] -> IO ()
ppTodoRems termW rems = do
  -- Get the maximum length of all due dates
  let maxDateW = maximum $ map (\todoRem -> T.length $ T.pack . show . dueDate $ todoRem) rems
      msgLen = termW - maxDateW - 1
      headr =
        fillBreak msgLen (pretty $ T.pack "Message")
          <> " "
          <> fillBreak maxDateW (pretty $ T.pack "Due date")
      headSep = pretty $ T.replicate termW "-"
      rows = map (todoRemToDoc maxDateW msgLen) rems
      table = vsep (headr : headSep : rows)
  renderIO stdout (layoutPretty defaultLayoutOptions table)

main :: IO ()
main =
  do
    cmd <- execParser parseCmd -- Read the command line arguments and get the structured output

    -- Get the path of the database file in the home folder
    fullPath <- getHomeFilePath ".todo-manag-rems"
    -- Retrieve an handle to the database file (after creating it if it doesn't exist)
    fileExists <- doesFileExist fullPath
    unless fileExists $ do
      writeFile fullPath "" -- Create an empty file
      putStrLn $ "Database file created from scratch at the path: " ++ show fullPath

    handle <- openFile fullPath ReadWriteMode
    remsEither <- readCSVFile handle
    rems <- case remsEither of
      Left err -> do
        putStrLn $ "Failed to parse CSV: " ++ err
        exitFailure
      Right ts -> return ts

    let handleIOExc :: IOException -> IO ()
        handleIOExc e = do
          putStrLn $ "File operation failed: " ++ show e
          exitFailure

    case cmd of
      -- Add a reminder to the already present reminders
      AddRem newRem -> do
        -- Write updated contents back to CSV file
        let newRems = V.snoc rems newRem
        writeCSVFile handle newRems `catch` handleIOExc
      -- Remove a reminder from its message
      RemoveRem msgToRemove -> do
        let maybeNewRems = removeTodoRemByMsg msgToRemove rems
        case maybeNewRems of
          Just newRems -> writeCSVFile handle newRems `catch` handleIOExc
          Nothing -> do
            putStrLn $ "Unable to find the message: " ++ show msgToRemove
            exitFailure
      -- List all the present reminders
      ListRems -> do
        -- Initialize the terminal
        term <- setupTermFromEnv
        -- Get the width of the terminal
        let maybeWidth = getCapability term termColumns
        let termW = case maybeWidth of
              Nothing -> 80 -- Default to 100 characters in one line
              Just w -> w
        ppTodoRems termW (V.toList rems)

    hClose handle
    exitSuccess
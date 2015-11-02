module Zork
  ( ZorkProcess
  , startZork
  , inputCommand
  , readFrom
  , readFrom'
  , terminate
  , saveState
  , restoreState
  ) where

import BasePrelude
import Data.List.Utils
import System.IO
import System.Process

data ZorkProcess = ZorkProcess
  { zorkUserId :: String
  , stdinHandle :: Handle
  , stdoutHandle :: Handle
  , processHandle :: ProcessHandle
  }

startZork :: FilePath -> String -> IO (ZorkProcess, String)
startZork zorkDat userId = do
  let frotz = proc "dfrotz" ["-p", "-w", "-1", zorkDat]
  env <- insert ("USER_ID", userId) <$> getEnvironment
  (Just stdin', Just stdout', _, process) <- createProcess frotz
    { env = Just env
    , std_in = CreatePipe
    , std_out = CreatePipe
    , close_fds = True
    }
  let p = ZorkProcess userId stdin' stdout' process
  output <- liftM2 fromMaybe (readFrom p) (restoreState p)
  return (p, output)

inputCommand :: ZorkProcess -> String -> IO String
inputCommand p@ZorkProcess{..} s = do
  hPutStrLn stdinHandle s
  hFlush stdinHandle
  readFrom' p

readFrom :: ZorkProcess -> IO String
readFrom p@ZorkProcess{..} = do
  hasInput <- hWaitForInput stdoutHandle readTimeout
  if hasInput
    then liftM2 (:) (hGetChar stdoutHandle) (readFrom p)
    else return ""
  where
    readTimeout = 10

readFrom' :: ZorkProcess -> IO String
readFrom' p = do
  output <- readFrom p
  case output of
    "" -> readFrom' p
    _ -> return output

terminate :: ZorkProcess -> IO ()
terminate p@ZorkProcess{..} = do
  saveState p
  terminateProcess processHandle

saveStateFile :: FilePath
saveStateFile = ".savedstate"

saveState :: ZorkProcess -> IO ()
saveState p = do
  _ <- inputCommand p ""
  _ <- inputCommand p "save"
  output <- inputCommand p saveStateFile
  output' <-
    if "Overwrite existing file?" `isPrefixOf` output
      then inputCommand p "y"
      else return output
  if not ("Ok." `isPrefixOf` output')
    then error ("Failed: " <> show output')
    else return ()

restoreState :: ZorkProcess -> IO (Maybe String)
restoreState p = do
  _ <- inputCommand p "restore"
  output <- inputCommand p saveStateFile
  if "Ok." `isInfixOf` output
    then return (Just (replace "Ok.\n\n" "" output))
    else return Nothing

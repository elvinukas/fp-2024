{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements
    ) where

import Control.Concurrent ( Chan , readChan, writeChan )
import Control.Concurrent.STM(STM, TVar)
import qualified Lib2
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Lib2 (Query(Add))

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = loop
  where
    loop = do
      op <- readChan chan -- reading the operation
      case op of
        Save info saveChannel -> do -- saving the info about status into a file
          writeFile "info.txt" info
          writeChan saveChannel ()
        Load loadChannel -> do
          info <- readFile "info.txt"
          writeChan loadChannel info
      loop


data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)


-- instance Show Statements where
--   show (Batch queries) =
--     "Queries:\n" ++ unlines (map show queries)

--   show (Single query) =
--     "Single query:\n" ++ show query

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)


-- | Helper functions
trim :: String -> String
trim str =
  let
    removeFrontSpaces = dropWhile isSpace
    removeEndSpaces = reverse . dropWhile isSpace . reverse -- reverse reverses the text,
                                                            -- then the space is removed at the end (now the front)
                                                            -- and then reversed again to retrieve the og message
  in removeEndSpaces (removeFrontSpaces str)


-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand input
  | trim input == "LOAD" = Right (LoadCommand, "")
  | trim input == "SAVE" = Right (SaveCommand, "")
  | otherwise = case parseStatements (trim input) of
        Left err -> Left err
        Right (statements, rest) -> Right (StatementCommand statements, rest)


-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements input =
  if "BEGIN\n" `isPrefixOf` input
    then case parseBatch (drop 6 input) of
      Right (queries, rest) -> Right (Batch queries, rest)
      Left err -> Left err
  else case Lib2.parseQuery input of
    Right query ->
      let rest = drop (length (show query)) input
      in Right (Single query, rest)
    Left err -> Left err



-- | parses a batch of queries if there are any

parseBatch :: String -> Either String ([Lib2.Query], String)
parseBatch input =
  let
    (queries, rest) = break null (lines input) -- removing empty lines too
    parsedQ = mapM Lib2.parseQuery queries
  in
    case parsedQ of
      Right query -> Right (query, unlines rest)
      Left err -> Left err



-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)

-- here optimizations for the states also take place
marshallState :: Lib2.State -> Statements
marshallState state =
  let
    



-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements _ = error "Not implemented 5"

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp ->
                   IO (Either String (Maybe String))
stateTransition _ _ ioChan = return $ Left "Not implemented 6"

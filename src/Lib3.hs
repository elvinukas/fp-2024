{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
module Lib3
  ( stateTransition,
  StorageOp (..),
  storageOpLoop,
  parseCommand,
  parseStatements,
  marshallState,
  renderStatements,
  renderQuery,
  Statements(..),
  Command(..)
  ) where

import Control.Concurrent ( Chan , readChan, writeChan, newChan )
import Control.Concurrent.STM(STM, TVar, atomically, readTVarIO, readTVar, writeTVar)
import qualified Lib2
import Data.Char (isSpace)
import Data.List (isPrefixOf, notElem, (\\), intercalate, find, delete, partition)
import Lib2 (Query(Add), formatHotel, formatReservation)
import Debug.Trace

instance Show Lib2.Query where
  show = renderQuery 

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
    loop = do -- do-notation (pavercia >>= operatoriu (bind) paprasciau uzrasoma forma)
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
               deriving (Eq)

instance Show Statements where
  show (Single query) = "BEGIN\n" ++ renderQuery query ++ "END"  
  show (Batch queries) =
    "BEGIN\n" ++ unlines (map renderQuery queries) ++ "END" 
  
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
parseCommand :: String -> Either String (Command, String) -- monad'as either
parseCommand input
  | trim input == "LOAD" = Right (LoadCommand, "")
  | trim input == "SAVE" = Right (SaveCommand, "")
  | trim input == "LIST" = Right (StatementCommand (Batch [Lib2.ListState]), "")
  | otherwise = case parseStatements (trim input) of
        Left err -> Left err
        Right (statements, rest) -> Right (StatementCommand statements, rest)


-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements input =
  let trimmedInput = trim input in
  --trace ("Parsing input: " ++ show trimmedInput) $ 
  if "BEGIN" `isPrefixOf` trimmedInput
    then case parseBatch (drop (length "BEGIN") trimmedInput) of
      Right (queries, rest) -> Right (Batch queries, rest)
      Left err -> Left err
  else case Lib2.parseQuery trimmedInput of
    Right query -> Right (Single query, drop (length (show query)) trimmedInput)
    Left err -> Left err


-- | parses a batch of queries if there are any

parseBatch :: String -> Either String ([Lib2.Query], String)
parseBatch input = do
  let lines' = lines input  -- split into lines
  let (queryLines, rest) = break (== "END") lines'  -- split at END

      -- removing empty lines, begin and end markers
  let cleanedQueries = filter (\line -> not (null (trim line))
                                   && line /= "BEGIN"
                                   && line /= "END") queryLines

  --trace ("Cleaned queries: " ++ show cleanedQueries) $
  -- parsing each line
  case mapM Lib2.parseQuery cleanedQueries of -- monad mapM, if all pass, then okay, if not err, monadic
      Right queries -> Right (queries, unlines (drop 1 rest))  -- drop 1 to skip the END line
      Left err -> Left err



-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)

-- as far as I understand it, we take some type of state and we try to get the exact state using the least amount of queries possible
-- the signature could be changed to support transitions from some state to another (instead of empty state to final state)
marshallState :: Lib2.State -> Statements
marshallState finalState =
  let
    emptyState = Lib2.emptyState

    findAdded :: [Lib2.AvailableHotelEntity] -> [Lib2.AvailableHotelEntity] -> [Lib2.AvailableHotelEntity]
    findAdded initial final = final \\ initial -- in final state but not in initial

    -- non-empty state might be added
    findRemoved :: [Lib2.AvailableHotelEntity] -> [Lib2.AvailableHotelEntity] -> [Lib2.AvailableHotelEntity]
    findRemoved initial final = initial \\ final -- in initial state but not in final

    addHotelsQueries :: [Lib2.AvailableHotelEntity] -> [Lib2.Query]
    addHotelsQueries = map (\hotel -> Add (Lib2.availableEntityId hotel) (Lib2.availableHotel hotel))  --map pritaiko funkciją kiekvienam sąrašo elementui, monadiska

    removeHotelQueries :: [Lib2.AvailableHotelEntity] -> [Lib2.Query]
    removeHotelQueries = map (\hotel -> Lib2.Remove (Lib2.availableEntityId hotel))

    -- don't care about the other entries
    makeReservationQueries :: [Lib2.Reservation] -> [Lib2.Query]
    makeReservationQueries = map (\res -> Lib2.MakeReservation (head (Lib2.guests res)) (Lib2.reservationID res) (Lib2.checkIn res) (Lib2.checkOut res) (Lib2.price res))

    -- | finding the actual queries

    addedHotels = findAdded (Lib2.availableHotelEntities emptyState) (Lib2.availableHotelEntities finalState)
    removedHotels = findRemoved (Lib2.availableHotelEntities emptyState) (Lib2.availableHotelEntities finalState)

    addQueries = addHotelsQueries addedHotels
    removeQueries = removeHotelQueries removedHotels
    reservationQueries = makeReservationQueries (Lib2.reservations finalState)

    combinedQueries = addQueries ++ removeQueries ++ reservationQueries

    isCancelled :: Lib2.Query -> Lib2.Query -> Bool
    isCancelled (Lib2.MakeReservation _ id1 _ _ _) (Lib2.CancelReservation id2) = id1 == id2
    isCancelled (Lib2.CancelReservation id1) (Lib2.MakeReservation _ id2 _ _ _) = id1 == id2
    isCancelled _ _ = False

    filterCancelled :: [Lib2.Query] -> [Lib2.Query]
    filterCancelled [] = []
    filterCancelled (q:qs) =
      case find (isCancelled q) qs of 
        Just matchingQuery -> filterCancelled (delete matchingQuery qs)
        Nothing -> q : filterCancelled qs
      
    finalQueries = filterCancelled combinedQueries

  in
    case finalQueries of
      [query] -> Single query
      queries -> Batch queries

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single query) =
  "BEGIN\n" ++ renderQuery query ++ "\nEND"
renderStatements (Batch queries) =
  "BEGIN\n" ++ unlines (map renderQuery queries) ++ "END"


renderQuery :: Lib2.Query -> String
renderQuery (Add id hotel) =
  "ADD. " ++ show id ++ ". " ++ renderHotel hotel
renderQuery (Lib2.Remove id) =
  "REMOVE. " ++ show id ++ ". "
renderQuery (Lib2.MakeReservation guest id checkIn checkOut price) =
  "MAKE RESERVATION. " ++ renderGuest guest ++ show id ++ ". " ++
  renderCheckIn checkIn ++ renderCheckout checkOut ++ renderPrice price
renderQuery (Lib2.CancelReservation id) =
  "CANCEL RESERVATION. " ++ show id ++ ". "
renderQuery (Lib2.AddAdditionalGuest guest id) =
  "ADD ADDITIONAL GUEST. " ++ renderGuest guest ++ show id ++ ". "
renderQuery (Lib2.ListState) =
  "LIST"

renderHotel :: Lib2.Hotel -> String
renderHotel (Lib2.Hotel {Lib2.hotelName, Lib2.hotelChain, Lib2.floors}) =
  "HOTEL: " ++ hotelName ++ ". " ++
  renderChain hotelChain ++
  renderFloors floors

renderChain :: [Lib2.Hotel] -> String
renderChain [] = ""
renderChain (hotel:hotels) =
  "CHAIN OF. " ++ renderHotel hotel ++ renderChain hotels

renderFloors :: [Lib2.Floor] -> String
renderFloors [] = ""
renderFloors floors =
  concatMap renderFloor floors

renderFloor (Lib2.Floor { Lib2.floorNumber, Lib2.rooms }) =
  "FLOOR: " ++ show floorNumber ++ ". " ++ renderRooms rooms

renderRooms :: [Lib2.Room] -> String
renderRooms [] = ""
renderRooms (room:rooms) =
  renderRoom room ++ renderRooms rooms

renderRoom :: Lib2.Room -> String
renderRoom room =
  "ROOM: " ++ show (Lib2.roomNumber room) ++ ". " ++
  renderAmenities (Lib2.amenities room) ++
  renderRoomSections (Lib2.roomSections room)
 

renderRoomSections :: [Lib2.Room] -> String
renderRoomSections [] = ""
renderRoomSections (section:sections) =
  "ROOM SECTION. " ++ renderRoom section ++
  renderRoomSections sections

renderAmenities :: [Lib2.Amenity] -> String
renderAmenities [] = ""
renderAmenities amenities = "AMENITIES: " ++ intercalate ", " (map show amenities) ++ ". " -- functor map 

renderGuest :: Lib2.Guest -> String
renderGuest (Lib2.Guest{Lib2.guestName, Lib2.guestSurname}) =
  "GUEST: " ++ guestName ++ " " ++ guestSurname ++ ". "

renderCheckIn :: Lib2.CheckIn -> String
renderCheckIn checkIn =
  "CHECK IN: " ++ show (Lib2.checkInDate checkIn) ++ " " ++ show (Lib2.checkOuttime checkIn) ++ ". "

renderCheckout :: Lib2.CheckOut -> String
renderCheckout checkOut =
  "CHECK OUT: " ++ show (Lib2.checkOutDate checkOut) ++ " " ++ show (Lib2.checkOutTime checkOut) ++ ". "

renderPrice :: Lib2.Price -> String
renderPrice (Lib2.Price price) =
  "PRICE: " ++ show price ++ ". "


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
            IO (Either String (Maybe String, String))
stateTransition stateVar command ioChan = case command of
  LoadCommand -> do
    loadChan <- newChan
    writeChan ioChan (Load loadChan)
    info <- readChan loadChan
    case parseStatements info of
      Right (statements, _) -> atomically $ do -- atomically monad'as
        initialState <- readTVar stateVar
        case statements of
          Batch queries -> do
            let applyQueries = foldl (\stateRes query -> case stateRes of -- accumuliuojamos reiksmes, vis pridedamos viena prie kitos
                  Right state -> case Lib2.stateTransition state query of
                    Right (_, newState) -> Right newState
                    Left err -> Left err
                  Left err -> Left err) (Right initialState) queries
            case applyQueries of
              Right newState -> do
                writeTVar stateVar newState
                return $ Right (Just "Loading was successful.", show newState)
              Left err -> return $ Left ("Error applying queries: " ++ err)
          Single query -> do
            case Lib2.stateTransition initialState query of
              Right (_, newState) -> do
                writeTVar stateVar newState
                return $ Right (Just "Loading was successful.", show query)
              Left err -> return $ Left ("Error applying query: " ++ err)
      Left err -> return $ Left ("Error parsing statements: " ++ err)


  SaveCommand -> do
    currentState <- readTVarIO stateVar
    let state = renderStatements (marshallState currentState)
    saveChan <- newChan
    writeChan ioChan (Save state saveChan)
    _ <- readChan saveChan
    return $ Right (Just "State saving was successful.", show currentState)

  StatementCommand statements -> atomically $ do
      currentState <- readTVar stateVar
      case statements of
        Batch queries -> do
          -- querying by priority
          let (addQueries, rest) = partition isAddQuery queries
              (makeReservationQueries, rest') = partition isMakeReservationQuery rest
              (removeQueries, rest'') = partition isRemoveQuery rest'
              (cancelReservationQueries, addAdditionalGuestQueries) = partition isCancelReservationQuery rest''
              orderedQueries = addQueries ++ makeReservationQueries ++ removeQueries ++ cancelReservationQueries ++ addAdditionalGuestQueries

          -- applying the queries based on priority
          let applyQueries = foldl (\stateRes query -> case stateRes of
                Right state -> case Lib2.stateTransition state query of
                  Right (_, newState) -> Right newState
                  Left err -> Left err
                Left err -> Left err) (Right currentState) orderedQueries

          case applyQueries of
            Right newState -> do
              writeTVar stateVar newState
              return $ if any isListQuery queries
                then Right (Just "Statements executed successfully.", show currentState)
                else Right (Just "Statements executed successfully.", "Statements executed successfully.")
            Left err -> return $ Left ("Error executing statements: " ++ err)
        Single query -> do
          case Lib2.stateTransition currentState query of
            Right (_, newState) -> do
              writeTVar stateVar newState
              return $ Right (Just "Statement executed successfully.", "Statement executed successfully.")
            Left err -> return $ Left ("Error executing statement: " ++ err)
    where
      -- functions to check query type
      isAddQuery (Lib2.Add _ _) = True
      isAddQuery _ = False

      isMakeReservationQuery (Lib2.MakeReservation _ _ _ _ _) = True
      isMakeReservationQuery _ = False

      isRemoveQuery (Lib2.Remove _) = True
      isRemoveQuery _ = False

      isCancelReservationQuery (Lib2.CancelReservation _) = True
      isCancelReservationQuery _ = False

      isAddAdditionalGuestQuery (Lib2.AddAdditionalGuest _ _) = True
      isAddAdditionalGuestQuery _ = False

      isListQuery Lib2.ListState = True
      isListQuery _ = False


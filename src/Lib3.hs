{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Data.List (isPrefixOf, notElem, (\\), intercalate)
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
    addHotelsQueries = map (\hotel -> Add (Lib2.availableHotel hotel))

    removeHotelQueries :: [Lib2.AvailableHotelEntity] -> [Lib2.Query]
    removeHotelQueries = map (\hotel -> Lib2.Remove (Lib2.availableEntityId hotel)) 

    -- | finding the actual queries

    addedHotels = findAdded (Lib2.availableHotelEntities emptyState) (Lib2.availableHotelEntities finalState)
    removedHotels = findRemoved (Lib2.availableHotelEntities emptyState) (Lib2.availableHotelEntities finalState)

    addQueries = addHotelsQueries addedHotels
    removeQueries = removeHotelQueries removedHotels

    finalQueries = addQueries ++ removeQueries

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
  "START\n" ++ renderQuery query ++ ".\nEND"
renderStatements (Batch queries) =
  "START\n" ++ unlines (map (\q -> renderQuery q ++ ".") queries) ++ "END" 


renderQuery :: Lib2.Query -> String
renderQuery (Add hotel) =
  "ADD. " ++ renderHotel hotel
renderQuery (Lib2.Remove id) =
  "REMOVE. " ++ show id
renderQuery (Lib2.MakeReservation guest hotel checkIn checkOut price) =
  "MAKE RESERVATION. " ++ renderGuest guest ++ renderHotel hotel ++
  renderCheckIn checkIn ++ renderCheckout checkOut ++ renderPrice price
renderQuery (Lib2.CancelReservation id) =
  "CANCEL RESERVATION. " ++ show id
renderQuery (Lib2.AddAdditionalGuest guest id) =
  "ADD ADDITIONAL GUEST. " ++ renderGuest guest ++ show id
renderQuery (Lib2.ListState) =
  "List of the current state..."


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
  renderRoomSections (Lib2.roomSections room) ++
  renderAmenities (Lib2.amenities room)

renderRoomSections :: [Lib2.Room] -> String
renderRoomSections [] = ""
renderRoomSections (section:sections) =
  "ROOM SECTION. " ++ renderRoom section ++
  renderRoomSections sections

renderAmenities :: [Lib2.Amenity] -> String
renderAmenities [] = ""
renderAmenities amenities = "AMENITIES: " ++ intercalate ", " (map show amenities)

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
                   IO (Either String (Maybe String))
stateTransition _ _ ioChan = return $ Left "Not implemented 6"

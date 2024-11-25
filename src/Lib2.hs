{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BlockArguments #-}
module Lib2
    ( 
    Query(..),
    State(..),
    ID(..),
    Guest(..),
    Hotel(..),
    Floor(..),
    Room(..),
    Amenity(..),
    Date(..),    
    Time(..),
    CheckIn(..),
    CheckOut(..),
    Price(..),
    Reservation(..),
    AvailableHotelEntity(..),
    parseQuery,
    emptyState,
    stateTransition,
    formatHotel,
    formatReservation

    ) where

import Data.Char (isSpace, isDigit)
import Data.List (isPrefixOf, tails, findIndex, intercalate, find)
import Debug.Trace (trace)
import Foreign.C (errnoToIOError)


-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query =
  Add ID Hotel|
  Remove ID |
  MakeReservation Guest ID CheckIn CheckOut Price  |
  CancelReservation ID |
  AddAdditionalGuest Guest ID |
  ListState


newtype ID = ID Int
  deriving (Eq)

instance Show Lib2.ID where
  show (ID id) = show id


data Guest = Guest{
  guestName :: String,
  guestSurname :: String 
} deriving (Show, Eq)

data Hotel = Hotel {
  hotelName :: String,
  hotelChain :: [Hotel], -- possible that the hotel may not have a chain (empty)
  floors :: [Floor]
} deriving (Show, Eq)

data Floor = Floor {
  floorNumber :: Int,
  rooms :: [Room] -- floor can have many rooms attached to it
} deriving (Show, Eq)

data Room = Room {
  roomNumber :: Int,
  roomSections :: [Room], -- empty list if no room sections
  amenities :: [Amenity]
} deriving (Show, Eq)

data Amenity = TV | WiFi | Minibar | Balcony | AC | Unknown
  deriving (Show, Eq)

data Date = Date { year :: Int, month :: Int, day :: Int} deriving (Eq)
instance Show Lib2.Date where
  show (Date year month day) = show year ++ "-" ++ show month ++ "-" ++ show day

data Time = Time {
  hour :: Int,
  minute :: Int
} deriving (Eq)

instance Show Lib2.Time where
  show (Time h m) = show h ++ ":" ++ show m

data CheckIn = CheckIn {
  checkInDate :: Date,
  checkOuttime :: Time
} deriving (Eq)

instance Show Lib2.CheckIn where
  show (CheckIn d t) = show d ++ show t


data CheckOut = CheckOut {
  checkOutDate :: Date,
  checkOutTime :: Time
} deriving (Show, Eq)

data Price = Price Int
  deriving (Eq)

instance Show Lib2.Price where
  show (Price p) = show p



-- | Parser type
type Parser a = String -> Either String (a, String)


-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _= False

-- instance Show Lib2.Query where
--   show query = 


stateToString :: State -> String
stateToString st =
  let reservationsList = map formatReservation (reservations st) -- converting reservations to string format
      hotelsList = map formatHotel (availableHotelEntities st)
  in "Reservations: \n" ++ unlines reservationsList ++
     "\n----------------------------\nAvailable hotels/hotel rooms:\n" ++ unlines hotelsList

  
  
-- After all query entries are converted into seperate lines, parse them as seperate inputs
parseLine :: Parser String
parseLine input =
  case lines input of
    (line:rest) -> Right (line, unlines rest)
    [] -> Left "Expected a dot (end of query operation), but got end of input."

-- | Keyword parcing
-- Utility function to check if a line starts with a specific keyword
startsWith :: String -> String -> Bool
startsWith keyword line = keyword `isPrefixOf` line

-- Utility function to parse a specific keyword
parseKeyword :: String -> Parser String
parseKeyword keyword input =
  case parseLine input of
    Right (line, rest) ->
      if startsWith keyword line
        then Right (line, rest)
        else Left $ "Expected keyword: " ++ keyword
    Left err -> Left err

-- | Helper functions for multiple parsers
and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' c a b input =
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) -> Right (c v1 v2, r2)
        Left e2 -> Left e2
    Left e1 -> Left e1

and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' d a b c input =
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) ->
          case c r2 of
            Right (v3, r3) -> Right (d v1 v2 v3, r3)
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

and5' :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f 
and5' f a b c d e input =
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) ->
          case c r2 of
            Right (v3, r3) ->
              case d r3 of 
                Right (v4, r4) ->
                  case e r4 of
                    Right (v5, r5) -> Right (f v1 v2 v3 v4 v5, r5)
                    Left e5 -> Left e5
                Left e4 -> Left e4
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

or2 :: Parser a -> Parser a -> Parser a
or2 a b = \input ->
    case a input of
        Right r1 -> Right r1
        Left e1 ->
            case b input of
                Right r2 -> Right r2
                Left e2 -> Left (e1 ++ ", " ++ e2)


or3 :: Parser a -> Parser a -> Parser a -> Parser a
or3 a b c = \input ->
  case a input of
    Right r1 -> Right r1
    Left e1 ->
      case b input of
        Right r2 -> Right r2
        Left e2 ->
          case c input of
            Right r3 -> Right r3
            Left e3 -> Left (e1 ++ ", " ++ e2 ++ ", " ++ e3)

or4 :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or4 a b c d = \input ->
  case a input of
    Right r1 -> Right r1
    Left e1 ->
      case b input of
        Right r2 -> Right r2
        Left e2 ->
          case c input of
            Right r3 -> Right r3
            Left e3 ->
              case d input of
                Right r4 -> Right r4
                Left e4 -> Left (e1 ++ ", " ++ e2 ++ ", " ++ e3 ++ ", " ++ e4)





-- | Parses user's input.
-- The function must have tests.
-- Is pat pradziu buvau padares su Parser, todel kad visko taip stipriai neperrasyti tiesiog pasiimti query is tuple (query, string)
parseQuery :: String -> Either String Query
parseQuery input = case splitOn ". " input of
  ("ADD":rest) -> fmap fst (parseAdd (unlines rest))
  ("REMOVE":rest) -> fmap fst (parseRemove (unlines rest))
  ("MAKE RESERVATION":rest) -> fmap fst (parseMakeReservation (unlines rest))
  ("CANCEL RESERVATION":rest) -> fmap fst (parseCancelReservation (unlines rest))
  ("ADD ADDITIONAL GUEST":rest) -> fmap fst (parseAddAdditionalGuest (unlines rest))
  ("LIST":_) -> Right ListState
  _ -> Left "Invalid command"

parseInt :: Parser Int
parseInt input =
  let (numberStr, remaining) = span isDigit input
  in if null numberStr
       then Left "Expected an integer"
       else Right (read numberStr, remaining)

parseID :: Parser ID
parseID input = 
  case parseInt input of
    Right (intValue, remaining) -> 
      let remaining' = dropWhile isSpace (drop (length (show intValue)) remaining)
      in Right (ID intValue, remaining')
    Left err -> Left err

-- <add> ::= "ADD. " <hotelsID> <hotel> 
parseAdd :: Parser Query
parseAdd input =
  case parseID input of
    Right (id, rest) ->
      case parseHotel rest of
        Right (hotel, remaining) -> Right (Add id hotel, remaining)
        Left err -> Left err
    Left err -> Left err

-- <remove> ::= "REMOVE. " <hotelsID>
parseRemove :: Parser Query
parseRemove input =
  case parseID input of
    Right (id, remaining) -> Right (Remove id, remaining)
    Left err -> Left err

-- <make_reservation> ::= "MAKE RESERVATION. " <guest> <hotelsID> <check_in> <check_out> <price>
parseMakeReservation :: Parser Query
parseMakeReservation =
  and5' MakeReservation parseGuest parseID parseCheckIn parseCheckOut parsePrice


-- <cancel_reservation> ::= "CANCEL RESERVATION. " <reservationID>
parseCancelReservation :: Parser Query
parseCancelReservation input =
  case parseID input of
    Right (id, remaining) -> Right (CancelReservation id, remaining)
    Left err -> Left err

-- <add_additional_guest> ::= "ADD ADDITIONAL GUEST. " <guest> <reservationID>
parseAddAdditionalGuest :: Parser Query
parseAddAdditionalGuest =
  and2' AddAdditionalGuest parseGuest parseID

-- | Parsing the hotel.
-- <hotel> ::= "HOTEL: " <text> ". " |  <hotel> "CHAIN OF " <hotel> | <hotel> <floors>
parseHotel :: Parser Hotel
parseHotel = or3 parseChainedHotel parseHotelWithFloors parseSingleHotel

-- ///

parseSingleHotel :: Parser Hotel
parseSingleHotel input =
  case parseHotelName input of
    Right (name, rest) -> Right (Hotel name [] [], rest)
    Left err -> Left err

parseHotelName :: Parser String
parseHotelName input =
  case parseKeyword "HOTEL: " input of
    Right (line, rest) -> Right (drop 7 line, rest) -- removing hotel prefix
    Left err -> Left err

-- ///

parseChainedHotel :: Parser Hotel
parseChainedHotel input =
  case parseHotelName input of
    Right (name, rest1) ->
      case parseHotelChain rest1 of
        Right (chain, rest2) -> 
          case parseFloors rest2 of
            Right (floors, rest3) -> Right (Hotel name chain floors, rest3)
            Left err -> Right (Hotel name chain [], err) 
    Left err -> Left err

parseHotelChain :: Parser [Hotel]
parseHotelChain input =
  case parseLine input of
    Right (line, rest) ->
      if startsWith "CHAIN OF" line
        then case parseHotel rest of
          Right (nextHotel, remaining) ->
            case parseHotelChain remaining of
              Right (moreHotels, finalRest) ->
                Right (nextHotel : moreHotels, finalRest)
              Left _ -> Right ([nextHotel], remaining)
          Left _ -> Right ([], input)
      else Right ([], input)
    Left _ -> Right ([], input)

-- ///

parseHotelWithFloors :: Parser Hotel
parseHotelWithFloors input =
  case parseHotelName input of
    Right (name, rest1) ->
      case parseFloors rest1 of
        Right (floors, rest2) -> Right (Hotel name [] floors, rest2)
        Left err              -> Left err
    Left err -> Left err


-- | Parsing floors.
-- <floors> ::= <floor> | <floor> <floors>
parseFloors :: Parser [Floor]
parseFloors = or2 parseMultipleFloors parseSingleFloor

parseMultipleFloors :: Parser [Floor]
parseMultipleFloors input =
  case parseFloor input of
    Right (floor, remaining) ->
      case parseFloors remaining of
        Right (moreFloors, finalRest) -> Right (floor: moreFloors, finalRest)
        Left _ -> Right ([floor], remaining)
    Left err -> Left err 

parseSingleFloor :: Parser [Floor] -- cannot use the parseFloor, since it is Parser Floor, not [Floor], so the or comparison will not work
parseSingleFloor input =
  case parseFloor input of
    Right (floor, remaining) -> Right ([floor], remaining)
    Left err -> Left err


-- <floor> ::= "FLOOR: " <number> ". " <rooms>
parseFloor :: Parser Floor
parseFloor input = 
  case parseKeyword "FLOOR: " input of
    Right (line, rest) ->
      let floorNum = read (drop 7 line) :: Int -- removing floor prefix and reading the number
      in case parseRooms rest of
           Right (roomsList, finalRest) -> Right (Floor floorNum roomsList, finalRest)
           Left err -> Left err
    Left err -> Left err

-- ///

-- | Parsing rooms.

parseRooms :: Parser [Room]
parseRooms = or2 parseMultipleRooms parseSingleRoom

-- <rooms> :: <room> | <room> <rooms>
parseMultipleRooms :: Parser [Room]
parseMultipleRooms input =
  case parseRoom input of
    Right (room, remaining) ->
      case parseRooms remaining of
        Right (moreRooms, finalRest) ->
          Right (room : moreRooms, finalRest)
        Left _ -> Right ([room], remaining)
    Left err -> Left err

parseSingleRoom :: Parser [Room] -- cannot use parseRoom, since or will require Parser [Room], not Room
parseSingleRoom input = 
  case parseRoom input of
    Right (room, remaining) -> Right ([room], remaining)
    Left err -> Left err

-- <room> ::= "ROOM: " <number> ". " | <room> "ROOM SECTION " <room> | <room> <amenities> ". "
parseRoom :: Parser Room
parseRoom = or4 parseRoomWithSecAndAmen parseRoomWithSections parseRoomWithAmenities parseSimpleRoom

parseRoomWithSecAndAmen :: Parser Room
parseRoomWithSecAndAmen input =
  case parseRoomNumber input of
    Right (roomNumber, rest) ->
      case parseAmenities rest of
        Right (amenities, rest2) ->
          case parseKeyword "ROOM SECTION" rest2 of
            Right (_, remaining) ->
              case parseRoom remaining of
                Right (nextRoom, finalRest) ->
                  case parseRoomWithSecAndAmen finalRest of
                    Right (moreRooms, finalRest2) ->
                      Right (Room roomNumber (nextRoom : [moreRooms]) amenities, finalRest2)
                    Left _ ->
                       Right (Room roomNumber [nextRoom] amenities, finalRest)
                Left err -> Left err
            Left err -> Left err
        Left err -> Left err
    Left err -> Left err

parseRoomWithSections :: Parser Room
parseRoomWithSections input =
  case parseRoomNumber input of
    Right (roomNumber, rest) ->
      case parseKeyword "ROOM SECTION" rest of
        Right (roomSection, remaining) ->
          case parseRoom remaining of
            Right (nextRoom, remaining2) ->
              case parseRoomWithSecAndAmen remaining2 of
                Right (moreRooms, finalRest) -> 
                  Right (Room roomNumber (nextRoom : [moreRooms]) [], finalRest)
                Left err ->
                  Right (Room roomNumber [nextRoom] [], err)
            Left err -> Left err 
        Left err -> Left err 
    Left err -> Left err

parseRoomWithAmenities :: Parser Room
parseRoomWithAmenities input =
  case parseRoomNumber input of
    Right (roomNumber, rest) ->
      case parseAmenities rest of
        Right (amenities, finalRest) ->
          Right (Room roomNumber [] amenities, finalRest)
        Left err -> Left err 
    Left err -> Left err

parseSimpleRoom :: Parser Room
parseSimpleRoom input =
  case parseRoomNumber input of
    Right (roomNumber, rest) -> Right (Room roomNumber [] [], rest)
    Left err -> Left err
       

parseRoomNumber :: Parser Int
parseRoomNumber input =
  case parseKeyword "ROOM: " input of
    Right (line, rest) -> 
      let numStr = drop 6 line  -- Removing "ROOM: "
      in Right (read numStr :: Int, rest)  -- Convert String to Int
    Left err -> Left err


    




-- parseRoom :: Parser Room
-- parseRoom input =
--   case parseKeyword "ROOM: " input of
--     Right (line, rest) ->
--       let roomNum = read (drop 6 line) :: Int
--       in case parseAmenities rest of  
--            Right (amenitiesList, remaining) ->
--              case parseRoomSections remaining of  
--                Right (sections, finalRest) ->
--                  Right (Room roomNum sections amenitiesList, finalRest)
--                Left err -> Left err
--            Left err -> Left err
--     Left err -> Left err

-- parseRoomSections :: Parser [Room]
-- parseRoomSections input =
--   case parseKeyword "ROOM SECTION" input of
--     Right (_, rest) ->
--       case parseRoom rest of
--         Right (roomSection, remaining) ->
--           case parseRoomSections remaining of
--             Right (sections, finalRest) -> Right (roomSection : sections, finalRest)
--             Left _ -> Right ([roomSection], remaining)
--         Left _ -> Right ([], input)
--     Left _ -> Right ([], input)

-- | Parsing amenities.

-- <amenities> ::= "AMENITIES: " <amenity> | <amenities> ", " <amenity>
parseAmenities :: Parser [Amenity]
parseAmenities = or2 parseMultipleAmenities parseSingleAmenity

parseMultipleAmenities :: Parser [Amenity]
parseMultipleAmenities input =
  case parseKeyword "AMENITIES: " input of
    Right (line, rest) ->
      let amenitiesStr = drop 11 line
          amenitiesWords = splitOn ", " amenitiesStr
      in Right (parseAmenitiesList amenitiesWords, rest)
    Left _ -> Right ([], input)  

parseSingleAmenity :: Parser [Amenity]
parseSingleAmenity input =
  case parseKeyword "AMENITIES: " input of
    Right (line, rest) ->
      let amenitiesStr = drop 11 line
          amenitiesWords = splitOn ", " amenitiesStr
      in case amenitiesWords of
        [] -> Right ([], rest) 
        [amenity] -> Right ([parseAmenity amenity], rest)  
      

parseAmenitiesList :: [String] -> [Amenity]
parseAmenitiesList [] = []
parseAmenitiesList (word:rest) =
  let amenity = parseAmenity word
  in amenity : parseAmenitiesList rest

-- <amenity> ::= "TV" | "WI-FI" | "MINI-BAR" | "BALCONY" | "AC"
parseAmenity :: String -> Amenity
parseAmenity str = case str of
  "TV" -> TV
  "WiFi" -> WiFi
  "Minibar" -> Minibar
  "Balcony" -> Balcony
  "AC" -> AC
  _ -> Unknown

-- <guest> ::= "GUEST: " <name> " " <surname> ". "
parseGuest :: Parser Guest
parseGuest input =
  case parseKeyword "GUEST: " input of
    Right (line, rest) ->
      let names = words (drop (length "GUEST: ") line)
      in case names of
        [name, surname] -> Right (Guest name surname, rest)
        _ -> Left "Invalid guest format."
    Left _ -> Left "Invalid guest format."

-- <check_in> ::= "CHECK IN: " <date> " " <time> ". "
parseCheckIn :: Parser CheckIn
parseCheckIn input = 
  --trace ("Parsing CheckIn from input: " ++ input) $
  case parseKeyword "CHECK IN: " input of
    Right (line, rest) ->
      let parts = words (drop (length "CHECK IN: ") line)
      in case parts of
        [dateString, timeString] ->
          case parseDate dateString of
            Right (date, _) ->
              case parseTime timeString of
                Right (time, _) -> Right (CheckIn date time, rest)
                Left err -> Left err
            Left err -> Left err
        _ -> Left "Invalid check-in format."
    Left _ -> Left "Invalid check-in format."

-- <check_out> ::= "CHECK OUT: " <date> " " <time> ". "
parseCheckOut :: Parser CheckOut
parseCheckOut input =
  case parseKeyword "CHECK OUT: " input of
    Right (line, rest) ->
      let parts = words (drop (length "CHECK OUT: ") line)
      in case parts of
        [dateString, timeString] ->
          case parseDate dateString of
            Right (date, _) ->
              case parseTime timeString of
                Right (time, _) -> Right (CheckOut date time, rest)
                Left err -> Left err
            Left err -> Left err
        _ -> Left "Invalid check-out format."
    Left _ -> Left "Invalid check-out format."

-- <date> ::= <digit> <digit> <digit> <digit> "-" <digit> <digit> "-" <digit> <digit>
parseDate :: Parser Date
parseDate input =
  let (dateStr, rest) = break isSpace input
      dateParts = splitOn "-" dateStr
  in case dateParts of
       [yearStr, monthStr, dayStr] ->
         let yearParsed = read yearStr :: Int
             monthParsed = read monthStr :: Int
             dayParsed = read dayStr :: Int
         in Right (Date yearParsed monthParsed dayParsed, dropWhile isSpace rest)
       _ -> Left "Invalid date format."

-- <time> ::= <digit> <digit> ":" <digit> <digit>
parseTime :: Parser Time
parseTime input =
  let (timeStr, rest) = break isSpace input
      timeParts = splitOn ":" timeStr
  in case timeParts of
       [hourStr, minuteStr] ->
         let hourParsed = read hourStr :: Int
             minuteParsed = read minuteStr :: Int
         in Right (Time hourParsed minuteParsed, dropWhile isSpace rest)
       _ -> Left "Invalid time format."


-- | Function to split strings based on another string.

splitOn :: String -> String -> [String]
splitOn _ [] = []
splitOn delimiter str =
  let (before, remainder) = breakOn delimiter str
  in before : if null remainder
     then []
     else splitOn delimiter (drop (length delimiter) remainder)

-- Helper function to find the first occurrence of the delimiter
breakOn :: String -> String -> (String, String)
breakOn delim s =
  case findIndex (isPrefixOf delim) (tails s) of
    Just idx -> splitAt idx s
    Nothing  -> (s, "")

-- <price> ::= "PRICE: " <number> ". "
parsePrice :: Parser Price
parsePrice input = 
  case parseKeyword "PRICE: " input of
    Right (line, rest) ->
      let number = read (drop 7 line) :: Int 
      in Right (Price number, rest)
    Left err -> Left err




-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Reservation = Reservation {
  reservationID :: ID,
  hotel :: Hotel,
  guests :: [Guest],
  checkIn :: CheckIn,
  checkOut :: CheckOut,
  price :: Price
} deriving (Show, Eq)

data AvailableHotelEntity = AvailableHotelEntity {
  availableEntityId :: ID,
  availableHotel :: Hotel
} deriving (Show, Eq)

data State = State {
  reservations :: [Reservation],
  availableHotelEntities :: [AvailableHotelEntity]
} deriving (Eq)

instance Show Lib2.State where
  show = stateToString

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State {
  reservations = [],
  availableHotelEntities = []
}

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = case query of
  Add id hotel ->
    if any (\h -> availableEntityId h == id) (availableHotelEntities st)
      then Left "Error: Hotel with this ID already exists."
      else
        let newHotelEntity = AvailableHotelEntity id hotel
            newHotels = newHotelEntity : availableHotelEntities st
            newState = st { availableHotelEntities = newHotels }
        in Right (Just "Hotel added successfully!", newState)

  Remove (ID entityId) ->
    let newHotelEntities = filter (\h -> availableEntityId h /= ID entityId) (availableHotelEntities st)
        newState = st { availableHotelEntities = newHotelEntities }
    in Right (Just "Hotel removed successfully!", newState)

  MakeReservation guest hotelID checkInDate checkOutDate reservationPrice ->
    -- Check if the hotel exists in availableHotelEntities
    case find (\h -> availableEntityId h == hotelID) (availableHotelEntities st) of
      Just (AvailableHotelEntity _ hotel) ->
        let newId = ID (length (reservations st) + 1)
            newReservation = Reservation newId hotel [guest] checkInDate checkOutDate reservationPrice
            newReservations = newReservation : reservations st
            newState = st { reservations = newReservations }
        in Right (Just "Reservation made successfully!", newState)
      Nothing ->
        Left "Error: Hotel does not exist."

  CancelReservation (ID resID) ->
    let newReservations = filter (\r -> reservationID r /= ID resID) (reservations st)
        newState = st { reservations = newReservations }
    in Right (Just "Reservation cancelled successfully!", newState)

  AddAdditionalGuest guest (ID reservID) ->
    case filter (\r -> reservationID r == ID reservID) (reservations st) of
      [] -> Left "Error: Reservation not found."
      (reservation:_) ->
        let updatedReservation = reservation { guests = guest : guests reservation }
            newReservations = updatedReservation : filter (\r -> reservationID r /= ID reservID) (reservations st)
            newState = st { reservations = newReservations }
        in Right (Just "Guest added successfully!", newState)

  ListState ->
    let reservationsList = map formatReservation (reservations st) -- converting reservations to string format
        hotelsList = map formatHotel (availableHotelEntities st)
        result = "Reservations: \n" ++ unlines reservationsList ++
                  "\nAvailable hotels/hotel rooms:\n" ++ unlines hotelsList
    in Right (Just result, st)

-- | Formatiing for the ListState.
formatHotel :: AvailableHotelEntity -> String
formatHotel (AvailableHotelEntity (ID id) hotel) =
  "Hotel ID: " ++ show id ++ "\n" ++ formatHotelDetails hotel

formatHotelDetails :: Hotel -> String
formatHotelDetails (Hotel name chain floors) =
  "Hotel Name: " ++ name ++ "\n" ++
  (if null chain
    then ""
    else "Hotel Chain: \n" ++ unlines (map formatHotelDetails chain)) ++
  (if null floors
    then ""
    else "Floors:\n" ++ unlines (map formatFloor floors))

formatFloor :: Floor -> String
formatFloor (Floor number rooms) =
  "  Floor Number: " ++ show number ++ "\n" ++
  "  Rooms:\n" ++ unlines (map formatRoom rooms)

formatRoom :: Room -> String
formatRoom (Room number sections amenities) =
  "    Room Number: " ++ show number ++ "\n" ++
  (if null amenities
     then "    Amenities: None\n"
     else "    Amenities: " ++ unwords (map show amenities) ++ "\n") ++
  (if null sections
     then "    Sections: None\n" 
     else "\n    Sections: \n" ++ unlines (map formatRoom sections))

formatReservation :: Reservation -> String
formatReservation (Reservation (ID id) hotel guests checkIn checkOut price) =
  "Reservation ID: " ++ show id ++ "\n" ++
  formatHotelDetails hotel ++
  "Guests:\n" ++ unlines (map formatGuest guests) ++
  "Check-in: " ++ formatCheckIn checkIn ++ "\n" ++
  "Check-out: " ++ formatCheckOut checkOut ++ "\n" ++
  "Price: " ++ formatPrice price ++ " euros \n"

formatGuest :: Guest -> String
formatGuest (Guest name surname) = 
  "Guest: " ++ name ++ " " ++ surname

formatCheckIn :: CheckIn -> String
formatCheckIn (CheckIn date time) =
  "Date: " ++ formatDate date ++ ", Time: " ++ formatTime time

formatCheckOut :: CheckOut -> String
formatCheckOut (CheckOut date time) =
  "Date: " ++ formatDate date ++ ", Time: " ++ formatTime time

formatDate :: Date -> String
formatDate (Date year month day) =
  show year ++ "-" ++ show month ++ "-" ++ show day

formatTime :: Time -> String
formatTime (Time hour minute) =
  show hour ++ ":" ++ show minute

formatPrice :: Price -> String
formatPrice (Price number) =
  show number
  


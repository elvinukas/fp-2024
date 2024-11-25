{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
import Test.Tasty ( TestTree, defaultMain, testGroup, )
import Test.Tasty.HUnit ( testCase, (@?=), assertFailure)
import Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck (Arbitrary, arbitrary, oneof, vectorOf)


import Data.List
import Data.Ord

import Data.Either (isRight)

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
--  tests = testGroup "Tests" [unitTests]
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 / Lib2 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,
    testCase "Parsing basic hotel variant" $
      isRight (Lib2.parseQuery "ADD. 1. HOTEL: Grand. FLOOR: 1. ROOM: 101. ") @?= True,
    testCase "Parsing incorrect variant" $
      Lib2.parseQuery "ADD. " @?= (Left "Expected an integer"),
    testCase "Parsing add hotel variant with chains and amenities" $
      isRight (Lib2.parseQuery "ADD. 1. HOTEL: Grand. FLOOR: 1. ROOM: 101. AMENITIES: TV, AC, WiFi. ") @?= True,
    testCase "Testing adding and removing hotel variant" $ do
      
      let initialState = Lib2.emptyState

      case Lib2.parseQuery "ADD. 1. HOTEL: Grand. FLOOR: 1. ROOM: 101. " of
        Left err -> assertFailure $ "Failed to parse 'ADD' query: " ++ err
        Right addQuery -> 
          case Lib2.stateTransition initialState addQuery of
            Left err -> assertFailure $ "Failed to execute 'ADD' query: " ++ err
            Right (_, stateAfterAdd) -> 
              case Lib2.parseQuery "REMOVE. 1. " of
                Left err -> assertFailure $ "Failed to parse 'REMOVE' query: " ++ err
                Right removeQuery -> do
                  let result = Lib2.stateTransition stateAfterAdd removeQuery

                  result @?= Right (Just "Hotel removed successfully!", stateAfterAdd { Lib2.availableHotelEntities = [] }),
    testCase "Testing adding and making a reservation on a hotel room" $ do
      let initialState = Lib2.emptyState
      
      case Lib2.parseQuery "ADD. 1. HOTEL: Grand. FLOOR: 1. ROOM: 101. " of
        Left err -> assertFailure $ "Failed to parse 'ADD' query: " ++ err
        Right addQuery ->
          case Lib2.stateTransition initialState addQuery of
            Left err -> assertFailure $ "Failed to execute 'ADD query: " ++ err
            Right (_, stateAfterAdd) ->
              case Lib2.parseQuery ("MAKE RESERVATION. GUEST: Elvinukas Testukas. 1. CHECK IN: 2020-01-01 10:00. " ++ 
              "CHECK OUT: 2020-02-02 12:00. PRICE: 210. ") of
                Left err -> assertFailure $ "Failed to parse 'MAKE RESERVATION' query: " ++ err
                Right makeReservationQuery -> do
                  let result = Lib2.stateTransition stateAfterAdd makeReservationQuery
                  case result of
                    Left err -> assertFailure $ "Failed to execute 'MAKE RESERVATION' query: " ++ err
                    Right (_, newState) -> 
                      result @?= Right (Just "Reservation made successfully!", newState),

    testCase "Testing making a reservation and then adding an additional guest" $ do
      let initialState = Lib2.emptyState
      
      case Lib2.parseQuery "ADD. 1. HOTEL: Grand. FLOOR: 1. ROOM: 101. " of
        Left err -> assertFailure $ "Failed to parse 'ADD' query: " ++ err
        Right addQuery ->
          case Lib2.stateTransition initialState addQuery of
            Left err -> assertFailure $ "Failed to execute 'ADD query: " ++ err
            Right (_, stateAfterAdd) ->
              case Lib2.parseQuery ("MAKE RESERVATION. GUEST: Elvinukas Testukas. 1. CHECK IN: 2020-01-01 10:00. " ++ 
              "CHECK OUT: 2020-02-02 12:00. PRICE: 210. ") of
                Left err -> assertFailure $ "Failed to parse 'MAKE RESERVATION' query: " ++ err
                Right makeReservationQuery -> do
                  let result = Lib2.stateTransition stateAfterAdd makeReservationQuery
                  case result of
                    Left err -> assertFailure $ "Failed to execute 'MAKE RESERVATION' query: " ++ err
                    Right (_, newState) -> do
                      result @?= Right (Just "Reservation made successfully!", newState)
                      (case Lib2.parseQuery "ADD ADDITIONAL GUEST. GUEST: antras testukas. 1. " of
                        Left err -> assertFailure $ "Failed to parse 'ADD ADDITIONAL GUEST' query: " ++ err
                        Right addGuestQuery -> do
                          let result = Lib2.stateTransition newState addGuestQuery
                          case result of
                            Left err -> assertFailure $ "Failed to execute 'ADD ADDITIONAL GUEST' query: " ++ err
                            Right (_, finalState) ->
                              result @?= Right (Just "Guest added successfully!", finalState))

    ]
    

-- | Property testing.
propertyTests :: TestTree
propertyTests = testGroup "Property tests"
  [ testProperty "Checking if rendering statement and parsing it back gives the original statement" $
      let statement = Lib3.Single (Lib2.Add (Lib2.ID 1) (Lib2.Hotel "Grand" [] [Lib2.Floor 1 [Lib2.Room 101 [] []]]))
          renderedStatements = Lib3.renderStatements statement
          parsedStatements = Lib3.parseStatements renderedStatements
      in counterexample
          ("renderedStatements: " ++ renderedStatements ++ "\nparsedStatements: " ++ show parsedStatements)
            (case parsedStatements of
              Right (parsedStmt, "") -> renderedStatements == show parsedStmt
              _ -> False)
          
  , testProperty "Checking if rendering statement and parsing it back gives the original statement (harder)" $
      let statements = Lib3.Batch
            [ Lib2.Add (Lib2.ID 1) (Lib2.Hotel "Ausra" [] [Lib2.Floor 1 [Lib2.Room 101 [] []]])
            , Lib2.Add (Lib2.ID 2) (Lib2.Hotel "Rytas" [] [Lib2.Floor 2 [Lib2.Room 202 [] []]])
            , Lib2.MakeReservation (Lib2.Guest "Valdas" "Adamkus") (Lib2.ID 1) (Lib2.CheckIn (Lib2.Date 2022 02 20) (Lib2.Time 12 30))
             (Lib2.CheckOut (Lib2.Date 2022 02 22) (Lib2.Time 13 30)) (Lib2.Price 500)
            ]
          renderedStatements = Lib3.renderStatements statements
          parsedStatements = Lib3.parseStatements renderedStatements
      in counterexample
          ("renderedStatements: " ++ renderedStatements ++ "\nparsedStatements: " ++ show parsedStatements)
            (case parsedStatements of
              Right (parsedStmt, "") -> renderedStatements == show parsedStmt
              _ -> False)
      
  ]









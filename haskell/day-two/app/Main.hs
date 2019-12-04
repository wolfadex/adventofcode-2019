module Main where

import Lib
import qualified Data.Sequence as Seq
import Debug.Trace

main :: IO ()
main = do
  putStrLn $ (++) "Part 1: " $ show $ Seq.lookup 0 $ compute 0 (setVerb 2 (setNoun 12 puzzleInput))
  -- putStrLn $ (++) "Part 2: " $



type Instruction = Int

type Pointer = Int

type Operation = Int -> Int -> Int

compute :: Pointer -> Seq.Seq Int -> Seq.Seq Int
compute instructionPointer items =
  computeHelper
    (Seq.lookup instructionPointer items)
    (Seq.lookup (instructionPointer + 1) items)
    (Seq.lookup (instructionPointer + 2) items)
    (Seq.lookup (instructionPointer + 3) items)
    instructionPointer
    items

computeHelper :: Maybe Instruction -> Maybe Pointer -> Maybe Pointer -> Maybe Pointer -> Pointer -> Seq.Seq Int -> Seq.Seq Int
computeHelper (Just 99) _ _ _ _ items = items
computeHelper (Just 1) (Just aPoint) (Just bPoint) (Just resultPointer) instructionPointer items = operate aPoint bPoint (+) resultPointer items instructionPointer
computeHelper (Just 2) (Just aPoint) (Just bPoint) (Just resultPointer) instructionPointer items = operate aPoint bPoint (*) resultPointer items instructionPointer
computeHelper _ _ _ _ _ items = items


operate :: Pointer -> Pointer -> Operation -> Pointer -> Seq.Seq Int -> Pointer -> Seq.Seq Int
operate aPoint bPoint oper resPointer items instructionPointer =
  let newItems = nextItems a b oper resPointer items
  in computeHelper
    (Seq.lookup nextInsPointer newItems)
    (Seq.lookup (nextInsPointer + 1) newItems)
    (Seq.lookup (nextInsPointer + 2) newItems)
    (Seq.lookup (nextInsPointer + 3) newItems)
    nextInsPointer
    newItems
  where
    a = Seq.lookup aPoint items
    b = Seq.lookup bPoint items
    nextInsPointer = instructionPointer + 4


nextItems :: Maybe Int -> Maybe Int -> Operation -> Pointer -> Seq.Seq Int -> Seq.Seq Int
nextItems (Just a) (Just b) oper resPointer items = Seq.update resPointer (oper a b) items
nextItems _ _ _ _ items = items


goalValue :: Int
goalValue =
    19690720


setNoun :: Int -> Seq.Seq Int -> Seq.Seq Int
setNoun =
  Seq.update 1


setVerb :: Int -> Seq.Seq Int -> Seq.Seq Int
setVerb =
  Seq.update 2


puzzleInput :: Seq.Seq Int
puzzleInput =
    Seq.fromList
        [ 1
        , 0
        , 0
        , 3
        , 1
        , 1
        , 2
        , 3
        , 1
        , 3
        , 4
        , 3
        , 1
        , 5
        , 0
        , 3
        , 2
        , 13
        , 1
        , 19
        , 1
        , 19
        , 10
        , 23
        , 1
        , 23
        , 13
        , 27
        , 1
        , 6
        , 27
        , 31
        , 1
        , 9
        , 31
        , 35
        , 2
        , 10
        , 35
        , 39
        , 1
        , 39
        , 6
        , 43
        , 1
        , 6
        , 43
        , 47
        , 2
        , 13
        , 47
        , 51
        , 1
        , 51
        , 6
        , 55
        , 2
        , 6
        , 55
        , 59
        , 2
        , 59
        , 6
        , 63
        , 2
        , 63
        , 13
        , 67
        , 1
        , 5
        , 67
        , 71
        , 2
        , 9
        , 71
        , 75
        , 1
        , 5
        , 75
        , 79
        , 1
        , 5
        , 79
        , 83
        , 1
        , 83
        , 6
        , 87
        , 1
        , 87
        , 6
        , 91
        , 1
        , 91
        , 5
        , 95
        , 2
        , 10
        , 95
        , 99
        , 1
        , 5
        , 99
        , 103
        , 1
        , 10
        , 103
        , 107
        , 1
        , 107
        , 9
        , 111
        , 2
        , 111
        , 10
        , 115
        , 1
        , 115
        , 9
        , 119
        , 1
        , 13
        , 119
        , 123
        , 1
        , 123
        , 9
        , 127
        , 1
        , 5
        , 127
        , 131
        , 2
        , 13
        , 131
        , 135
        , 1
        , 9
        , 135
        , 139
        , 1
        , 2
        , 139
        , 143
        , 1
        , 13
        , 143
        , 0
        , 99
        , 2
        , 0
        , 14
        , 0
        ]

module Main where

import Lib
import qualified Data.Sequence as Seq
import qualified Data.Maybe as Maybe
import Debug.Trace

(<|) :: (a -> b) -> a -> b
(<|) f a =
  f a

(|>) :: a -> (a -> b) -> b
(|>) a f =
  f a

(<<) :: (b -> c) -> (a -> b) -> a -> c
(<<) fb fa a =
  fb (fa a)

(>>) :: (a -> b) -> (b -> c) -> a -> c
(>>) fa fb a =
  fb (fa a)

main :: IO ()
main = do
  (Puzzle
    { input = puzzleInput |> setNoun 12 |> setVerb 2
    , verb = 2
    , noun = 12
    , instructionPointer = 0
    })
    |> compute AnyGoal
    |> show
    |> (++) "Part 1: "
    |> putStrLn
  (Puzzle
    { input = puzzleInput
    , verb = 0
    , noun = 0
    , instructionPointer = 0
    })
    |> compute (Goal 19690720)
    |> show
    |> (++) "Part 2: "
    |> putStrLn



type Instruction = Int

type Pointer = Int

type Operation = Int -> Int -> Int

data Puzzle = Puzzle
  { input :: Seq.Seq Int
  , verb :: Int
  , noun :: Int
  , instructionPointer :: Pointer
  } deriving (Show)

data Goal = Goal Int | AnyGoal

compute :: Goal -> Puzzle -> Int
compute AnyGoal puzzle = puzzle |> computeHelper |> input |> Seq.lookup 0 |> Maybe.fromMaybe (-1)
compute (Goal goal) puzzle =
  puzzle
    |> computeHelper
    |> input
    |> Seq.lookup 0
    |> Maybe.fromMaybe (-1)
    |> (==) goal
    |> (\reachedGoal ->
          if reachedGoal then
            100 * (noun puzzle) + (verb puzzle)

          else
            compute (Goal goal) (incrementNounVerb puzzle)
        )


incrementNounVerb :: Puzzle -> Puzzle
incrementNounVerb puzzle =
  let
    n = noun puzzle
    v = verb puzzle
    nextNoun = if n + 1 > 99 then 0 else n + 1
    nextVerb = if n + 1 > 99 then v + 1 else v
  in
  puzzle { noun = nextNoun
         , verb = nextVerb
         , input = (input puzzle) |> setNoun nextNoun |> setVerb nextVerb
         }
  

computeHelper :: Puzzle -> Puzzle
computeHelper puzzle =
      execInstruction (Seq.lookup (instructionPointer puzzle) (input puzzle)) puzzle

execInstruction :: Maybe Instruction -> Puzzle -> Puzzle
execInstruction (Just 99) puzzle = puzzle
execInstruction (Just 1) puzzle = puzzle |> operate (+) |> computeHelper
execInstruction (Just 2) puzzle = puzzle |> operate (*) |> computeHelper
execInstruction _ puzzle = puzzle

operate :: Operation -> Puzzle -> Puzzle
operate oper puzzle =
  let
    inp = input puzzle
    inPointer = instructionPointer puzzle
    maybeAPoint = Seq.lookup (inPointer + 1) inp
    maybeBPoint = Seq.lookup (inPointer + 2) inp
    resPoint = Seq.lookup (inPointer + 3) inp
  in case ( maybeAPoint, maybeBPoint, resPoint ) of
      ( Just aPoint, Just bPoint, Just res ) ->
        let
          maybeA = Seq.lookup aPoint inp
          maybeB = Seq.lookup bPoint inp
        in case ( maybeA, maybeB ) of
          ( Just a, Just b ) -> puzzle { input = Seq.update res (oper a b) inp, instructionPointer = inPointer + 4 }
          _ -> puzzle
      _ -> puzzle


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

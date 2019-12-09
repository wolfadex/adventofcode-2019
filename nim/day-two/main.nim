from strformat import fmt
import Options

var puzzleInput = @[1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 13, 1,
    19, 1, 19, 10, 23, 1, 23, 13, 27, 1, 6, 27, 31, 1, 9, 31, 35, 2, 10, 35, 39,
    1, 39, 6, 43, 1, 6, 43, 47, 2, 13, 47, 51, 1, 51, 6, 55, 2, 6, 55, 59, 2,
    59, 6, 63, 2, 63, 13, 67, 1, 5, 67, 71, 2, 9, 71, 75, 1, 5, 75, 79, 1, 5,
    79, 83, 1, 83, 6, 87, 1, 87, 6, 91, 1, 91, 5, 95, 2, 10, 95, 99, 1, 5, 99,
    103, 1, 10, 103, 107, 1, 107, 9, 111, 2, 111, 10, 115, 1, 115, 9, 119, 1,
    13, 119, 123, 1, 123, 9, 127, 1, 5, 127, 131, 2, 13, 131, 135, 1, 9, 135,
    139, 1, 2, 139, 143, 1, 13, 143, 0, 99, 2, 0, 14, 0]

proc computeHelper(state: var seq[int]): seq[int] =
  var instructionPointer = 0
  while state[instructionPointer] <= state.len:
    case state[instructionPointer]
    of 99:
      break
    of 1:
      state[state[instructionPointer + 3]] = state[state[
          instructionPointer + 1]] + state[state[instructionPointer + 2]]
    of 2:
      state[state[instructionPointer + 3]] = state[state[
          instructionPointer + 1]] * state[state[instructionPointer + 2]]
    else:
      echo "This should be impossible"
    instructionPointer += 4
  state

proc compute(state: seq[int], initNoun: int, initVerb: int, goal: Option[
    int]): seq[int] =
  result = state
  var noun = initNoun
  var verb = initVerb
  while true:
    var tempState = state
    tempState[1] = noun
    tempState[2] = verb
    let computed = computeHelper(tempState)
    if goal.isNone or goal.get() == computed[0]:
      result = computed
      break
    elif goal.isSome:
      noun += 1
      if noun > 99:
        noun = 0
        verb += 1

let p1 = compute(puzzleInput, 12, 2, none(int))
echo fmt"Part 1: {p1[0]}"
let p2 = compute(puzzleInput, 0, 0, some(19690720))
echo fmt"Part 2: {100 * p2[1] + p2[2]}"

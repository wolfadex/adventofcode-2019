from sequtils import foldl
from strformat import fmt

proc compute(items: seq): int =
  foldl(items, a + (weirdMath b), 0)

proc compute2(items: seq): int =
  foldl(items, a + (solve2 b), 0)


proc weirdMath(x: int): int =
  (x div 3 - 2)


proc solve2(mass: int): int =
  var minimum = weirdMath mass
  var fuel = 0
  while minimum > 0:
    fuel += minimum
    minimum = weirdMath minimum

  return fuel

const input = @[102562, 138390, 145043, 86679, 120601, 58443, 54761, 81175,
    127897, 69559, 56776, 145671, 69003, 119334, 130205, 77249, 74637, 92068,
    66594, 90485, 140465, 73444, 107772, 107639, 144420, 58764, 56299, 66010,
    84841, 83686, 139830, 136298, 135009, 136506, 61547, 73653, 136219, 138875, 95483,
    91695, 146597, 121813, 131555, 145848, 139396, 141520, 54207, 86748, 98355,
    67179, 59820, 137299, 92371, 74512, 110854, 111960, 63787, 114701, 63773,
    127377, 128159, 120370, 138193, 106409, 135550, 107235, 56662, 99314, 69052,
    131816, 138788, 96494, 73025, 148907, 85883, 86138, 86965, 55645, 119284,
    80690, 69276, 116640, 108595, 50721, 94623, 93224, 137069, 130118, 97916,
    82232, 137621, 97909, 74061, 140419, 101795, 69316, 64973, 90578, 118503, 100369]

echo fmt"Part 1: {compute(input)}"
echo fmt"Part 2: {compute2(input)}"

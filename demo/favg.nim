# gcc requires -ffast-math to use a "matrix" accumulation vaddps in `sum`.
{.passC: "-O3 -ffast-math -march=native -mtune=native".}
import strutils, math, cligen, nio

proc favg(fmt=".5g", n=100, paths: seq[string]): int =
  ## print averages of `paths` **which must be float32 column files**
  for path in paths:
    var inp = load[float32](path)               # or `initFileArray`
    var tot = 0.0'f32                           # 64 precise; 32 enhances SIMD
    for count in 1..n:                          # too fast; Loop to time
      tot = tot + inp.toOA.sum                  # or `toOpenArray`
    echo path, " ", formatFloat(tot / inp.len.float / n.float, fmt)
    inp.close

dispatch favg

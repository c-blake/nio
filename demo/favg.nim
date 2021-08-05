{.passC: "-O3 -ffast-math -march=native -mtune=native".}
import nio, strutils, cligen

proc favg(fmt=".5g", n=100, paths: seq[string]): int =
  ## print averages of `paths` **which must be float32 column files**
  for path in paths:
    var inp = initFileArray[float32](path)
    var tot = 0.0'f32 # 64 precise, but 32 allows vectorization
    for count in 1..n:
      for f in inp:   # gcc requires -ffast-math to use a..
        tot = tot + f #.."matrix" accumulation vaddps here.
    echo path, " ", formatFloat(tot / inp.len.float, fmt)
    inp.close

dispatch favg

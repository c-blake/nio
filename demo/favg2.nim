# gcc requires -ffast-math to use a "matrix" accumulation vaddps in `sum`.
{.passC: "-O3 -ffast-math -march=native -mtune=native".}

import strutils, math, cligen, nio

proc favg(fmt=".5g", n=100, paths: seq[string]): int =
  ## print averages of `paths` **which must be float32 column files** using
  ## non-mmap IO and a GC'd IO buffer.  About 2X slower for me on Linux.
  var buf: seq[float32]         # re-use buffer over `paths`
  for path in paths:
    buf.setLen 0                # but trunc before adding
    buf.add path, naCvt=true    # read binary file and add to `buf`
    var tot = 0.0'f32           # 64 precise, but 32 enhances vectorization
    for count in 1..n: tot = tot + buf.sum
    echo path, " ", formatFloat(tot/buf.len.float/n.float, fmt)

dispatch favg

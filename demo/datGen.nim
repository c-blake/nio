import std/[os, strutils, random]
# randomize()
let n    = if paramCount() > 0: parseInt(paramStr(1)) else: 30
let m    = if paramCount() > 1: parseInt(paramStr(2)) else: 10
let mean = if paramCount() > 2: parseInt(paramStr(3)) else: 0
let sdev = if paramCount() > 2: parseInt(paramStr(3)) else: 1

let cols = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
            "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
            "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
            "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z" ]
if m > cols.len: stderr.write "too many cols\n"; quit(1)

var base = ""; var fmt = ""     # create a "descriptive" pathname
for j in 0..<m:
  base.add cols[j]
  fmt.add if mean > 0: 'l' else: 'f'

var f = open(base & ".N" & fmt, fmWrite)
for i in 0 ..< n:               # write data to data file
  for j in 0 ..< m:
    let r = float32(randState.gauss(j.float, float(j + 1)))
    if mean > 0:
      let k = int(max(0, mean.float + r * sdev.float))
      discard f.writeBuffer(k.unsafeAddr, k.sizeof)
    else:
      discard f.writeBuffer(r.unsafeAddr, r.sizeof)
close f

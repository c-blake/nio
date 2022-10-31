when not declared(stdout): import std/syncio
import std/[os, strutils, random]
# randomize()
let n = if paramCount() > 0: parseInt(paramStr(1)) else: 30
let m = if paramCount() > 1: parseInt(paramStr(2)) else: 10

let cols = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
            "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
            "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
            "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z" ]
if m > cols.len: stderr.write "too many cols\n"; quit(1)

stderr.write """--preproc=c2tsv < $1            # popen() to make strict TSV
--nHeader=1                     # number of rows which are headers
"""
for j in 0..<m:                 # write headers to data file
  if j > 0: stdout.write ','
  stdout.write cols[j]
  stderr.write cols[j], "\tf\tf\n"
stdout.write '\n'

for i in 0 ..< n:               # write data to data file
  for j in 0 ..< m:
    if j > 0: stdout.write ','
    stdout.write randState.gauss(j.float, float(j + 1))
  stdout.write '\n'

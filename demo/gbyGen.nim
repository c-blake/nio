import std/[os, osproc, random, strutils, private/digitsutils],
       cligen/[strUt, osUt, macUt]

proc irng(r: var Rand, lim: int): int =             # slightly biased but much
  1 + int(r.rand(1.0)*lim.float)                    #..faster than rand(lim).

type Arg = tuple[path: string; i,nT,NdT,NdK,K: int; nas: float; rH: bool]
proc work(a: Arg) {.thread.} =                      # Slow & independent => MT
  let pfx = if a.nT != 1: align($a.i, 3, '0') & "-" else: ""
  var r = initRand(100 + a.i)                       # Each thread has own RNG
  let f = open(pfx & a.path, fmWrite)               #..and path.
  if a.i == 0 or a.rH:
    f.write "id1,id2,id3,id4,id5,id6,v1,v2,v3\n"    # write CSV header

  template wr(s: string) = f.urite s                # blast string to file
  let zeros = "00000000000"                         # pad w/n zeros
  template zs(n: int) = discard f.uriteBuffer(zeros[0].unsafeAddr, n)
  template na(x) =                                  # maybe replace x with NA
    if a.nas > 0.0 and rand(1.0)*100.0 < a.nas: discard else: x
  template p0(n: int, id: untyped) =                # id w/0 padding to n dig
    na: wr "id"; zs n - id.len; wr id
  var i1, i2, i3, i4, i5, i6, v1, v2, v3: string
  for i in 0 ..< a.NdT:
    callsOn setLen, [i1, i2, i3, i4, i5, i6, v1, v2, v3], 0
    i1.addInt r.irng(a.K)     ; p0( 3, i1); wr ","  # large groups (char)
    i2.addInt r.irng(a.K)     ; p0( 3, i2); wr ","  # large groups (char)
    i3.addInt r.irng(a.NdK)   ; p0(10, i3); wr ","  # small groups (char)
    i4.addInt r.irng(a.K)     ; na(wr i4) ; wr ","  # large groups (int)
    i5.addInt r.irng(a.K)     ; na(wr i5) ; wr ","  # large groups (int)
    i6.addInt r.irng(a.NdK)   ; na(wr i6) ; wr ","  # small groups (int)
    v1.addInt r.irng(5)       ; na(wr v1) ; wr ","  # int in range [1,5]
    v2.addInt r.irng(5)       ; na(wr v2) ; wr ","  # int in range [1,5]
    v3.fcvt   r.rand(100.0), 6; na(wr v3) ; wr "\n" # numeric e.g. 23.5749
  f.close

template av(i): untyped = paramStr(i)               # validate args/dump usage
if paramCount() < 5:
  quit "Usage:\n\t" & av(0) & " N K %NAs nT replicateHdr\nIf you use" &
       "nT!=1 and do not wish to stay separate, cat 0* >G1 to merge.\n" &
       "Use sort(1) to pre-sort. (Keep _0.csv code for compatibility.)\n"
let N   = int(parseFloat(av(1)))                    # total num rows
let K   = int(parseFloat(av(2)))                    # num small groups
let nas = parseFloat(av(3))                         # pctage of NA
let nT0 = parseInt(av(4))                           # num.Threads
let nT  = if nT0 != 0: nT0 else: countProcessors() - countProcessors() div 8
let rH  = parseInt(av(5)) != 0                      # replicate Header
echo "Producing data of ", av(1), " rows and ", av(2), " K groups factors"
let path = "G1_" & av(1) & "_" & av(2) & "_" & av(3) & "_0.csv"
var t = newSeq[Thread[Arg]](nT)
for i in 0..<nT: createThread t[i], work,(path,i,nT,N div nT,N div K,K,nas,rH)
joinThreads t

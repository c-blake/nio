## Moby Thesaurus has a slow (350..500 ms) NPM prog to CL query.  Shipped data
## is: word,synonym,..(no antonyms).  To enable rapid query, this prog compiles
## said format to binary files: length-prefixed uniques (.LC), length-prefixed
## lists of refs (.NI), and a hash mapping the former to latter (.NII).  Use is
## just: wget github.com/words/moby/raw/master/words.txt; thes b; thes s ajar

import std/[tables, os, math, times], cligen/[mfile, mslice, tab]
from terminal import terminalWidth; let ttyWidth = terminalWidth()

template pua(T: typedesc): untyped = ptr UncheckedArray[T]
proc `+%`(p: pointer, i: int): pointer = cast[pointer](cast[int](p) +% i)

type # Core code is just the 99 lines from here to the end of `reduced`.
  TabEnt {.packed.} = object
    wdN {.bitsize:  8.}: uint8          # length of key cached in table slot
    wdR {.bitsize: 24.}: uint32         # ref(byte offset) into uniq
    synsR: uint32                       # ref(byte offset) into lists
  Thes* = object
    mode: FileMode                      # object mode (fmRead|fmReadWrite)
    tabM, uniM, synM: MFile             # Table, Unique, Synonym memory files
    tabSz: int                          # Size of Table (in TabEnt units)
    tab: pua TabEnt                     # Table in mapped memory
    synF: File                          # open file handle (only for build)

proc word(th: Thes, i: uint32): MSlice =
  result.len = cast[ptr uint8](th.uniM.mem +% i.int)[].int
  result.mem = th.uniM.mem +% (i.int + 1)

proc find(th: Thes, w: MSlice): int =   # Vanilla linear probe hash search
  let mask = th.tabSz - 1 #NOTE: Moby+256kB L2 CPU params =>15,17bit hash,wdR
  let n = w.len.csize_t   # ..+RobinHood+parallel KV arrays likely ~2X faster.
  var i = w.hash and mask               # Initial probe
  while (let j = th.tab[i].wdR.int; j != 0):
    if th.tab[i].wdN.int==n.int and c_memcmp(th.uniM.mem +% (j+1), w.mem, n)==0:
      return i                          # Len & *only* then bytes equal => Found
    i = (i + 1) and mask                # The linear part of linear probing
  return -i - 1                         # Not Found, return -(insertion point)-1

proc topen*(base: string, mode=fmRead, n=32768): Thes =
  if mode == fmRead:
    result.uniM = mopen(base & ".LC")
    result.synM = mopen(base & ".NI")
    result.tabM = mopen(base & ".NII")
  else:
    try: removeFile base&".LC"; removeFile base&".NI"; removeFile base&".NII"
    except: discard
    result.uniM = mopen(base & ".LC", PROT_READ or PROT_WRITE, b=65536,allowRemap=true)
    result.synF = open( base & ".NI", fmWrite)
    result.tabM = mopen(base & ".NII",PROT_READ or PROT_WRITE,b=n*TabEnt.sizeof)
  result.tab   = cast[pua TabEnt](result.tabM.mem)
  result.tabSz = result.tabM.len div TabEnt.sizeof

proc close*(th: Thes) =
  th.uniM.close; th.synM.close; th.tabM.close
  if not th.synF.isNil: th.synF.close

proc build*(bpsl=821.0, time=false, input: seq[string]): int =
  ## Build binary files that can be rapidly queried
  let t0 = epochTime()
  if input.len != 1: raise newException(ValueError, "build expects 1 path")
  let (dir, name, _) = input[0].splitFile
  let base = dir/name

  template oGet(o, k, uniq, uniO, uniM): untyped =
    try: o = uniq[k]            # get or add an offset from an MSlice
    except: o=uniO; uniq[k]=o; uniM.add chr(k.len.uint8), uniO; uniM.add k, uniO

  if (let mf = mopen(input[0]); mf != nil):
    let n    = nextPowerOfTwo(int(mf.len.float / bpsl + 1.0))
    var th   = topen(base, fmWrite, n)
    var uniq = initTable[MSlice, uint32](110_000)
    var uniO, wO, synO, synsO: uint32
    th.uniM.add chr(0u8), uniO # => all offs > 0; So 0 encodes missing in hash
    var w: MSlice
    for line in mf.mSlices:
      var line = line
      if line.nextSlice(w, ',') < 1: continue
      oGet(wO, w, uniq, uniO, th.uniM)
      var syns: seq[uint32]                  #NOTE: Hash-order for inSynonyms..
      for syn in line.mSlices(','):          #.. with a later alpha readability
        oGet(synO, syn, uniq, uniO, th.uniM) #.. sort is faster, BUT fastest of
        syns.add synO                        #.. all is saving the 2nd syn list.
      let i = -th.find(w) - 1       # Lookups must fail for correct Moby inputs
      if i < 0: raise newException(ValueError, "build using stale files")
      th.tab[i].wdN   = w.len.uint8
      th.tab[i].wdR   = wO
      th.tab[i].synsR = synsO
      let synLen = syns.len.uint32
      discard th.synF.writeBuffer(synLen.addr, 4)
      discard th.synF.writeBuffer(syns[0].addr, 4*syns.len)
      synsO += 4*(syns.len.uint32 + 1)
    discard th.uniM.resize uniO.int; th.close; mf.close
  else: stderr.write "Cannot open|mmap \"", input[0], "\"\n"; return 1
  if time: stderr.write epochTime() - t0, " seconds\n"

iterator synos*(th: Thes, ws: MSlice, wn: ptr uint32 = nil): uint32 =
  if (let i = th.find(ws); i >= 0):
    if not wn.isNil: wn[] = th.tab[i].wdR
    let syns = cast[pua uint32](th.synM.mem +% th.tab[i].synsR.int)
    for j in 1u32..syns[0]: yield syns[j]
  elif not wn.isNil: wn[] = 0

proc inSynonyms*(th: Thes, ss: MSlice, wn: uint32): bool =
  for rn in th.synos(ss):       # This linear scan can be slow; So be sure to
    if rn == wn: return true    #..do integer compares, not string compares.

iterator reduced*(th: Thes, ws: MSlice): MSlice =
  var wn: uint32                    # Could share old lookup; Do it again for..
  for sn in th.synos(ws, wn.addr):  # more autonomous/fair timing, not "best".
    let ss = th.word(sn)
    if th.inSynonyms(ss, wn): yield ss

proc format(strs: seq[string]) =
  var wids: seq[int]; for s in strs: wids.add -s.len
  var nrow, ncol: int
  var colWs = layout(wids, ttyWidth - 2, gap=1, 999, 1, nrow, ncol)
  colPad(colWs, ttyWidth - 2, 999, 1)
  stdout.write(strs, wids, colWs, 1, nrow, ncol, 0, "  ")

proc synon*(base="words", also=false, time=false, words: seq[string]) =
  ## Synonyms for given words|phrases; Maybe symmetrized suggestions.
  var t0: float
  let th = topen(base)
  for w in words:
    stdout.write w, '\n'
    if time: t0 = epochTime()
    let ws = w.toMSlice
    var strs: seq[string]
    for sn in th.synos(ws): strs.add $th.word(sn)
    if time: stderr.write epochTime() - t0, " seconds\n"
    strs.format
    if also:
      strs.setLen 0
      if time: t0 = epochTime()
      for s in th.reduced(ws): strs.add $s
      if time: stderr.write epochTime() - t0, " seconds\n"
      stdout.write "See also:\n"
      strs.format
  th.close

when isMainModule:
  import cligen
  dispatchMulti [build, help={"bpsl": "bytes/synonym list (to size table)"}],
                [synon, help={"base": "prefix of .LC, .NI, .NII",
                              "also": "show symmetry imposed 'see also'"}]

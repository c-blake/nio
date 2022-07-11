## Moby Thesaurus has a slow (350..500 ms) NPM prog to CL query.  Shipped data
## is: word,synonym,..(no antonyms).  To enable rapid query, this prog compiles
## said format to various binary files inspectable with `nio`.  Use is just:
## wget github.com/words/moby/raw/master/words.txt; thes b; thes s ajar

import std/[tables, os, math, times], cligen/[mfile, mslice, tab]
from terminal import terminalWidth; let ttyWidth = terminalWidth()

template pua(T: typedesc): untyped = ptr UncheckedArray[T]
proc `+%`(p: pointer, i: int): pointer = cast[pointer](cast[int](p) +% i)

type # Core code is just the 99 lines from here to the end of `reduced`.
  TabEnt {.packed.} = object
    wdN {.bitsize:  8.}: uint8          # length of key cached in table slot
    wdR {.bitsize: 24.}: uint32         # ref(byte offset) into uniq
  #NOTE: synM shares the same indexing as the table.  It is separated to lessen
  #      size pressure on the random access/hash portion of the calculation.
  Thes* = object
    mode: FileMode                      # object mode (fmRead|fmReadWrite)
    base: string
    tabM, uniM, synM, synRM, alsoM: MFile # MemFiles: Tab, Uniq, Syn, Lists&Also
    tabSz: int                          # Size of Table (in TabEnt units)
    tab: pua TabEnt                     # Table in mapped memory
    synF, alsoF: File                   # open file handles (only for build)

proc word(th: Thes, i: uint32): MSlice =
  result.len = cast[ptr uint8](th.uniM.mem +% i.int)[].int
  result.mem = th.uniM.mem +% (i.int + 1)

#var probes = 0; var worst = 0
proc find(th: Thes, w: MSlice): int =   # Vanilla linear probe hash search
  let mask = th.tabSz - 1
  let n = w.len.csize_t
  var i = w.hash and mask               # Initial probe
#  let probes0 = probes
  while (let j = th.tab[i].wdR.int; j != 0):
#    inc probes
    if th.tab[i].wdN.int==n.int and c_memcmp(th.uniM.mem +% (j+1), w.mem, n)==0:
#      worst = max(worst, probes - probes0)
      return i                          # Len & *only* then bytes equal => Found
    i = (i + 1) and mask                # The linear part of linear probing
  return -i - 1                         # Not Found, return -(insertion point)-1

proc topen*(base: string, mode=fmRead, n=32768): Thes =
  result.base = base
  if mode == fmRead:
    result.uniM  = mopen(base & ".LC")
    result.tabM  = mopen(base & ".NI")
    result.synM  = mopen(base & "_s.NI")
    result.synRM = mopen(base & "_sr.NI")
    result.alsoM = mopen(base & "_ar.NI")
  else:
    try:
      removeFile base&".LC"   ; removeFile base&".NI"; removeFile base&"_s.NI"
      removeFile base&"_sr.NI"; removeFile base&"_ar.NI"
    except: discard                     # Ensure we start empty
    result.uniM  = mopen(base & ".LC"   , PROT_READ or PROT_WRITE,
                         b=65536, allowRemap=true) # Resized as we go
    result.tabM  = mopen(base & ".NI"   , PROT_READ or PROT_WRITE, b=4*n)
    result.synF  = open( base & "_s.NI" , fmWrite)
    result.synRM = mopen(base & "_sr.NI", PROT_READ or PROT_WRITE, b=4*n)
    result.alsoF = open( base & "_ar.NI", fmWrite)
  result.tab   = cast[pua TabEnt](result.tabM.mem)
  result.tabSz = result.tabM.len div TabEnt.sizeof

proc close*(th: Thes) =
  th.uniM.close; th.tabM.close; th.synM.close; th.synRM.close; th.alsoM.close
  if not th.synF.isNil: th.synF.close
  if not th.alsoF.isNil: th.alsoF.close

var gAl: pua uint32
iterator synos*(th: Thes, ws: MSlice, wn: ptr uint32 = nil,
                al: var pua uint32 = gAl): uint32 =
  if (let i = th.find(ws); i >= 0):
    if not wn.isNil: wn[] = th.tab[i].wdR
    let synsR = cast[pua uint32](th.synRM.mem)[i.int]
    let syns = cast[pua uint32](th.synM.mem +% synsR.int)
    for j in 1u32..syns[0]: yield syns[j]
    al = cast[pua uint32](th.alsoM.mem +% syns[syns[0]+1].int)
  elif not wn.isNil: wn[] = 0

iterator alsos*(al: pua uint32): uint32 =
  for j in 1u32..al[0]: yield al[j]

proc count(th: Thes, ws: MSlice, al: var pua uint32 = gAl): int =
  if (let i = th.find(ws); i >= 0):
    let synsR = cast[pua uint32](th.synRM.mem)[i.int]
    let syns = cast[pua uint32](th.synM.mem +% synsR.int)
    al = cast[pua uint32](th.alsoM.mem +% syns[syns[0]+1].int)
    return syns[0].int

proc inSynonyms(th: Thes, ss: MSlice, wn: uint32): bool =
  for rn in th.synos(ss):       # This linear scan can be slow; So be sure to
    if rn == wn: return true    #..do integer compares, not string compares.

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
    var uniO, wO, synO, synsO, alsoO: uint32
    th.uniM.add chr(0u8), uniO # => all offs > 0; So 0 encodes missing in hash
    var w: MSlice
    for line in mf.mSlices:
      var line = line
      if line.nextSlice(w, ',') < 1: continue
      if w.len > 255: stderr.write "ignoring overlong word\n"; continue
      oGet(wO, w, uniq, uniO, th.uniM)
      var syns: seq[uint32]                  #NOTE: Hash-order for inSynonyms..
      for syn in line.mSlices(','):
        if syn.len > 255: stderr.write "ignoring overlong synonym\n"; continue
        oGet(synO, syn, uniq, uniO, th.uniM)
        syns.add synO
      let i = -th.find(w) - 1       # Lookups must fail for correct Moby inputs
      if i < 0: raise newException(ValueError, "build using stale files")
      th.tab[i].wdN = w.len.uint8
      th.tab[i].wdR = wO
      cast[pua uint32](th.synRM.mem)[i] = synsO
      var synLen = syns.len.uint32
      discard th.synF.writeBuffer(synLen.addr, 4) # +1 length
      discard th.synF.writeBuffer(syns[0].addr, 4*syns.len)
      discard th.synF.writeBuffer(synLen.addr, 4) # +1 "see also" place holder
      synsO += 4*(synLen + 2)
    discard th.uniM.resize uniO.int     # Finalize size of uniq words
    th.synF.close; th.synF = nil        # Close synF & make th fully operational
    th.synM = mopen(th.base & "_s.NI", PROT_READ or PROT_WRITE)
    for i in 0 ..< th.tabM.len div 4:   # Build alsoM; point there with syns[^1]
      if th.tab[i].wdR != 0:
        var ws = th.word(th.tab[i].wdR)
        var wn: uint32
        var also: seq[uint32]
        for sn in th.synos(ws, wn.addr):
          let ss = th.word(sn)
          if th.inSynonyms(ss, wn): also.add sn
        var alsoLen = also.len.uint32
        discard th.alsoF.writeBuffer(alsoLen.addr, 4)
        if alsoLen > 0:
          discard th.alsoF.writeBuffer(also[0].addr, 4*also.len)
        let synsR = cast[pua uint32](th.synRM.mem)[i.int]
        let syns = cast[pua uint32](th.synM.mem +% synsR.int)
        syns[syns[0]+1] = alsoO
        alsoO += 4*(alsoLen + 1)
    mf.close; th.close                  # close input,output
  else: stderr.write "Cannot open|mmap \"", input[0], "\"\n"; return 1
  if time: stderr.write epochTime() - t0, " seconds\n"

proc format(strs: seq[string]) =
  var wids: seq[int]; for s in strs: wids.add -s.len
  var nrow, ncol: int
  var colWs = layout(wids, ttyWidth - 2, gap=1, 999, 1, nrow, ncol)
  colPad(colWs, ttyWidth - 2, 999, 1)
  stdout.write(strs, wids, colWs, 1, nrow, ncol, 0, "  ")

proc synon*(base="words", also=false, time=false, words: seq[string]) =
  ## Synonyms for given words|phrases; Maybe symmetrized suggestions.
  ##
  ## When no `words` are given, act as flushing stdin-stdout "server".
  var t0: float
  let th = topen(base)
  template doWord(w) =
    stdout.write "Word: ", w, '\n'
    if time: t0 = epochTime()
    let ws = w.toMSlice
    var strs: seq[string]
    var al: pua uint32
    for sn in th.synos(ws, nil, al): strs.add $th.word(sn)
    if time: stderr.write epochTime() - t0, " seconds\n"
    strs.format
    if also and strs.len > 0:
      strs.setLen 0
      if time: t0 = epochTime()
      for sn in alsos(al): strs.add $th.word(sn)
      if time: stderr.write epochTime() - t0, " seconds\n"
      stdout.write "See also:\n"
      strs.format
    if words.len == 0: flushFile stdout
  if words.len == 0: (for w in stdin.lines: doWord(w))
  else: (for w in words: doWord(w))
  th.close

proc cnt*(base="words", also=false, time=false, words: seq[string]) =
  ## Just like synon but merely count synonyms
  var t0: float
  let th = topen(base)
  template doWord(w) =
    stdout.write "Word: ", w, ": "
    if time: t0 = epochTime()
    let ws = w.toMSlice
    var al: pua uint32
    var cnt = th.count(ws, al)
    if time: stderr.write epochTime() - t0, " seconds\n"
    if also and cnt > 0:
      if time: t0 = epochTime()
      inc cnt, al[0].int
      if time: stderr.write epochTime() - t0, " seconds\n"
    echo cnt
    if words.len == 0: flushFile stdout
  if words.len == 0: (for w in stdin.lines: doWord(w))
  else: (for w in words: doWord(w))
  th.close
# stderr.write "total probes: ", probes, " worst case: ", worst, "\n"

when isMainModule:
  import cligen; include cligen/mergeCfgEnvMulti
  dispatchMulti [build, help={"bpsl": "bytes/synonym list (to size table)"}],
                [synon, help={"base": "prefix of .LC, .NI, ..",
                              "also": "show symmetry imposed 'see also'"}],
                [cnt  , help={"base": "prefix of .LC, .NI, ..",
                              "also": "show symmetry imposed 'see also'"}]

# Some observations.  diff -uw thes*.nim shows the amendments we had to make to
# save the whole answer which takes another 6 MiB for Moby (still < text file).
#
# Instrumentation w/stdlib Murmur hash shows for 32768 avg=6.7, max=607 probes,
# and for 65536, avg=1.43 & max=17.  Even 607 is not so awful, since only a
# small fraction will match word lengths.  In terms of unsuccessful search perf,
# nio p words.NI|g -n '^0$'|sed|awk has the top 5 probe sizes: 431 479 480 626
# 1088 => worst-worst is still ~1*4k page in the 128k scenario or 22 22 24 27 36
# in 256k scenario.  I couldn't measure significant dt at q=.95 timing finding
# all present keys between the 6.7 & 1.43 cases (though that ratio is 4.7X!).
# It is noticeable in file build time (503ms vs 361ms, but that's still ~1.4x).
# Similar observations old using my wyHash impl.  So, they likely generalize.
#
# For a thesaurus, most lookups are (probably) for present keys. So, while it's
# possible & might be pedagogical (but see also adix/lptabz), it's likely not
# worth Robin-Hood re-org.  That usually pays off to speed up missing searches
# and/or reduce variance.  In this case, to not cost the same space as a sparser
# 256k table (which has fine maxes), it would also add a new file.  30259 lists
# w/103307 uniq words allows 15,17bit hash,wordNum; 128k = 92%Full operation
# which is likely fine for RH.  To squeeze into 17bit, we need a file to resolve
# wordNum.  It's also frail to lists size being >15 bits (but see adix/bltab).
#
# So, if you worry about worst cases, a sparser 256k table approach is likely ok
# here.  Intel's moving to 512k L2 cache w/Ice Lake anyway&this is already fast.
#
# BTW, `wc words.txt` gives you enough to figure out average bytes/synonym list
# and control table size, if you have some other source of synonym data.  While
# we could grow dynamically, it's unlikely that statistics of synonym lists are
# very dynamic, you could always toss a -b in a config file, and, really builds
# are not likely to happen often.

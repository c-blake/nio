## This is a system for managing pre-parsed N)umeric|N)ative binary data.  File
## names have extensions with metadata needed to interpret data as typed rows.
## Syntax is *".N"* then 1|more *{COUNT{,COUNT..}}[cCsSiIlLfdg]* specs where
## **Uppercase => Signed** & letter=first letter of C type except g=lonG double.
## No count => 1.  *'_'* is ignored.  An empty string before ".N" gives *stdin*
## row format if needed. E.g., a packed *struct{ short id,day; float price; }*
## <~~> *"idDayPrice.N2sf"*. N/A = NaN|signed.low|unsigned.high.
const fmtUse* = "\nSyntax: ({COUNT{,COUNT...}}[cCsSiIlLfdg])+\n"

import strutils, math, os, strformat {.all.}, # only for proc formatInt
       tables, sets, system.ansi_C, cligen/[osUt, strUt, fileUt]
from memfiles as mf import nil

type #*** BASIC TYPE SETUP  #NOTE: gcc __float128 CPU-portable but slow
  IOKind* = enum cIk = "int8" , CIk = "uint8" , sIk = "int16", SIk = "uint16",
                 iIk = "int32", IIk = "uint32", lIk = "int64", LIk = "uint64",
                 fIk = "float32", dIk = "float32", gIk = "float80"

  IOCol* = object       ## column metadata
    iok*:   IOKind      ## number kind
    cnts*:  seq[int]    ## repetition counts for each dimension
    width*: int         ## cached cnts.prod: total width of a subcol

  IORow* = object       ## row metadata
    bytes*: int         ## width of a whole row buffer in bytes
    cols*:  seq[IOCol]  ## specs for each column

  NFile* = object
    mode:    FileMode   # fm(Read|Write|ReadWrite|ReadWriteExisting|Append)
    m*:      mf.MemFile ## for memory mapped IO
    f*:      File       ## for C stdio streams (e.g. pipes)
    rowFmt*: IORow      # row format for IO
    off,j,k: int        # next byte offset into `mf`; next col,subcol in row

  float80* {.importc: "long double".} = object  ## C backend long double type
  IONumber* = SomeNumber|float80                ## all IO numbers type class
  Strings* = seq[string]                        ## alias for seq[string]

proc low*(T: typedesc[float80]): float80 = float64.low    # float80 support
proc high*(T: typedesc[float80]): float80 = float64.high
converter toFloat80*(pdqr: SomeNumber): float80 = {.emit: "result = pdqr;".}
template defc(T) {.dirty.} = # need dirty to avoid genSym so emit can work
  converter `to T`*(xyzw: float80): T = {.emit: "result = xyzw;".}
defc(uint8);defc(uint16);defc(uint32);defc(uint64); defc(float32)
defc( int8);defc( int16);defc( int32);defc( int64); defc(float64)

const ioCode*: array[IOKind, char] = [ 'c','C', 's','S', 'i','I', 'l','L',
                                       'f','d','g' ]              ## type codes
const ioSize*: array[IOKind, int] = [1,1, 2,2, 4,4, 8,8, 4,8,16]  ## type sizes
const ioFloats* = {fIk, dIk, gIk}                                 ## float kinds

proc ioCodeK*(c: char): IOKind {.inline.} =
  ## return IOKind correspnding to character `c`
  let ix = ioCode.find(c)
  if ix < 0: raise newException(ValueError, "expecting [cCsSiIlLfdg]")
  IOKind(ix)

proc codeOf*(x: IONumber): IOKind =
  ## return NIO code from a static Nim type
  when x is  int8  : result = cIk
  elif x is uint8  : result = CIk
  elif x is  int16 : result = sIk
  elif x is uint16 : result = SIk
  elif x is  int32 : result = iIk
  elif x is uint32 : result = IIk
  elif x is  int64 : result = lIk
  elif x is uint64 : result = LIk
  elif x is float32: result = fIk
  elif x is float64: result = dIk
  elif x is float80: result = gIk

proc isSigned*  (k: IOKind): bool {.inline.} = (k.int and 1) == 0
proc isUnSigned*(k: IOKind): bool {.inline.} = (k.int and 1) == 1
proc isFloat*   (k: IOKind): bool {.inline.} = k.int > LIk.int

#*** MISSING VALUE CONVENTION; NA-AWARE LOW/HIGH MOSTLY FOR CLIPPING PARSED DATA
const c_na* =  int8.low;const c_low* = int64( int8.low+1);const c_high* = int64( int8.high)
const s_na* = int16.low;const s_low* = int64(int16.low+1);const s_high* = int64(int16.high)
const i_na* = int32.low;const i_low* = int64(int32.low+1);const i_high* = int64(int32.high)
const l_na* = int64.low;const l_low* = int64.low+1       ;const l_high* = int64.high
const C_na* =  uint8.high;const C_low* = 0'u64;const C_high* = uint64( uint8.high-1)
const S_na* = uint16.high;const S_low* = 0'u64;const S_high* = uint64(uint16.high-1)
const I_na* = uint32.high;const I_low* = 0'u64;const I_high* = uint64(uint32.high-1)
const L_na* = uint64.high;const L_low* = 0'u64;const L_high* = uint64.high-1
const f_na* = float32(NaN); const f_low* = float32.low; const f_high* = float32.high #-+inf
const d_na* = float64(NaN); const d_low* = float64.low; const d_high* = float64.high #-+inf
const g_na* = float80(NaN); const g_low* = float80.low; const g_high* = float80.high #-+inf
const lowS*  = [c_low , s_low , i_low , l_low ] # [k.int shr 1] post-isSigned
const highS* = [c_high, s_high, i_high, l_high]
const lowU*  = [C_low , S_low , I_low , L_low ] # [k.int shr 1] post-isUnsigned
const highU* = [C_high, S_high, I_high, L_high]
const lowF*  = [float64.low , float64.low , float64.low ] # [k.int-8] post-isFlt
const highF* = [float64.high, float64.high, float64.high]

template withTyped_P_NA(k, adr, p, na, body) =
  case k
  of cIk: (let p = cast[ptr  int8  ](adr); let na{.used.} = c_na; body)
  of CIk: (let p = cast[ptr uint8  ](adr); let na{.used.} = C_na; body)
  of sIk: (let p = cast[ptr  int16 ](adr); let na{.used.} = s_na; body)
  of SIk: (let p = cast[ptr uint16 ](adr); let na{.used.} = S_na; body)
  of iIk: (let p = cast[ptr  int32 ](adr); let na{.used.} = i_na; body)
  of IIk: (let p = cast[ptr uint32 ](adr); let na{.used.} = I_na; body)
  of lIk: (let p = cast[ptr  int64 ](adr); let na{.used.} = l_na; body)
  of LIk: (let p = cast[ptr uint64 ](adr); let na{.used.} = L_na; body)
  of fIk: (let p = cast[ptr float32](adr); let na{.used.} = f_na; body)
  of dIk: (let p = cast[ptr float64](adr); let na{.used.} = d_na; body)
  of gIk: (let p = cast[ptr float80](adr); let na{.used.} = g_na; body)

proc isNA*(k: IOKind, adr: pointer): bool {.inline.} =
  ## Test the number IO kind at `adr` against its missing/NA value
  withTyped_P_NA(k, adr, p, na):
    when declared(isnan): return if k > LIk: p[].float64.isnan else: p[] == na
    else: return if k > LIk: p[].float.classify == fcNan else: p[] == na

proc setNA*(k: IOKind, adr: pointer) {.inline.} =
  ## Set the number IO kind at `adr` to its missing/NA value
  withTyped_P_NA(k, adr, p, na): p[] = na

proc convert*(dk, sk: IOKind, da, sa: pointer, naCvt=false) {.inline.} =
  ## From-any to-any convertor, optionally translating NAs
  if naCvt and sk.isNA(sa):
    dk.setNA(da)
    return
  case dk
  of cIk: withTyped_P_NA(sk, sa, p, na, (cast[ptr  int8  ](da)[] = p[].int8   ))
  of CIk: withTyped_P_NA(sk, sa, p, na, (cast[ptr uint8  ](da)[] = p[].uint8  ))
  of sIk: withTyped_P_NA(sk, sa, p, na, (cast[ptr  int16 ](da)[] = p[].int16  ))
  of SIk: withTyped_P_NA(sk, sa, p, na, (cast[ptr uint16 ](da)[] = p[].uint16 ))
  of iIk: withTyped_P_NA(sk, sa, p, na, (cast[ptr  int32 ](da)[] = p[].int32  ))
  of IIk: withTyped_P_NA(sk, sa, p, na, (cast[ptr uint32 ](da)[] = p[].uint32 ))
  of lIk: withTyped_P_NA(sk, sa, p, na, (cast[ptr  int64 ](da)[] = p[].int64  ))
  of LIk: withTyped_P_NA(sk, sa, p, na, (cast[ptr uint64 ](da)[] = p[].uint64 ))
  of fIk: withTyped_P_NA(sk, sa, p, na, (cast[ptr float32](da)[] = p[].float32))
  of dIk: withTyped_P_NA(sk, sa, p, na, (cast[ptr float64](da)[] = p[].float64))
  of gIk: withTyped_P_NA(sk, sa, p, na, (cast[ptr float80](da)[] = p[]        ))

#*** METADATA ACQUISITION SECTION
proc initIORow*(fmt: string, endAt: var int): IORow =
  ## Parse NIO format string `fmt` into an `IORow`
  if fmt.len < 1: raise newException(IOError, "empty row format")
  var col: IOCol
  col.cnts.add 0
  endAt = fmt.len - 1
  for i, c in fmt:
    case c:
    of '_': discard                     # allow optional spacing stuff out
    of '0'..'9': col.cnts[^1] *= 10; col.cnts[^1] += ord(c) - ord('0')
#Explicit "0+" -> raise newException(IOError, "0 dim in type fmt: "&fmt&fmtUse)
    of ',': col.cnts.add 0              # make room for next dimension
    of 'c', 'C', 's', 'S', 'i', 'I', 'l', 'L', 'f', 'd', 'g':
      col.iok = fmt[i].ioCodeK
      for i in 0 ..< col.cnts.len:      # All 0s -> 1s
        if col.cnts[i] == 0: col.cnts[0] = 1
      col.width = col.cnts.prod
      result.bytes += col.width * ioSize[col.iok]
      result.cols.add col
      col.cnts = @[0]
    else:
      if c in {'a'..'z', 'A'..'Z'}:
        raise newException(IOError, &"bad code {c} in fmt: " & fmt & fmtUse)
      else: # allow things like '@'.* after format suffix
        endAt = i
        return
  if result.cols.len == 0:
    raise newException(IOError, "no columns in fmt: " & fmt & fmtUse)

proc dotContents(path: string): string =
  for line in lines(path): result.add line.commentStrip

proc metaData(path: string): (string, IORow, string) =
  var endAt: int
  var (dir, name, ext) = splitPathName(path)
  let fmt = if ext.startsWith(".N"): ext[2 .. ^1] else:
              try: dotContents(dir / "." & name) except:
                raise newException(ValueError,
                                   path & ": has no .N extension | dotfile")
  result[1] = fmt.initIORow(endAt)
  if endAt >= fmt.len - 1:
    result[0] = if name.len == 0: "" else: path
  else:
    result[0] = if ext.startsWith(".N"): path[0 ..< path.len - fmt.len + endAt]
                elif name.len == 0: "" else: path
    result[2] = fmt[endAt..^1]

#*** FILE IO SECTION
# fmRead       RDONLY          fmWrite  CREAT|TRUNC
# fmReadWrite  RW|CREAT|TRUNC  fmAppend WRONLY|APPEND  fmReadWriteExisting RW
proc nOpen*(path: string, mode=fmRead, newFileSize = -1, allowRemap=false,
            mapFlags=cint(-1), rest: ptr string=nil): NFile =
  let (path, fmt, rst) = metaData(path)
  if fmt.bytes == 0: raise newException(ValueError, &"{path}: some ext problem")
  result.rowFmt = fmt
  result.mode = mode
  if path.len == 0:
    case mode
    of fmRead: result.f = stdin
    of fmAppend, fmWrite: result.f = stdout
    else: raise newException(ValueError, "nameless unsupported by " & $mode)
  else:
    try:
      result.m = mf.open(path, mode, newFileSize=newFileSize,
                         allowRemap=allowRemap, mapFlags=mapFlags)
    except:
      result.f = open(path, mode, max(8192, result.rowFmt.bytes))
#Q: Parse {[a]:[b]} row slice fmt here & adjust start,len? headTail filtering?
#   or just rely on accessing language being able to range/slice?
  if rest != nil: rest[] = rst

proc close*(nf: var NFile) =
  if not nf.m.mem.isNil: mf.close nf.m
  if not nf.f.isNil: close nf.f

proc read*(nf: var NFile, buf: var string, sz=0): bool =
  ## Read `sz` bytes (a whole row by default); Returns false on short read/EOF.
  ## `while nf.read(buf): ..`
  let sz = if sz > 0: sz else: nf.rowFmt.bytes
  buf.setLen sz
  if not nf.f.isNil:
    result = nf.f.readBuffer(buf[0].addr, sz) == sz:
  elif not nf.m.mem.isNil:
    result = nf.off + sz <= nf.m.size
    if result:
      copyMem buf[0].addr, cast[pointer](cast[int](nf.m.mem) +% nf.off), sz
      nf.off += sz

proc read*(nf: var NFile, kind: var IOKind, s: var string): bool =
  ## Fill `s` with the next number, returning false at EOF & IOKind in `kind`.
  kind = nf.rowFmt.cols[nf.j].iok
  if nf.read(s, ioSize[kind]):
    result = true
    nf.k.inc
    if nf.k == nf.rowFmt.cols[nf.j].width:
      nf.k = 0
      nf.j.inc
      if nf.j == nf.rowFmt.cols.len:
        nf.j = 0

var sBuf = newString(16)                # Nim includes an extra byte for '\0'
template defr(T) =
  proc read*(nf: var NFile, da: var T, naCvt=false): bool =
    var sk: IOKind
    if nf.read(sk, sBuf):
      convert(da.codeOf, sk, da.addr, sBuf[0].addr, naCvt)
      result = true

defr(uint8);defr(uint16);defr(uint32);defr(uint64); defr(float32)
defr( int8);defr( int16);defr( int32);defr( int64); defr(float64); defr(float80)

proc readCvt*(nf: var NFile, nums: var openArray[IONumber], naCvt=false): bool =
  ## Read next `nums.len` items, converting to buf type & maybe converting NAs.
  result = true
  for i in 0 ..< nums.len:
    if not nf.read(nums[i].addr, naCvt): return false

proc nurite*(f: File, kout: IOKind, buf: pointer) =
  ## Numeric unlocked write to stdio `f`.
  discard f.uriteBuffer(buf, ioSize[kout])

type #*** MEMORY MAPPED IO SECTION
  IOTensor* = object
    m*:   mf.MemFile  ## backing file
    t*:   IOKind      ## single IOKind, e.g. fIk for .N128,128f
    fmt*: IORow       ## parsed row format
    d*:   seq[int]    ## dimensions

proc mOpen*(tsr: var IOTensor, path: string, mode=fmRead, mappedSize = -1,
            offset=0, newFileSize = -1, allowRemap=false, mapFlags=cint(-1)) =
  let (path, fmt, _) = metaData(path) #XXX match any filled in IOTensor fields
  tsr.fmt = fmt                       #.. add DATA_PATH search; also for nOpen
  tsr.t = fmt.cols[0].iok             #.. add indexing/slicing ops or maybe..
  if fmt.cols.len != 1:               #.. just numnim/neo/Arraymancer compat.
    raise newException(ValueError, &"{path}: impure tensors unsupported")
  tsr.m = mf.open(path)
  if tsr.m.size mod fmt.bytes != 0:
    mf.close(tsr.m)
    raise newException(ValueError, &"{path}: file size indivisible by row size")
  tsr.d = @[ tsr.m.size div fmt.bytes ] & fmt.cols[0].cnts

proc close*(tsr: var IOTensor) = mf.close(tsr.m)

type #*** INDIRECTION SUBSYSTEM FOR FIXED OR VARIABLE-LENGTH STRING DATA
  Ix* = uint32                  # max file size for repos is 4 GiB/GiEntry
  RepoKind* = enum rkFixWid, rkDelim, rkLenPfx
  RepoMode* = enum rmFast, rmIndex, rmUpdate
  Repo* = ref object
    case kind: RepoKind
    of rkFixWid: fmt : IORow    # simplest random access format: just a matrix
    of rkDelim : dlm : char     # most popular format: delimited
    of rkLenPfx: lenK: IOKind   # best trade-off format: length-prefixed
    path: string
    m:    mf.MemFile
    na:   string                # *output* N/A; input is always ""
    mode: RepoMode
    kout: IOKind                # output pointer type for index|updating mode
    f:    File                  # updating mode only fields
    off:  Ix                    # running file size (what to set .[DL] ptrs to)
    tab*: Table[string, Ix]     # index lookup table
const IxIk = IIk
const IxNA = Ina

iterator keysAtOpen*(r: Repo): (string, Ix) =
  case r.kind
  of rkFixWid:
    var k = newString(r.fmt.bytes)
    for i in countup(0, r.m.size - 1, k.len):
      copyMem k[0].addr, cast[pointer](cast[int](r.m.mem) +% i), k.len
      yield (k, Ix(i div r.fmt.bytes))
  of rkDelim:
    for ms in mf.memSlices(r.m, r.dlm):
      yield ($ms, Ix(cast[int](ms.data) -% cast[int](r.m.mem)))
  of rkLenPfx:
    var k: string
    var off, len: uint64
    while off.int < r.m.size - 1:
      let off0 = off.Ix
      convert LIk, r.lenK, len.addr, cast[pointer](cast[uint64](r.m.mem) + off)
      off.inc ioSize[r.lenK]
      k.setLen len
      copyMem k[0].addr, cast[pointer](cast[uint64](r.m.mem) + off), len
      off += len
      yield (k, off0)

#Q: per mp, bind len,ptr Ncols for non-autonomous repos? E.g. "@repo,L,P%s".
proc rOpen*(path: string, mode=rmFast, kout=IxIk, na=""): Repo =
  ## REPOs can be byte-delimited *.Dn* | length-prefixed *.Li* with byte-offset
  ## pointer values or fixed width like *.N16c* with row index vals.  There are
  ## 3 open modes: mmap & go read-only, tab-building indexing, and full updates.
  if path.len == 0: return
  let cols = strutils.split(path, maxSplit=1)
  let path = cols[0]
  new result
  result.path = path
  if mode != rmFast:
    if cols.len > 1:                    # Schema-visible NOUP, NO_UP..
      result.mode = (if "UP" in cols[1]: rmIndex else: rmUpdate)
    else:                               # $NO_UP can switch off even w/o schema
      result.mode = if getEnv("NO_UP", "xYz") == "xYz": rmUpdate else: rmIndex
  var info: FileInfo
  try: info = getFileInfo(path)
  except: discard
  let openMode = if result.mode == rmUpdate and info.id.device == 0: fmWrite
                 else: fmAppend
  if info.size > result.off.high.int:
    raise newException(ValueError, &"{path}: too big for index pointer sizes")
  result.off = info.size.Ix
  let m = if openMode==fmWrite: mf.MemFile(mem: nil,size: 0) else: mf.open(path)
  let (_, _, ext) = splitPathName(path)
  if ext.len == 0 or ext == ".Dn":
    result = Repo(kind:rkDelim, dlm: '\n', m: m, na: na, mode: mode, kout: kout)
  elif ext.startsWith(".D"):
    try:
      let dlm = ext[2..^1]
      result = Repo(kind:rkDelim, m: m, na: na, mode: mode, kout: kout,
                    dlm: (if dlm == "n": '\n' else: chr(parseInt(dlm))))
    except:
      raise newException(ValueError, &"{path}: expecting .Dn or .D<INT>")
  elif ext.startsWith(".L") and ext.len == 3:
    result = Repo(kind:rkLenPfx, lenK: ioCodeK(ext[2]), m: m, na: na,
                  mode: mode, kout: kout)
  elif ext.startsWith(".N"):
    let (path, fmt, rst) = metaData(cols[0])
    if rst.len > 0:
      raise newException(ValueError, &"{path}: inappropriate for a repo")
    if not (fmt.cols.len == 1 and fmt.cols[0].iok in {cIk, CIk}):
      raise newException(ValueError, &"{path}: expecting .N<size>[cC]")
    result = Repo(kind:rkFixWid, fmt: fmt, m: m, na: na, mode: mode, kout: kout)
  else:
    raise newException(ValueError, &"{path}: unknown repo ext {ext}")
  if result.mode == rmUpdate and
     (result.f = mkdirOpen(path, openMode); result.f == nil):
    result.mode = rmIndex; erru &"{path}: cannot append; indexing anyway\n"
  for k, i in result.keysAtOpen: result.tab[k] = i

proc close*(at: var Repo) =
  if at.m.mem != nil: mf.close(at.m); at.m.mem = nil

proc `[]`*(at: Repo, i: Ix): string =
  if i == IxNA: return at.na
  var p: pointer
  var n: Ix
  case at.kind
  of rkFixWid:                  # Here `i` is row/record number
    n = at.fmt.bytes.Ix
    p = cast[pointer](cast[uint](at.m.mem) + i * n)
  of rkDelim:                   # Here `i` is a byte offset to start of row
    p = cast[pointer](cast[uint](at.m.mem) + i)
    let e = c_memchr(p, at.dlm.cint, csize_t(at.m.size.Ix - i))
    if e == nil:
      raise newException(ValueError, &"no terminating delimiter for {i}")
    n = Ix(cast[uint](e) - cast[uint](p))
  of rkLenPfx:                  # Here `i` is a byte offset to length field
    let pL = cast[pointer](cast[uint](at.m.mem) + i)
    convert(IxIk, at.lenK, n.addr, pL)
    p = cast[pointer](cast[uint](at.m.mem) + i + ioSize[at.lenK].Ix)
  result.setLen n
  copyMem result[0].addr, p, n

proc padClip(k: string, n: int): string {.inline.} = # FixWid key adjust
  let n0 = k.len
  if   n0 >= n: return k[0..<n]         # too long: clip key
  elif n0 < n:                          # too short: pad key
    result.setLen n
    result[0..<n0] = k                  # copyMem?
    zeroMem result[n0].addr, n - n0

proc clip(r: var Repo; k: string, lenK: IOKind, lno: int): string {.inline.} =
  if lenK.isSigned:
    if (let lim = highS[lenK.int shr 1]; k.len.int64 > lim):
      erru &"inputLine{lno}: truncating field of len {k.len}\n"
      result = k[0..<lim]
    else: result = k
  elif lenK.isUnsigned: # No NA for lens => +1 @end, does full addr space,eg 255
    if (let lim = highU[lenK.int shr 1]; k.len.uint64 > lim):
      erru &"inputLine{lno}: truncating field of len {k.len}\n"
      result = k[0..<lim]
    else: result = k
  else: raise newException(ValueError, "Length Prefix cannot be a float")

proc index*(r: var Repo, ixOut: pointer, k: string, lno: int) =
  template retNA =                      # helper template to return NA
    setNA(r.kout, ixOut); return
  if ixOut == nil: r.close; return
  if r.mode == rmFast:
    raise newException(ValueError, "rmFast mode does not do index(string)")
  if k.len == 0: retNA                  # NA key -> NA index
  var i: Ix
  let k = if   r.kind == rkFixWid: padClip(k, r.fmt.bytes)
          elif r.kind == rkLenPfx: r.clip(k, r.lenK, lno) else: k
  try: i = r.tab[k]                     # get extant index
  except:                               # novel key
    if r.mode == rmIndex: retNA         # missing && !up -> NA index
    case r.kind                         # update in-memory Table & repo
    of rkFixWid: i = r.tab.len.Ix; r.tab[k] = i; r.f.urite k; r.off = i + 1
    of rkDelim : i = r.off; r.tab[k] = i; r.f.urite k, r.dlm; r.off += Ix(k.len+1)
    of rkLenPfx:  # convert length to output type, write, then write key
      i = r.off; r.tab[k] = i
      r.off += Ix(k.len + ioSize[r.lenK])
      var n = k.len; var nbuf: array[8, char] # IO buffers for length
      convert(r.lenK, lIk, nbuf[0].addr, n.addr)
      r.f.nurite r.lenK, nbuf[0].addr
      r.f.urite k
    if r.off < i: erru &"pointer overflow for repo {r.path}\n"
  convert(r.kout, IxIk, ixOut, i.addr)  # convert pointer type

type #*** FORMAT NUMBERS TO ASCII PRIMARILY FOR DEBUGGING/SLOPPY EXPORT
  Formatter* = object
    rowFmt: IORow
    specs:  seq[StandardFormatSpecifier]
    radix:  seq[int]
    ffmode: seq[FloatFormatMode]
    ats:    seq[Repo]
    na:     string

proc radix(spec: StandardFormatSpecifier): int =
  case spec.typ                         # strformat should export this proc
  of 'x', 'X': 16
  of 'd', '\0': 10
  of 'b': 2
  of 'o': 8
  else: 0

proc ffmode(spec: StandardFormatSpecifier): FloatFormatMode =
  case spec.typ                         # strformat should export this proc
  of 'e', 'E': ffScientific
  of 'f', 'F': ffDecimal
  else: ffDefault

proc parseAugmentedSpecifier(s: string, start: int, at: var Repo,
                             spec: var StandardFormatSpecifier) =
  # Parse [@[path]] & '%' & standardFormatSpecifier
  if (let pct = s.find('%', start); pct >= 0 and pct + 1 < s.len):
    if s.len > start and s[start] == '@' and pct > 1:
      at = rOpen(s[1..<pct])
    spec = parseStandardFormatSpecifier(s, pct + 1, ignoreUnknownSuffix=true)
  else: raise newException(ValueError, &"{s}: missing '%' or type code")

proc initFormatter*(rowFmt: IORow, atShr: Repo = nil, fmTy: Strings = @[],
                    fmt="", na=""): Formatter =
  result.rowFmt = rowFmt
  result.ats.add atShr                  # [0] is the shared one (or nil if none)
  result.na = na
  var ft: array[IOKind,  string] = ["d", "c", "d", "d", "d", "d", "d", "d",
                                    ".07g", ".016g", ".019g"]
  for cf in fmTy:
    if cf.len < 3: raise newException(ValueError, &"\"{cf}\" too short")
    if cf[1]!='%': raise newException(ValueError, "no '%' between code & spec")
    if cf[0] notin ioCode:
      raise newException(ValueError, &"'{cf[0]}' not [cCsSiIlLfdg]")
    ft[cf[0].ioCodeK] = cf[2..^1]
  var fc: array[IOKind, StandardFormatSpecifier]
  for k in IOKind: fc[k] = parseStandardFormatSpecifier(ft[k], 0, true)
  for c in rowFmt.cols:
    let spec = fc[c.iok]
    result.specs .add spec
    result.radix .add spec.radix
    result.ffmode.add spec.ffmode
    result.ats   .add atShr
  var start, j: int
  while start < fmt.len:
    if j >= rowFmt.cols.len:
      raise newException(ValueError, &"\"{fmt}\" too many output formats")
    parseAugmentedSpecifier fmt, start, result.ats[j+1], result.specs[j]
    result.radix[j]  = result.specs[j].radix
    result.ffmode[j] = result.specs[j].ffmode
    start = result.specs[j].endPosition
    j.inc

proc formatFloat(result: var string, value: float64, ffmode: FloatFormatMode,
                 spec: StandardFormatSpecifier) =
  var f = formatBiggestFloat(value, ffmode, spec.precision)
  var sign = false      # Fix `strformat.formatBiggestFloat` += `sign`,
  if value >= 0.0:      #.. `padWithZero`, `minimumWidth`, `align`, & uppercase
    if spec.sign!='-':
      sign = true
      if value == 0.0:
        if 1.0 / value == Inf: f.insert($spec.sign, 0)
      else: f.insert($spec.sign, 0)
  else: sign = true
  if spec.padWithZero:
    var signStr = ""
    if sign: signStr = $f[0]; f = f[1..^1]
    let toFill = spec.minimumWidth - f.len - ord(sign)
    if toFill > 0: f = repeat('0', toFill) & f
    if sign: f = signStr & f
  let align = if spec.align == '\0': '>' else: spec.align
  let res = alignString(f, spec.minimumWidth, align, spec.fill)
  result.add if spec.typ in {'A'..'Z'}: toUpperAscii(res) else: res

import unicode
proc fmt*(result: var string; fmtr: Formatter; j: int; k: IOKind, s: string,
          naCvt=false) =
  let spec = fmtr.specs[j]
  let radix = fmtr.radix[j]
  if k in ioFloats:                     # SOME FLOAT TYPE
    var value: float64          #XXX port my C float80 to (prs|fmt)BiggestFloat
    convert(dIk, k, value.addr, s[0].unsafeAddr, naCvt)
    result.formatFloat(value, fmtr.ffmode[j], spec)
  else:
    var value: uint64                   # SOME INTEGRAL TYPE, INCLUDING PTR
    if k.isNA(s[0].unsafeAddr): result.add fmtr.na; return
    convert(LIk, k, value.addr, s[0].unsafeAddr, naCvt)
    let at = if fmtr.ats[j + 1].isNil: fmtr.ats[0] else: fmtr.ats[j + 1]
    if spec.typ == 's' and at != nil:   # STRING
      var value = at[value.Ix]          # Q: %r & lift for general indirect?
      if spec.precision != -1 and spec.precision < runeLen(value):
        setLen(value, runeOffset(value, spec.precision))
      result.add alignString(value, spec.minimumWidth, spec.align, spec.fill)
    else:
      result.add formatInt(value, (if radix == 0: 16 else: radix), spec)

proc close*(fmtr: var Formatter) =
  for i in 0 ..< fmtr.ats.len: fmtr.ats[i].close

proc meta*(format="", nios: Strings): int =
  ## print numeric file metadata {like stat(1)}.
  let f = if format.len > 0: format
        else: "name: %n\nrows: %r\nbytes/row: %z\nlastWidth: %w\nlastType: %b\n"
  for path in nios:
    let (path, fmt, _) = metaData(path)
    let sz = if path.len > 0: getFileSize(path) else: 0
    var inPct = false
    for c in f:
      if inPct:
        case c
        of '%': outu '%'
        of 'n': outu path
        of 'r': outu sz div fmt.bytes
        of 'z': outu fmt.bytes
        of 'w': outu $fmt.cols[^1].width
        of 'b': outu $fmt.cols[^1].iok
        else: erru &"bad meta specifier '{c}'\n"; return 1
        inPct = false
      else:
        if c == '%': inPct = true
        else: outu c

proc print*(sep="\t", at="", fmTy: Strings = @[], na="", paths: Strings) =
  ## print native numeric files; pasted side-by-side over rows.
  ##
  ## *.N* suffix + printf-esque extra suffix/`fmTy` may imply number conversion.
  ## Extra suffix fmt has no type code prefix (or space). Eg. **ixVal.Nif%x%g**.
  ## Specify location & formats for `stdin` by an empty path, e.g. `.Nsf%o%.2f`.
  ## Nim specifiers (see below) are pre-% augmented by *@* for `@REPO%s` to aid
  ## string renders.  Augmented Nim format specs are like:
  ##   [[fill]align][sign][#][0][minWidth][.prec][type]
  ##   [[fill]<^>][+- ][#][0][minWidth][.prec][cbdoxXeEfFgG]
  #  C %[flags][minWidth][.prec][modifier][type]
  #    %[' +-#0]*[minWidth][.prec]{hh|h|l|ll|L|z|t}[csdiuoxXeEfFgGaA]
  if paths.len == 0:
    erru "nio print needs >= 1 path/format; Run with --help for usage"; return
  var atShr: Repo
  try: atShr = rOpen(at, na=na)           # `nil` if at == ""
  except: erru &"Cannot open \"{at}\"\n"; quit(1)
  var ofmt: string
  var nfs: seq[NFile]
  var fms: seq[Formatter]
  for path in paths:
    nfs.add nOpen(path, rest=ofmt.addr)
    fms.add initFormatter(nfs[^1].rowFmt, atShr, fmTy, ofmt, na)
  var orow: string
  var sk: IOKind
  var item = newString(16)                # Nim includes an extra byte for '\0'
  while true:
    orow.setLen 0
    var i0, j0: int
    var chFmt: bool
    for i in 0 ..< nfs.len:
      for j in 0 ..< nfs[i].rowFmt.cols.len:
        for k in 0 ..< nfs[i].rowFmt.cols[j].width:
          if not nfs[i].read(sk, item):   # stop @first short file
            return
          if not(i==0 and j==0 and k==0): # skip 1st-1st-1st; sep leads data
            if not chFmt or i != i0 or j != j0: orow.add sep
          chFmt = fms[i].specs[j].typ == 'c' and sk in {cIk, CIk}
          if chFmt:
            orow.add (if item[0] == '\0': ' ' else: item[0])
          else:                           # add formatted field
            orow.fmt fms[i], j, sk, item, naCvt=true
          j0 = j
        i0 = i
    orow.add '\n'
    outu orow                             # cligen/osUt.urite?

proc rip*(input: string, names: Strings): int =
  ## rip apart all columns of `input` into files with given `names`.
  ##
  ## Output numeric formats are either the same as corresponding columns of
  ## input or the type specified in .N/.foo metaData.
  var inp = nOpen(input)
  if inp.rowFmt.cols.len != names.len:
    erru &"too few/many names for input: \"{input}\"\n"; return 1
  var outs = newSeq[NFile](inp.rowFmt.cols.len)
  var offs, ns: seq[int]
  var off = 0
  for j in 0 ..< outs.len:                      # open all outputs
    try: outs[j] = nOpen(names[j], fmWrite)
    except:
      if ".N" in names[j]: raise
      var sfx = ".N"
      for k, c in inp.rowFmt.cols[j].cnts:
        if c > 1:
          sfx.add $c
          if k < inp.rowFmt.cols[j].cnts.len - 1: sfx.add ','
      sfx.add ioCode[inp.rowFmt.cols[j].iok]
      outs[j] = nOpen(names[j] & sfx, fmWrite)
    offs.add off
    let n = outs[j].rowFmt.cols[0].width * ioSize[outs[j].rowFmt.cols[0].iok]
    ns.add n
    off.inc n
  var buf: string
  while inp.read(buf):
    for j in 0 ..< outs.len:
      if outs[j].f.uriteBuffer(buf[offs[j]].addr, ns[j]) < ns[j]: return 2
  for j in 0 ..< outs.len: outs[j].close

proc zip*(paths: Strings): int =
  ## opposite of `rip`; like `paste` but for rows from native files in `paths`.
  if paths.len < 2: erru "`zip` needs 2 or more NIO paths\n"; return 1
  var fs = newSeq[NFile](paths.len)
  for i, p in paths: fs[i] = nOpen(p)
  var buf: string
  block outer:
    while true:
      for i in 0 ..< fs.len:
        if not fs[i].read(buf):         # stop @first short file
          break outer
        outu buf
  for i in 0 ..< fs.len: fs[i].close

iterator elts(slices: Strings, bound: int): (int, int) =
  var a, b: int
  for s in slices:
    (a, b) = parseSlice(s)
    if a < 0: a.inc bound
    if b < 0: b.inc bound
    if b < a: b = a + 1
    yield (a, b)

proc cut*(drop: Strings = @[], pass: Strings = @[], paths: Strings): int =
  ## pass|drop selected column slices {generalized cut(1)} to stdout.
  ##
  ## Slice specification is `[a][:[b]]`, like Python (incl negatives).  Can
  ## either pass|drop but not both at once.  Multiple slices are "set unioned".
  if paths.len > 1 or (drop.len > 0 and pass.len > 0):
    erru "`cut` needs exactly 1 input and not both drop&pass\n"; return 1
  var cPass = int(pass.len > 0)
  let fields = if drop.len > 0: drop else: pass
  var inp = nOpen(paths[0])
  var colSet: HashSet[int]              # Q: Tensors aided by flat view?
  for (a,b) in fields.elts(inp.rowFmt.cols.len):
    for j in a..<b: colSet.incl j
  var buf: string
  var offs, ns: seq[int]
  var off = 0
  for j in 0 ..< inp.rowFmt.cols.len:
    offs.add off
    let n = inp.rowFmt.cols[j].width * ioSize[inp.rowFmt.cols[j].iok]
    ns.add n
    off.inc n
  while inp.read(buf):
    for j in 0 ..< inp.rowFmt.cols.len:
      if (cPass xor (j in colSet).int) == 0:    # Passing column/field
        if stdout.uriteBuffer(buf[offs[j]].addr, ns[j]) < ns[j]:
          return 1

proc tails*(head=0, tail=10, compl=false, paths: Strings): int =
  ## Generalized tail(1)-like filter
  ##
  ## -h10 -t10 will pass *both* head & tail, -h10 -t10 --compl will pass the
  ## "main body" of a distribution if the rows are sorted.  -ch10 =~ tail -n+10.
  discard

proc deftype*(names: Strings = @[], lang="nim", paths: Strings): int =
  ## print prog `lang` type defs for NIO rows from extensions in `paths`.
  ##
  ## E.g.: `fooBar.Nif` -> `struct fooBar { unsigned int foo; float bar; };`
  ## These types allow treating memory mapped files as unchecked arrays or ease
  ## row-wise calculation/streamed IO.
  case lang.toLowerASCII: #Q: Require schema to do this for nicer names?
  of "nim":
    let ntype: array[IOKind, string] = ["int8", "uint8", "int16", "uint16",
      "int32", "uint32", "int64", "uint64", "float32", "float64", "float80"]
    var i = 0
    for path in paths:
      let (_, baseName, _) = path.splitPathName
      let (_, fmt, _) = metaData(path)
      outu &"type {baseName} = object " & "{.packed.}\n"
      for c in fmt.cols:
        let nm = if i < names.len: names[i] else: "field" & $i
        outu &"    {nm}: "
        if   c.cnts.len > 2 : outu &"array[.., arr[{c.cnts[1]}, arr[{c.cnts[0]}, {ntype[c.iok]}]]"
        elif c.cnts.len == 2: outu &"array[{c.cnts[1]}, array[{c.cnts[0]}, {ntype[c.iok]}]]"
        elif c.cnts[0] > 1  : outu &"array[{c.cnts[0]}, {ntype[c.iok]}]"
        else                : outu &"{ntype[c.iok]}"
        outu "\n"
        i.inc
  of "c":
    let ctype: array[IOKind, string] = ["signed char", "unsigned char",
      "short", "unsigned short", "int", "unsigned int", "long", "unsigned long",
      "float", "double", "long double"]
    var i = 0
    for path in paths:
      let (_, baseName, _) = path.splitPathName
      let (_, fmt, _) = metaData(path)
      outu &"struct {baseName} " & "{\n"
      for c in fmt.cols:
        let nm = if i < names.len: names[i] else: "field" & $i
        outu &"    {ctype[c.iok]} {nm}"
        if   c.cnts.len > 2 : outu &"[...][{c.cnts[1]}][{c.cnts[0]}]"
        elif c.cnts.len == 2: outu &"[{c.cnts[1]}][{c.cnts[0]}]"
        elif c.cnts[0] > 1  : outu &"[{c.cnts[0]}]"
        outu ";\n"
        i.inc
      outu "} __attribute__((__packed__));\n"
  else: erru &"unknown programming language \"{lang}\"\n"; return 1

import parseutils
type Transform = proc(ixOut: pointer, inp: string, lno: int)

proc parse(inp, name: string; lno: int; inCode: char; kout: IOKind; outp=stdout,
           xfm: Transform, cnt=1) {.inline.} =
  var nS: int                         # widest types for number parsing
  var nU: uint
  var nF: float
  var obuf: array[16, char]             # actual output buffer
  template p(fn, n, k, low, high) =
    if inp.fn(n) != inp.len:
      raise newException(IOError, &"stdin:{lno}: bad fmt \"{inp}\"")
    let lo = type(n)(low[kout.int shr 1])
    let hi = type(n)(high[kout.int shr 1])
    if n < lo: erru &"{name}:{lno} \"{inp}\" underflows\n"; n = lo
    if n > hi: erru &"{name}:{lno} \"{inp}\" overflows\n";  n = hi
    convert(kout, k, obuf[0].addr, n.addr)

  if inp.len == 0:                      # empty string ==> N/A
    setNA kout, obuf[0].addr
    outp.nurite kout, obuf[0].addr
    return
  case inCode
  of 'c':
    let pc = padClip(inp, cnt)
    discard outp.uriteBuffer(pc[0].unsafeAddr, cnt)
    return
  of 'b': (if kout.isSigned: p(parseBin,nS,lIk,lowS,highS) else: p(parseBin ,nU,LIk,lowU,highU))
  of 'o': (if kout.isSigned: p(parseOct,nS,lIk,lowS,highS) else: p(parseOct ,nU,LIk,lowU,highU))
  of 'd': (if kout.isSigned: p(parseInt,nS,lIk,lowS,highS) else: p(parseUInt,nU,LIk,lowU,highU))
  of 'h': (if kout.isSigned: p(parseHex,nS,lIk,lowS,highS) else: p(parseHex ,nU,LIk,lowU,highU))
  of 'f':
    if inp.parseFloat(nF) != inp.len:
      raise newException(IOError, &"stdin:{lno}: bad fmt \"{inp}\"")
    convert(kout, dik, obuf[0].addr, nF.addr)
  of 'x': xfm(obuf[0].addr, inp, lno)
  else: raise newException(IOError, &"{inCode}: inCode not in [bodhfx]")
  outp.nurite kout, obuf[0].addr

proc initXfm(inCode: char, kout: IOKind, xfm: string): Transform =
  if xfm.startsWith('@'):               # transform to an index into a repo
    let mode = if "NOUP" in xfm: rmIndex else: rmUpdate
    var r = rOpen(xfm[1..^1], mode, kout)
    proc xfmIx(ixOut: pointer, inp: string, lno: int) = r.index(ixOut, inp, lno)
    return xfmIx
# elif xfm.startsWith("T"):             # parse times into whatever XXX
# elif xfm.startsWith("~"):             # integer-to-integer mapper
# elif xfm.startsWith("L"):             # external shared library/DLL
  else: raise newException(ValueError, "unknown transformer prefix")

proc load1*(inCode, oCode: char; xform="", count=1): int =
  ## load 1 ASCII column on stdin to NIO on stdout (for import,testing).
  let kout = ioCodeK(oCode)
  var xfm: Transform
  if inCode == 'x': xfm = initXfm(inCode, kout, xform)
  var inp: string
  var lno = 1
  while stdin.readLine(inp):
    inp.parse "stdin", lno, inCode, kout, stdout, xfm, count
    lno.inc
  if inCode == 'x': xfm(nil, "", 0)     # tell Transform to close

proc maybeAppend(path: string): FileMode = # modes maybe weird from Windows
  var info: FileInfo
  try: info = getFileInfo(path)
  except: discard
  result = if info.id.device == 0: fmWrite else: fmAppend

from posix import popen # does Windows really not have this?
from cligen/argcvt import unescape
proc fromSV*(schema="", nameSep="", onlyOut=false, SVs: Strings): int =
  ## parse *strict* separated values on stdin|SVs to NIO files via schemas.
  ##
  ## An example schema file (with default values but for `outSfx`):
  ##   --preproc=gzip -d $1|c2tsv   # popen() preproc to make strict TSV
  ##   --delim=\\0                   # NUL is c2tsv output delimiter
  ##   --nHeader=1                  # number of rows which are headers
  ##   --maxLog=100                 # limit same-kind log messages
  ##   --zip                        # stdout zip mode not default rip mode
  ##   --shared=strings             # name of any common strings file
  ##   #name NC SC TRANSFORM:args   # NC=NIOcode; SC=(load|src)Code like load1
  ##   qty   i  d                   # parse input as decimal; emit uint32
  ##   Px    f  f                   # parse as float32; emit float32
  ##   Junk  i  ignore              # ignore an input column
  ##   Id    8C c                   # embedded char arrays pad-clipped but can
  ##   Date  s  x  @dates.N9c       #..be transformed via intern into *fixed*
  ##   City  s  x  @cities.Dn       #..or *variable width* repositories,
  ##   Note  i  x  @.LS             #..with maybe a shared common string repo.
  ## create/appends *qtyPxIdDateCityNote.Nif8C2si*, *dates.N9c*, *cities.Dn*
  ## and length-prefixed *strings.LS*.
  type Col = tuple[inCode:char; f:File; xfm:Transform; kout:IOKind; count:int]
  if schema.len == 0: erru "Cannot infer schema; Provide one\n"; return 1
  var cols: seq[Col]
  var pp, outBase, outType: string
  var shrd = "strings"
  var dlm  = '\0' 
  var nHdr = 1    
  var mxLg = 100  
  var doZip: bool
  var xfm0: Transform = nil
  var slno = 1
  for line in lines(schema):
    var c: Col                          # in loop re-inits each time
    let line = line.commentStrip
    let mcols = line.split({'=', ':'})  # parse schema-level arg
    let sarg = if mcols.len > 1: mcols[1] else: ""
    if   line.len == 0: continue
    elif line.startsWith("--preproc"): pp    = sarg
    elif line.startsWith("--delim"  ): dlm   = argcvt.unescape(sarg)[0]
    elif line.startsWith("--nHeader"): nHdr  = parseInt(sarg)
    elif line.startsWith("--maxLog" ): mxLg  = parseInt(sarg)
    elif line.startsWith("--zip"    ): doZip = true
    elif line.startsWith("--shared" ): shrd  = sarg
    else: #XXX Requires exactly 1 char dlm for schema text cols; mslice.splitr?
      let scols = strutils.split(line, Whitespace, maxSplit=3)
      if scols.len > 0:
        if scols[0] != "_":             # Q: Allow zipped notation? fooBar.Nif?
          if scols.len < 3:
            raise newException(ValueError,&"{schema}:{slno} < 3 cols")
          c.kout = ioCodeK(scols[1][^1])
          c.inCode = scols[2][0]
          c.count = if scols[1].len > 1: parseInt(scols[1][0..^2]) else: 1
          if doZip:                     # stdout zipped mode
            c.f = stdout
            if outBase.len > 0: outBase.add nameSep
            outBase.add scols[0]
            outType.add scols[1]
          else:
            let path = scols[0] & ".N" & scols[1]
            if not onlyOut: c.f = open(path, path.maybeAppend)
          if scols.len > 3:
            if not onlyOut:
              if scols[3].startsWith("@."):
                if xfm0 == nil: 
                  xfm0 = initXfm(c.inCode, c.kout, "@" & shrd)
                  c.xfm = xfm0
                else: c.xfm = xfm0
              else: c.xfm = initXfm(c.inCode, c.kout, scols[3])
          elif c.inCode == 'x':
            raise newException(ValueError, "'x' but no tranform arguments")
        cols.add c
  if onlyOut:
    if doZip: outu outBase, ".N", outType, '\n'
    else: erru &"--onlyOut makes sense only for --zip schemas\n"
    return
  for path in SVs:                      # Now the actual parsing part!
    var lno = 1
    let inpFile = if pp.len > 0: popen(pp % [path], "r")
                  elif path.len == 0 or path == "/dev/stdin": stdin
                  else: open(path)
    for row in lines(inpFile):
      if lno > nHdr:                    # skip however many header rows
        var cix = 0
        for inp in row.split(dlm):
          let c = cols[cix]
          if c.f != nil:
            inp.parse path, lno, c.inCode, c.kout, c.f, c.xfm, c.count
          cix.inc
      lno.inc
    inpFile.close
  for c in cols:
    if c.xfm != xfm0 and c.xfm != nil: c.xfm(nil, "", 0)   # close `Transform`s
    if c.f != nil and c.f != stdout: c.f.close
  if xfm0 != nil: xfm0(nil, "", 0)        # close common `Transform`

import stats
type MomKind = enum moN="n", moMin="min", moMax="max", moSum="sum",
                    moAvg="avg", moSdev="sdev", moSkew="moew", moKurt="kurt"

proc fmtStat(rs: RunningStat, mo: MomKind): string =
  case mo
  of moN:    $rs.n
  of moMin:  $rs.min
  of moMax:  $rs.max
  of moSum:  $rs.sum
  of moAvg:  $rs.mean
  of moSdev: $rs.standardDeviation
  of moSkew: $rs.skewness
  of moKurt: $rs.kurtosis

proc moments*(fmt="%.6f", stats: set[MomKind] = {moMin, moMax}, paths: Strings): int =
  ## print selected moments over all columns of all `paths`.
  for path in paths:                    # NOTE: This is intended as an easy,
    var inp = nOpen(path)               #..but not useless example calculation.
    var sts = newSeq[RunningStat](inp.rowFmt.cols.len)
    var num: float
    block fileLoop:
      while true:
        for j in 0 ..< sts.len:
          if not inp.read(num):
            break fileLoop
          if not num.isnan:
            sts[j].push num
    for j in 0 ..< sts.len:
      outu path, ":", j
      for mo in [moN, moMin, moMax, moSum, moAvg, moSdev, moSkew, moKurt]:
        if mo in stats: outu " ", $mo, ": ", fmtStat(sts[j], mo)
      outu "\n"

when isMainModule:
  import cligen, cligen/cfUt, os

  proc mergeParams(cmdNames: Strings, cmdLine=commandLineParams()): Strings =
    if cmdNames.len > 0:
      if cmdNames[0] == "multi":
        let bn = paramStr(0).lastPathPart
        if bn.startsWith("n-"):
          let bns = bn[2..^1]           # baseName suffix
          if bns in ["load1", "fromSV", "meta", "print", "zip", "rip", "cut",
                     "tails", "moments", "deftype"]:  # allow n-foo.. hardlinks
            result.add bn[2..^1]
        return result & cmdline
      let underJoin = strutils.toUpperAscii(cmdNames.join("_"))
      var cfPath = getEnv(underJoin & "_CONFIG")      # See if cfg file redirect
      if cfPath.len == 0:                             #..else use getConfigDir.
        cfPath = getConfigDir() / cmdNames[0] / "config"   # See if dir w/cfg
        if not existsFile(cfPath): cfPath = cfPath[0..^8]  #..else use file.
      result.add cfToCL(cfPath, if cmdNames.len > 1: cmdNames[1] else: "",
                        quiet=true, noRaise=true)
      result.add envToCL(underJoin)     # Finally add $NIO_PRINT..
    result = result & cmdLine           # and then whatever user entered

  dispatchMulti(
    [load1 , help={"inCode": """input code: [**bodh**] Bin|Octal|Dec|Hex int
**f** Float; **c** charArray(count); **x** transform""",
                   "oCode" : "Usual [**cCsSiIlLfdg**] NIO storage code",
                   "xform" : "TransformParams; Eg. @file.[NDL].. {NOUP}",
                   "count" : "Output size; Only works for 'c'"}],
    [fromSV, help={"SVs"   : "[?SVs: input paths; empty|\"-\"=stdin]",
                   "onlyOut": "only parse schema & gen output name",
                   "nameSep": "string to separate schema col names",
                   "schema": "path to the parsing schema file"}],
    [meta  , help={"nios"  : "paths to NIO files",
                   "format": """%[nrwzb]: name,rows,width,rowSize,lastBaseType
empty string => full information format"""},
             short={"format": 'c'}],
    [print , help={"sep"   : "separator for output columns",
                   "at"    : "shared default file for %s formats",
                   "fmTy"  : """format spec defaults for types; Default:
c%c C%d s%d S%d i%d I%d l%d L%d f%g d%g g%g
if AT=="" %s renders as a number via `fmTy`""",
                   "na"    : "how NA pointers render",
                   "paths" : "[paths: 0|more paths to NIO files]"}],
    [rip   , help={"input" : "NIO file to separate",
                   "names" : "pre.N names for output files"}],
    [zip   , help={"paths" : "[paths: 2|more paths to NIO files]"}],
    [cut   , help={"paths" : "{paths: 1 path to a NIO file}",
                   "drop"  : "drop/delete field slice [a][:[b]]",
                   "pass"  : "pass/propagate field slice [a][:[b]]"}],
    [tails , help={"paths" : "{paths: 1|more NIO paths}",
                   "head"  : "initial fence post; 0=>none",
                   "tail"  : "final fence post; 0=>none",
                   "compl" : "pass complement/inside of fence posts"},
            short={"help": '?'}],
    [moments,help={"paths" : "[paths: 1|more paths to NIO files]",
                   "fmt"   : "Nim floating point output format",
                   "stats":"*n* *min* *max* *sum* *avg* *sdev* *skew* *kurt*"}],
    [deftype,help={"paths" : "[paths: 1|more paths to NIO files]",
                   "names" : "names for each column",
                   "lang"  : "programming language"}])

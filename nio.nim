## This is a system for managing pre-parsed N)umeric|N)ative binary data.  File
## names have extensions|dotfiles w/metadata needed to grasp data as typed rows.
## Syntax is *".N"* then 1|more *{COUNT{,COUNT..}}[cCsSiIlLfdg]* specs where
## **Uppercase => Unsigned** & letter=1st letter of C type except g=lonG double.
## No count => 1.  *'_'* is ignored.  *idDayVal.N2sf* <~> packed *struct{ short
## id,day; float val; }* can work for simple time series. N/A = NaN | signed.low
## | unsigned.high.  "" before ".N" gives *stdin* row formats when needed.
const fmtUse* = "\nSyntax: ({COUNT{,COUNT...}}[cCsSiIlLfdg])+\n"

when not declared(FileMode): import std/[syncio, objectdollar]; export syncio
include cligen/unsafeAddr                                   # `formatInt` --|
import std/strutils as su, std/[math, os, times, strtabs], std/strformat{.all.},
       std/[tables,sets], system/ansi_C, cligen/[sysUt,osUt,strUt,fileUt,mslice]
from std/memfiles as mf import nil

type #*** BASIC TYPE SETUP  #NOTE: gcc __float128 CPU-portable but slow
  IOKind* = enum cIk = "int8" , CIk = "uint8" , sIk = "int16", SIk = "uint16",
                 iIk = "int32", IIk = "uint32", lIk = "int64", LIk = "uint64",
                 fIk = "float32", dIk = "float64", gIk = "float80"

  IOCol* = object       ## column metadata
    iok*:   IOKind      ## number kind
    cnts*:  seq[int]    ## repetition counts for each dimension
    off*:   int         ## offset of start of column within a row
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

func isNil*(nf: NFile): bool = nf.m.mem.isNil and nf.f.isNil
func ok*   (nf: NFile): bool = not nf.isNil
func width*(nf: NFile): int = nf.rowFmt.bytes

func low* (T: typedesc[float80]): float80 = float64.low   # float80 support
func high*(T: typedesc[float80]): float80 = float64.high
converter toFloat80*(pdqr: SomeNumber): float80 = {.emit: "result = `pdqr`;".}
template defc(T) {.dirty.} = # need dirty to avoid genSym so emit can work
  converter `to T`*(xyzw: float80): T = {.emit: "result = `xyzw`;".}
defc uint8; defc uint16; defc uint32; defc uint64; defc float32
defc  int8; defc  int16; defc  int32; defc  int64; defc float64

const ioCode*: array[IOKind, char] = [ 'c','C', 's','S', 'i','I', 'l','L',
                                       'f','d','g' ]              ## type codes
const ioSize*: array[IOKind, int] = [1,1, 2,2, 4,4, 8,8, 4,8,16]  ## type sizes
const ioFloats* = {fIk, dIk, gIk}                                 ## float kinds

let nim2nio* = {"int8"   : 'c', "uint8"  : 'C', "int16": 's', "uint16": 'S',
                "int32"  : 'i', "uint32" : 'I', "int64": 'l', "uint64": 'L',
                "float32": 'f', "float64": 'd', "float80": 'g', "int": 'l',
                "uint": 'L', "float": 'd', "char": 'C', "byte": 'C'}.toTable

func kindOf*(c: char): IOKind {.inline.} =
  ## return IOKind corresponding to character `c`
  let ix = ioCode.find(c)
  if ix < 0: Value!"expecting [cCsSiIlLfdg]"
  IOKind(ix)

func kindOf*(x: IONumber): IOKind =
  ## return IOKind from a static Nim type
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

func isSigned*  (k: IOKind): bool {.inline.} = (k.int and 1) == 0
func isUnsigned*(k: IOKind): bool {.inline.} = (k.int and 1) == 1
func isFloat*   (k: IOKind): bool {.inline.} = k.int > LIk.int

#*** MISSING VALUE CONVENTION; NA-AWARE LOW/HIGH MOSTLY FOR CLIPPING PARSED DATA
template defs(c, T) {.dirty.} =  # NA,Low,High-Signed
 const `c na`* =T.low;const`c low`* =int64(T.low+1);const`c high`* =T.high.int64
defs c,  int8; defs s,  int16; defs i,  int32; defs l,  int64

template defu(C, T) {.dirty.} =  # NA,Low,High-Unsigned
 const `C na`* =T.high; const `C low`* =0u64; const `C high`* = uint64(T.high-1)
defu C, uint8; defu S, uint16; defu I, uint32; defu L, uint64

template deff(f, T) {.dirty.} =  # NA,Low,High-floating point
 const `f na`* = T(NaN); const `f low`* = T.low; const `f high`* = T.high
deff f, float32; deff d, float64; deff g, float80

const lowS*  = [clow , slow , ilow , llow ] # [k.int shr 1] post-isSigned
const highS* = [chigh, shigh, ihigh, lhigh]
const lowU*  = [Clow , Slow , Ilow , Llow ] # [k.int shr 1] post-isUnsigned
const highU* = [Chigh, Shigh, Ihigh, Lhigh]
const lowF*  = [float64.low , float64.low , float64.low ] # [k.int-8] post-isFlt
const highF* = [float64.high, float64.high, float64.high]

template withTyped_P_NA(k, adr, p, na, body) =
  case k
  of cIk: (let p = cast[ptr  int8  ](adr); let na{.used.} = cna; body)
  of CIk: (let p = cast[ptr uint8  ](adr); let na{.used.} = Cna; body)
  of sIk: (let p = cast[ptr  int16 ](adr); let na{.used.} = sna; body)
  of SIk: (let p = cast[ptr uint16 ](adr); let na{.used.} = Sna; body)
  of iIk: (let p = cast[ptr  int32 ](adr); let na{.used.} = ina; body)
  of IIk: (let p = cast[ptr uint32 ](adr); let na{.used.} = Ina; body)
  of lIk: (let p = cast[ptr  int64 ](adr); let na{.used.} = lna; body)
  of LIk: (let p = cast[ptr uint64 ](adr); let na{.used.} = Lna; body)
  of fIk: (let p = cast[ptr float32](adr); let na{.used.} = fna; body)
  of dIk: (let p = cast[ptr float64](adr); let na{.used.} = dna; body)
  of gIk: (let p = cast[ptr float80](adr); let na{.used.} = gna; body)

func isNA*(k: IOKind, adr: pointer): bool {.inline.} =
  ## Test the number IO kind at `adr` against its missing/NA value
  withTyped_P_NA(k, adr, p, na):
    when declared(isNaN): return if k > LIk: p[].float64.isNaN else: p[] == na
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

template withTypedPtrPair(k, aP, bP, a, b, body) =
  case k # `withTypedP(a): withTypedP(b): ..` may also work but is likely slow.
  of cIk: (let a = cast[ptr  int8  ](aP); let b = cast[ptr  int8  ](bP); body)
  of CIk: (let a = cast[ptr uint8  ](aP); let b = cast[ptr uint8  ](bP); body)
  of sIk: (let a = cast[ptr  int16 ](aP); let b = cast[ptr  int16 ](bP); body)
  of SIk: (let a = cast[ptr uint16 ](aP); let b = cast[ptr uint16 ](bP); body)
  of iIk: (let a = cast[ptr  int32 ](aP); let b = cast[ptr  int32 ](bP); body)
  of IIk: (let a = cast[ptr uint32 ](aP); let b = cast[ptr uint32 ](bP); body)
  of lIk: (let a = cast[ptr  int64 ](aP); let b = cast[ptr  int64 ](bP); body)
  of LIk: (let a = cast[ptr uint64 ](aP); let b = cast[ptr uint64 ](bP); body)
  of fIk: (let a = cast[ptr float32](aP); let b = cast[ptr float32](bP); body)
  of dIk: (let a = cast[ptr float64](aP); let b = cast[ptr float64](bP); body)
  of gIk: discard #(let a=cast[ptr float80](aP); let b=cast[ptr float80](bP))

proc compare*(k: IOKind; aP, bP: pointer): int {.inline.} =
  withTypedPtrPair(k, aP, bP, a, b): result = cmp(a[], b[])

#*** METADATA ACQUISITION SECTION
func initIORow*(fmt: string, endAt: var int): IORow =
  ## Parse NIO format string `fmt` into an `IORow`
  if fmt.len < 1: IO!"empty row format"
  var col = IOCol(cnts: @[0])
  endAt = fmt.len - 1
  for i, c in fmt:
    case c:
    of '_': discard                     # Allow optional spacing stuff out
    of '0'..'9': col.cnts[^1] *= 10; col.cnts[^1] += ord(c) - ord('0')
#Explicit "0+" -> IO!"0 dim in type fmt: " & fmt & fmtUse
    of ',': col.cnts.add 0              # Make room for next dimension
    of 'c', 'C', 's', 'S', 'i', 'I', 'l', 'L', 'f', 'd', 'g':
      col.iok = fmt[i].kindOf           # Type code terminates IO-column spec
      for i in 0 ..< col.cnts.len:      # All 0s -> 1s
        if col.cnts[i] == 0: col.cnts[0] = 1
      col.width = col.cnts.prod
      col.off = result.bytes
      result.bytes += col.width * ioSize[col.iok]
      result.cols.add move(col)         # only GC'd type in an IOCol is `cnts`..
      col.cnts = @[0]                   #..which we re-init here anyway.
    else:
      if c in {'a'..'z', 'A'..'Z'}: IO ! &"bad code {c} in fmt: "&fmt&fmtUse
      else: # Allow things like '@'.* after a format suffix
        endAt = i
        return
  if result.cols.len == 0: IO!"no columns in fmt: " & fmt & fmtUse

#XXX Add $pfx$kind${sep}foo meta file|dirs for kind="fmt","out".. Eg: .fmt/foo
proc dotContents(path: string): string =
  for line in lines(path): result.add line.commentStrip

proc metaData(path: string): (string, IORow, string) =
  var endAt: int
  var (dir, name) = splitPath(path)
  let ext = name.find(".N")
  let fmt = if ext != -1: name[ext+2 .. ^1] else:
              try: dotContents(dir / "." & name) except Ce:
                Value!path&": has no .N extension | dotfile"
  result[1] = fmt.initIORow(endAt)
  if ext != -1: name = name[0..<ext]
  if endAt >= fmt.len - 1:
    result[0] = if name.len == 0 or ext == 0: "" else: path
  else:
    result[0] = if name.len == 0 or ext==0: ""
                elif ext != -1: path[0 ..< path.len-fmt.len+endAt] else: path
    result[2] = fmt[endAt..^1]

#*** FILE IO SECTION
# fmRead       RDONLY          fmWrite  CREAT|TRUNC
# fmReadWrite  RW|CREAT|TRUNC  fmAppend WRONLY|APPEND  fmReadWriteExisting RW
proc nOpen*(path: string, mode=fmRead, newFileSize = -1, allowRemap=false,
            mapFlags=cint(-1), rest: ptr string=nil): NFile =
  let (path, fmt, rst) = metaData(path)
  if fmt.bytes == 0: Value ! &"{path}: some ext problem"
  result.rowFmt = fmt
  result.mode = mode
  if path.len == 0:
    case mode
    of fmRead: result.f = stdin
    of fmAppend, fmWrite: result.f = stdout
    else: Value!"nameless unsupported by " & $mode
  else:
    try: # XXX could get this down to 1 open syscall & map setup
      if mode == fmAppend: Value!""     # force stdIO
      if not (mode == fmRead and path.fileExists and path.getFileSize == 0):
        result.m = mf.open(path, mode, newFileSize=newFileSize,
                           allowRemap=allowRemap, mapFlags=mapFlags)
      else:
        result.m.mem = cast[pointer](1)
    except Ce:
      result.f = open(path, mode, max(8192, result.rowFmt.bytes))
  if rest != nil: rest[] = rst

proc close*(nf: var NFile) =    # mf.close requires `var`
  if not nf.m.mem.isNil and nf.m.mem != cast[pointer](1): mf.close nf.m
  if not nf.f.isNil: close nf.f

template arrayBase[I, T](a: typedesc[array[I, T]]): untyped = T
proc makeSuffix[T](result: var string, x: T, inArray=false) =
  when T is tuple or T is object:
    if inArray: Value!"array[N, obj|tup] is unsupported"
    for e in x.fields:          # could maybe do 2-passes to count and for
      result.makeSuffix e       #..the special case of 1 field just unbox.
  elif T is array:
    if inArray: result.add ','
    result.add $(T.high - T.low + 1)
    var x: arrayBase(T)         # Use devious `arrayBase` to handle tensors
    result.makeSuffix x, inArray=true
  else:
    try      : result.add nim2nio[$type(T)]
    except Ce: Value!("missing type key: " &
                 (when (NimMajor,NimMinor)<(2,1): repr($type(T)) else: T.repr))

proc makeName[T](result: var string, key: string, x: T, sep=",") =
  when T is tuple or T is object:
    for k, e in x.fieldPairs:
      result.makeName k, e, sep
  else:
    if result.len > 0: result.add sep
    result.add key

proc typedPath*[T: tuple|object|array|IONumber](path="", sep=","): string =
  ## Automatic suffix|basename simply by not providing in the passed `path`;
  ## E.g. `"./.Nl"` (gen name), `"./foo"` (gen sfx), or `"./"` (gen both).
  var (dir, name, ext) = splitPathName(path)
  if ".N" notin ext:
    ext.add ".N"
    var x: T
    ext.makeSuffix x
  if name.len == 0:
    var x: T
    name.makeName "field", x, sep
  if dir.len > 0: dir/(name & ext)
  else: name & ext #XXX Would be nice to detect&report missing {.packed.} pragma

proc save*[T: tuple|object|array|IONumber](x: openArray[T], path="",
           mode=fmWrite, sep: static[string]=",") =
  ## Blast `x` to a file with optionally generated path a la `typedPath`.
  var f = open(typedPath[T](path, sep), mode)
  let n = T.sizeof * x.len
  if f.writeBuffer(x[0].unsafeAddr, n) < n: Value!"nio.save: short write"
  f.close

proc read*(nf: var NFile, buf: var string, sz=0): bool =
  ## Read `sz` bytes (a whole row by default); Returns false on short read/EOF.
  ## `while nf.read(buf): ..`
  let sz = if sz > 0: sz else: nf.rowFmt.bytes
  buf.setLen sz
  if not nf.f.isNil:
    result = nf.f.readBuffer(buf[0].addr, sz) == sz
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
      convert da.kindOf, sk, da.addr, sBuf[0].addr, naCvt
      result = true

defr uint8; defr uint16; defr uint32; defr uint64; defr float32
defr  int8; defr  int16; defr  int32; defr  int64; defr float64; defr float80

proc readCvt*(nf: var NFile, nums: var openArray[IONumber], naCvt=false): bool =
  ## Read next `nums.len` items, converting to buf type & maybe converting NAs.
  result = true
  for i in 0 ..< nums.len:
    if not nf.read(nums[i], naCvt): return false

proc add*[T: IONumber](buffer: var seq[T], path: string, naCvt=false) =
  ## Append whole file `path` to type-homogeneous `seq[T]` maybe-converting NAs.
  var nf = nOpen(path)
  var num: array[1, T]
  while true:
    if not nf.readCvt(num, naCvt): break
    buffer.add num[0]
  nf.close

proc readCvt*[T: IONumber](path: string, naCvt=false): seq[T] =
  ## Read whole file `path` into type-homogeneous `seq[T]` maybe-converting NAs.
  result.add path, naCvt

proc nurite*(f: File, kout: IOKind, buf: pointer) =
  ## Numeric unlocked write to stdio `f`.
  discard f.uriteBuffer(buf, ioSize[kout])

#*** MEMORY MAPPED IO SECTION
func len*(nf: NFile): int {.inline.} =
  when not defined(danger): (if nf.m.mem.isNil: Value!"non-mmapped file")
  nf.m.size div nf.rowFmt.bytes

template BadIndex: untyped =
  when declared(IndexDefect): IndexDefect else: IndexError
template razIE(i,nf,m) =
  raise newException(BadIndex(),formatErrorIndexBound(i, nf.m.size div m))

func `[]`*(nf: NFile, i: int): pointer {.inline.} =
  ## Returns pointer to the i-th row of a file opened with whatever row format
  ## and whatever *mode* (eg. fmReadWrite).  Cast it to an appropriate Nim type:
  ##   `let p = cast[ptr MyType](nfil[17]); echo p.myField; p.bar=2 #May SEGV!`.
  let m = nf.rowFmt.bytes
  when not defined(danger):
    if nf.m.mem.isNil: Value!"non-mmapped file"
    if i >=% nf.m.size div m: razIE(i,nf,m)
  cast[pointer](cast[int](nf.m.mem) +% i*m)

func `[]`*(nf: NFile; T: typedesc; i: int): T {.inline.} =
  ## Returns i-th row of a file opened with whatever row format *copied* into
  ## `result`.  E.g.: `echo nfil[float, 17]`.
  cast[ptr T](nf[i])[]

iterator items*(nf: NFile): string =
  ## iteration over untyped rows; (only MemFile IO right now, but generalizable)
  var s = newString(nf.width)
  for e in 0 ..< nf.len:
    copyMem s[0].addr, nf[e], s.len
    yield s

iterator pairs*(nf: NFile): (int, string) =
  ## indexed iteration over untyped rows, like an `openArray[T]`
  var i = 0
  for e in nf:
    yield (i, e)
    inc i

type
  FileArray*[T] = object ## For *typed* external arrays of general records
    nf*: NFile # whole NFile here allows .close & maybe MemFile.resize niceness

  IOTensor* = object  ## Specialization for single-IOKind-base-types
    m*:   mf.MemFile  ## backing file
    t*:   IOKind      ## single IOKind, e.g. fIk for .N128,128f
    fmt*: IORow       ## parsed row format
    d*:   seq[int]    ## dimensions

func initFileArray*[T](nf: NFile): FileArray[T] =
  ## An init from NFile in case you want to nf.close before program exit.
  if T.sizeof != nf.rowFmt.bytes:Value!"path rowFmt.bytes != FileArray T.sizeof"
  result.nf = nf

proc init*[T](fa: var FileArray[T], path: string, mode=fmRead, newLen = -1,
              allowRemap=false, mapFlags=cint(-1), rest: ptr string=nil) =
  ## A var init from nOpen params for, e.g. init of FileArray fields in objects.
  ## `newLen` is in units of `T` row sizes.
  let sz = if newLen == -1: -1 else: newLen * T.sizeof
  fa = initFileArray[T](nOpen(path, mode, sz, allowRemap, mapFlags, rest))

proc initFileArray*[T](path: string, mode=fmRead, newLen = -1, allowRemap=false,
                       mapFlags=cint(-1), rest: ptr string=nil): FileArray[T] =
  ## The most likely entry point.  Note that `result.close` is both allowed and
  ## encouraged if you finish with data prior to program exit.
  result.init path, mode, newLen, allowRemap, mapFlags, rest

proc load*[T](path: string, mode=fmRead, newLen = -1, allowRemap=false,
              mapFlags=cint(-1), rest: ptr string=nil): FileArray[T] =
  ## Short alias for `initFileArray`.
  result.init path, mode, newLen, allowRemap, mapFlags, rest

proc close*[T](fa: var FileArray[T]) = close fa.nf

template toOA*[T](fa: FileArray[T]): untyped =
  ## Allow RO slice access like `myFileArray.toOA[^9..^1]` for a "tail".
  toOpenArray[T](cast[ptr UncheckedArray[T]](fa.nf.m.mem), 0, fa.len - 1)

template toOpenArray*[T](fa: FileArray[T]): untyped =
  ## Some people enjoy longer idents
  toOpenArray[T](cast[ptr UncheckedArray[T]](fa.nf.m.mem), 0, fa.len - 1)

iterator items*[T](fa: FileArray[T]): T =
  for b in countup(0, fa.nf.m.size - T.sizeof, T.sizeof):
    yield cast[ptr T](cast[int](fa.nf.m.mem) +% b)[]

iterator pairs*[T](fa: FileArray[T]): (int, T) =
  let m = T.sizeof
  for i in countup(0, fa.nf.m.size div m - 1):
    yield (i, cast[ptr T](cast[int](fa.nf.m.mem) +% i * m)[])

func len*[T](fa: FileArray[T]): int {.inline.} =
  ## Returns length of `fa` in units of T.sizeof records.  Since this does a
  ## divmod, you should save an answer rather than re-calling if appropriate.
  when not defined(danger):
    if fa.nf.m.mem.isNil: Value!"uninitialized FileArray[T]"
    if fa.nf.m.size mod T.sizeof != 0:
      Value!"FileArray[T] size non-multiple of T.sizeof"
  fa.nf.m.size div T.sizeof

func `[]`*[T](fa: FileArray[T], i: int): T {.inline.} =
  ## Returns i-th row of `r` copied into `result`.
  let m = T.sizeof
  when not defined(danger):
    if fa.nf.m.mem.isNil: Value!"uninitialized FileArray[T]"
    if i * m >=% fa.nf.m.size: razIE(i,nf,m)
  cast[ptr T](cast[int](fa.nf.m.mem) +% i * m)[]

func `[]`*[T](fa: var FileArray[T], i: int): var T {.inline.} =
  ## Returns i-th row of `r` copied into `result`.
  let m = T.sizeof
  when not defined(danger):
    if fa.nf.m.mem.isNil: Value!"uninitialized FileArray[T]"
    if i * m >=% fa.nf.m.size: razIE(i,fa.nf,m)
  cast[ptr T](cast[int](fa.nf.m.mem) +% i * m)[]

func `[]=`*[T](fa: FileArray[T], i: int, val: T) {.inline.} =
  ## Assign `val` to `i`-th row of `fa`.
  let m = T.sizeof
  when not defined(danger):
    if fa.nf.m.mem.isNil: Value!"uninitialized FileArray[T]"
    if i * m >=% fa.nf.m.size: razIE(i,nf,m)
  cast[ptr T](cast[int](fa.nf.m.mem) +% i * m)[] = val

proc `$`*[M](x: array[M, char]): string =
  ## Sometimes `x=initFileArray[array[z,char]]("x.NzC"); echo x[i]` is nice.
  let m = M.high + 1
  result.setLen m + 1                 # space for NUL-term for debug builds
  copyMem result[0].addr, x[0].unsafeAddr, m
  result.setLen result[0].addr.cstring.c_strlen

proc mOpen*(tsr: var IOTensor, path: string, mode=fmRead, mappedSize = -1,
            offset=0, newFileSize = -1, allowRemap=false, mapFlags=cint(-1)) =
  let (path, fmt, _) = metaData(path) #XXX Match filled-in IOTensor fields; Add
  tsr.fmt = fmt                       #.. DATA_PATH search; For nOpen add ixing/
  tsr.t = fmt.cols[0].iok             #.. slicing ops/neo/Arraymancer compat.
  if fmt.cols.len != 1: Value ! &"{path}: impure tensors unsupported"
  tsr.m = mf.open(path)
  if tsr.m.size mod fmt.bytes != 0:
    mf.close tsr.m
    Value ! &"{path}: file size indivisible by row size"
  tsr.d = @[ tsr.m.size div fmt.bytes ] & fmt.cols[0].cnts

proc mOpen*(path: string, mode=fmRead, mappedSize = -1, offset=0,
            newFileSize = -1, allowRemap=false, mapFlags=cint(-1)): IOTensor =
  result.mOpen path, mode, mappedSize, offset, newFileSize, allowRemap, mapFlags

proc close*(tsr: var IOTensor) = mf.close(tsr.m)

type #*** INDIRECTION SUBSYSTEM FOR FIXED OR VARIABLE-LENGTH STRING DATA
  Ix* = uint32                  ## max file size for repos is 4 GiB/GiEntry
  RepoKind* = enum rkFixWid,    ## Fixed Width buffers w/key truncation/0-pad
                   rkDelim,     ## A popular format: Disallowed Delimiter Char
                   rkLenPfx     ## An 8-bit clean format: Length-prefixed
  RepoMode* = enum rmFast,      ## Provide ptr->string only; not string->ptr
                   rmIndex,     ## Non-updating bi-directional ptr<->string
                   rmUpdate     ## Updating bi-directional ptr<->string
  Repo* = ref object            ## Mostly opaque string Repository type
    case kind: RepoKind
    of rkFixWid: fmt : IORow
    of rkDelim : dlm : char
    of rkLenPfx: lenK: IOKind
    path: string
    m:    mf.MemFile
    na:   string                # *output* N/A; input is always ""
    mode: RepoMode
    kout: IOKind                # output pointer type for index|updating mode
    f:    File                  # updating mode only fields
    off:  Ix                    # running file size (what to set .[DL] ptrs to)
    tab*: Table[string, Ix]     ## index lookup table
const IxIk = IIk
const IxNA = Ina

iterator keysAtOpen*(r: Repo): (string, Ix) =
  case r.kind
  of rkFixWid:
    var k = newString(r.fmt.bytes)
    for i in countup(0, r.m.size - 1, k.len):
      copyMem k[0].addr, cast[pointer](cast[int](r.m.mem) +% i), k.len
      yield (k[0..^1], Ix(i div r.fmt.bytes)) #COPY
  of rkDelim:
    for ms in mf.memSlices(r.m, r.dlm):
      yield (mf.`$`(ms), Ix(cast[int](ms.data) -% cast[int](r.m.mem)))
  of rkLenPfx:
    var k: string
    var off: uint64; var len: int64
    while off.int < r.m.size - 1:
      let off0 = off.Ix
      convert lIk, r.lenK, len.addr, cast[pointer](cast[uint64](r.m.mem) + off)
      len = len.abs
      off.inc ioSize[r.lenK]
      k.setLen len
      copyMem k[0].addr, cast[pointer](cast[uint64](r.m.mem) + off), len
      off += len.uint64
      yield (k[0..^1], off0)    #COPY

var openRepos*: Table[string, Repo]
proc rOpen*(path: string, mode=rmFast, kout=IxIk, na=""): Repo =
  ## REPOs can be byte-delimited *.Dn* | length-prefixed *.Li* with byte-offset
  ## pointer values or fixed width like *.N16c* with row index vals.  There are
  ## 3 open modes: mmap & go read-only, tab-building indexing, and full updates.
  if path.len == 0: return
  if path in openRepos: return openRepos[path]
  let cols = su.split(path, maxSplit=1)
  let path = cols[0]
  new result
  result.path = path
  if mode != rmFast:
    if cols.len > 1:                    # Schema-visible NOUP, NO_UP..
      result.mode = (if "UP" in cols[1]: rmIndex else: rmUpdate)
    else:                               # $NO_UP can switch off even w/o schema
      result.mode = if getEnv("NO_UP", "xYz") == "xYz": rmUpdate else: rmIndex
  var info: FileInfo
  try: info = getFileInfo(path) except Ce: discard
  let openMode = if result.mode == rmUpdate and info.id.device == 0: fmWrite
                 else: fmAppend
  if info.size > type(result.off).high.int:
    Value ! &"{path}: too big for index pointer sizes"
  result.off = info.size.Ix
  let m = if openMode==fmWrite: mf.MemFile(mem: nil,size: 0) else:
    try: mf.open(path) except Ce: mf.MemFile(mem: nil, size: 0)
  let (_, _, ext) = splitPathName(path)
  if ext.len == 0 or ext == ".Dn":
    result = Repo(kind:rkDelim, dlm: '\n', m: m, na: na, mode: mode, kout: kout)
  elif ext.startsWith(".D"):
    try:
      let dlm = ext[2..^1]
      result = Repo(kind:rkDelim, m: m, na: na, mode: mode, kout: kout,
                    dlm: (if dlm == "n": '\n' else: chr(parseInt(dlm))))
    except Ce: Value ! &"{path}: expecting .Dn or .D<INT>"
  elif ext.startsWith(".L") and ext.len == 3:
    result = Repo(kind:rkLenPfx, lenK: kindOf(ext[2]), m: m, na: na,
                  mode: mode, kout: kout)
  elif ext.startsWith(".N"):
    let (path, fmt, rst) = metaData(cols[0])
    if rst.len > 0: Value ! &"{path}: inappropriate for a repo"
    if not (fmt.cols.len == 1 and fmt.cols[0].iok in {cIk, CIk}):
      Value ! &"{path}: expecting .N<size>[cC]"
    result = Repo(kind:rkFixWid, fmt: fmt, m: m, na: na, mode: mode, kout: kout)
  else: Value ! &"{path}: unknown repo ext {ext}"
  if result.mode == rmUpdate and (result.f := mkdirOpen(path, openMode)) == nil:
    result.mode = rmIndex; erru &"{path}: cannot append; indexing anyway\n"
  for k, i in result.keysAtOpen: result.tab[k] = i
  openRepos[path] = result

proc close*(at: var Repo) =
  if at.m.mem != nil: mf.close(at.m); at.m.mem = nil
  if at.path in openRepos: openRepos.del at.path

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
    if e == nil: Value ! &"no terminating delimiter for {i.int}"
    n = Ix(cast[uint](e) - cast[uint](p))
  of rkLenPfx:                  # Here `i` is a byte offset to length field
    let pL = cast[pointer](cast[uint](at.m.mem) + i)
    convert IxIk, at.lenK, n.addr, pL
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
  else: Value ! "Length Prefix cannot be a float"

proc index*(r: var Repo, ixOut: pointer, k: string, lno: int) =
  template retNA =                      # helper template to return NA
    setNA(r.kout, ixOut); return
  if ixOut == nil: r.close; return
  if r.mode == rmFast: Value ! "rmFast mode does not do index(string)"
  if k.len == 0: retNA                  # NA key -> NA index
  var i: Ix
  let k = if   r.kind == rkFixWid: padClip(k, r.fmt.bytes)
          elif r.kind == rkLenPfx: r.clip(k, r.lenK, lno) else: k
  try: i = r.tab[k]                     # get extant index
  except Ce:                            # novel key
    if r.mode == rmIndex: retNA         # missing && !up -> NA index
    case r.kind                         # update in-memory Table & repo
    of rkFixWid: i = r.tab.len.Ix; r.tab[k] = i; r.f.urite k; r.off = i + 1
    of rkDelim : i = r.off; r.tab[k]=i; r.f.urite k, r.dlm; r.off += Ix(k.len+1)
    of rkLenPfx:  # convert length to output type, write, then write key
      i = r.off; r.tab[k] = i
      r.off += Ix(k.len + ioSize[r.lenK])
      var n = k.len; var nbuf: array[8, char] # IO buffers for length
      convert r.lenK, lIk, nbuf[0].addr, n.addr
      r.f.nurite r.lenK, nbuf[0].addr
      r.f.urite k
    if r.off < i: erru &"pointer overflow for repo {r.path}\n"
  convert r.kout, IxIk, ixOut, i.addr   # convert pointer type

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
  else: Value ! &"{s}: missing '%' or type code"

proc initFormatter*(rowFmt: IORow, atShr: Repo = nil, fmTy: Strings = @[],
                    fmt="", na=""): Formatter =
  result.rowFmt = rowFmt
  result.ats.add atShr                  # [0] is the shared one (or nil if none)
  result.na = na
  var ft: array[IOKind,  string] = ["d", "c", "d", "d", "d", "d", "d", "d",
                                    ".07g", ".016g", ".019g"]
  for cf in fmTy:
    if cf.len < 3: Value ! &"\"{cf}\" too short"
    if cf[1]!='%': Value ! "no '%' between code & spec"
    if cf[0] notin ioCode: Value ! &"'{cf[0]}' not [cCsSiIlLfdg]"
    ft[cf[0].kindOf] = cf[2..^1]
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
    if j >= rowFmt.cols.len: Value ! &"\"{fmt}\" too many output formats"
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

proc formatFloat*(result: var string, value: float64, fmt=".04g") =
  let spec = parseStandardFormatSpecifier(fmt, 0, true)
  formatFloat(result, value, spec.ffmode, spec)

proc formatFloat*(value: float64, fmt: string): string = # E.g. ".4g"
  formatFloat(result, value, fmt)

import std/unicode
proc fmt*(result: var string; fmtr: Formatter; j: int; k: IOKind, s: string,
          naCvt=false) =
  let spec = fmtr.specs[j]
  let radix = fmtr.radix[j]
  if k in ioFloats:                     # SOME FLOAT TYPE
    var value: float64          #XXX port my C float80 to (prs|fmt)BiggestFloat
    convert dIk, k, value.addr, s[0].unsafeAddr, naCvt
    result.formatFloat(value, fmtr.ffmode[j], spec)
  else:
    var value: uint64                   # SOME INTEGRAL TYPE, INCLUDING PTR
    if k.isNA(s[0].unsafeAddr): result.add fmtr.na; return
    convert LIk, k, value.addr, s[0].unsafeAddr, naCvt
    let at = if fmtr.ats[j + 1].isNil: fmtr.ats[0] else: fmtr.ats[j + 1]
    if spec.typ == 's' and at != nil:   # STRING
      var value = at[value.Ix]          # Q: %r & lift for general indirect?
      if spec.precision != -1 and spec.precision < runeLen(value):
        setLen(value, runeOffset(value, spec.precision))
      result.add alignString(value, spec.minimumWidth, spec.align, spec.fill)
    elif k.isSigned:
      let value = cast[int64](value)
      result.add formatInt(value, (if radix == 0: 16 else: radix), spec)
    else:
      result.add formatInt(value, (if radix == 0: 16 else: radix), spec)

proc close*(fmtr: var Formatter) =
  for i in 0 ..< fmtr.ats.len: fmtr.ats[i].close

proc stat*(format="", nios: Strings): int =
  ## print numeric file metadata {like stat(1)}.
  let f = if format.len > 0: format
        else: "name: %n\nrows: %r\nbytes/row: %z\nlastWidth: %w\nlastType: %b\n"
  for path in nios:
    let (path, fmt, _) = metaData(path)
    let sz = getFileSize(if path.len > 0: path else: "/dev/stdin")
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

proc print*(sep="\t", at="", t='x', fmTy: Strings = @[], na="", paths: Strings)=
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
  var atShr: Repo     # `nil` if at == ""
  try: atShr = rOpen(at, na=na) except Ce: erru &"Cannot open \"{at}\"\n"; quit(1)
  if t != 'x' and at.len > 0 and paths.len == 0:
    for (key, ix) in atShr.keysAtOpen: outu key, t
    return
  elif paths.len<1: erru "nio print needs >= 1 path; --help for usage\n"; return
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

#*** VARIOUS CL REFORMATTING/SELECTION TOOLS: rip, zip, cut, tails
proc rip*(input: string, names: Strings): int =
  ## rip apart all columns of `input` into files with given `names`.
  ##
  ## Output numeric formats are either the same as corresponding columns of
  ## input or the type specified in .N/.foo metaData.
  var inp = nOpen(input)
  if inp.rowFmt.cols.len != names.len:
    erru &"too few/many names for input: \"{input}\"\n"; inp.close; return 1
  var outs = newSeq[NFile](inp.rowFmt.cols.len)
  var offs, ns: seq[int]
  var off = 0
  for j in 0 ..< outs.len:                      # open all outputs
    try      : outs[j] = nOpen(names[j], fmWrite)
    except Ce:
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
  var fs = newSeq[NFile](paths.len)     #XXX smarten Re: merging dot files.
  for i, p in paths: fs[i] = nOpen(p)
  var buf: string
  block outer:
    while true:
      for i in 0 ..< fs.len:
        if not fs[i].read(buf):         # stop @first short file
          break outer
        outu buf
  for i in 0 ..< fs.len: fs[i].close

proc widen*(io: Strings, pad='\0'): int =
  ## Widen (/narrow) numeric types or subColumns.
  ##
  ## Egs: `nio w .N7C .N8C` right-pads rows with a 0-byte (shorter truncs)
  ##  `nio w .N9f .N9d` widens float matrix on stdin to double on stdout
  ##  `nio w .N3C5C .NLL | nio p .NLL` -> (24-bit decimal, 40-bit decimal)
  proc c_fputc(c: char, f: File): cint {.importc: "fputc", header: "stdio.h".}
  var i = nOpen(io[0])
  var o = nOpen(io[1], fmAppend)        # fmAppend here ensures nurite works
  template bye = i.close; o.close; return 1
  if i.rowFmt.cols.len != o.rowFmt.cols.len: #XXX COULD become more strict
    erru "`width`: need out & in to have same number of major columns\n"; bye()
  var row, obuf: string
  while i.read row:
    for j, ic in i.rowFmt.cols:
      let oc = o.rowFmt.cols[j]
      if ioSize[ic.iok] == 1 and ioSize[oc.iok] == 1: # Both iok 1-byte
        if oc.width <= ic.width:        # Truncating copy
          discard o.f.uriteBuffer(row[ic.off].addr, oc.width)
        else:                           # Pad
          discard o.f.uriteBuffer(row[ic.off].addr, ic.width)
          for k in 0 ..< oc.width - ic.width: discard c_fputc(pad, o.f)
      elif ic.width == oc.width:        # Convert elements 1-by-1
        obuf.setLen ioSize[oc.iok]
        for k in 0 ..< ic.width:        # Same iok & width => just cp 1-by-1
          convert oc.iok,ic.iok,obuf[0].addr,row[ic.off + k*ioSize[ic.iok]].addr
          discard o.f.uriteBuffer(obuf[0].addr, ioSize[oc.iok])
      elif ioSize[ic.iok] == 1 and oc.width == 1: #XXX COULD collapse fastest..
        var u: uint64                             #  ..varying dim, not all.
        for k in 0 ..< ic.width: u = u or (row[ic.off + k].uint64 shl (8*k))
        obuf.setLen ioSize[oc.iok]      #XXX ^- Handle big-endianness someday
        convert oc.iok, LIk, obuf[0].addr, u.addr
        discard o.f.uriteBuffer(obuf[0].addr, obuf.len)

proc cvtSlice(ab: tuple[a, b: int]; bound: int): (int, int) =
  var (a, b) = ab
  if a < 0: a.inc bound
  if b < 0: b.inc bound
  if b < a: b = a + 1
  (a, min(b, bound))

iterator elts(slices: Strings, bound: int): (int, int) =
  for s in slices: yield cvtSlice(parseSlice(s), bound)

proc cut*(drop: Strings = @[], pass: Strings = @[], path: Strings): int =
  ## pass|drop selected column slices {generalized cut(1)} to stdout.
  ##
  ## Slice specification is `[a][:[b]]`, like Python (incl negatives).  Can
  ## either pass|drop but not both at once.  Multiple slices are "set unioned".
  if path.len != 1 or (drop.len > 0 and pass.len > 0):
    erru "`cut` needs exactly 1 input and not both drop&pass\n"; return 1
  let cPass = int(not (drop.len > 0))
  var inp = nOpen(path[0])
  var colSet: HashSet[int]              #XXX tensors need a flat view
  for (a, b) in (if drop.len > 0: drop else: pass).elts(inp.rowFmt.cols.len):
    for j in a..<b: colSet.incl j
  var buf: string
  var pass, offs, lens: seq[int]
  for j, c in inp.rowFmt.cols:
    if (cPass xor (j in colSet).int) == 0: pass.add j
    offs.add c.off
    lens.add c.width * ioSize[c.iok]
  while inp.read(buf):
    for j in pass:                      # Passing column/field
      if stdout.uriteBuffer(buf[offs[j]].addr, lens[j]) < lens[j]: return 1

proc tailsOuter(nf: NFile, head=0, tail=0, repeat=false): int =
  let m = nf.rowFmt.bytes
  if not nf.m.mem.isNil:
    let n = nf.m.size div m
    if not repeat and head + tail >= n: # head & tail === `cat`
      if stdout.uriteBuffer(nf.m.mem, nf.m.size) < nf.m.size: return 1
    else:
      let head = min(n, head); let tail = min(n, tail)
      if stdout.uriteBuffer(nf.m.mem, m*head) < m*head: return 1
      let adr = cast[pointer](cast[int](nf.m.mem) +% (n - tail) * m)
      if stdout.uriteBuffer(adr, m*tail) < m*tail: return 1
  elif nf.f != nil:
    var hBuf = newString(m * head)      # could do LCM(m, head) in a loop
    if head > 0:
      let nH = nf.f.ureadBuffer(hBuf[0].addr, hBuf.len)
      if nH < 0: return 1
      if stdout.uriteBuffer(hBuf[0].addr, nH) < nH: return 1
      hBuf.setLen nH
    if tail > 0: # unknown row count => row@a time IO into matrix buf
      var tBuf = newString(tail * m)
      let i0 = hBuf.len div m
      var i = i0; var j = 0
      if repeat:
        let n = min(hBuf.len, tBuf.len) # copy needed data from END of hBuf
        tBuf[0..<n] = hBuf[^n..^1]      #..into START of running tBuf.
        j = (n div m) mod tail          # set j as if that was done row-by-row
      while nf.f.ureadBuffer(tBuf[j * m].addr, m) == m:
        i.inc; j.inc
        if j >= tail: j = 0
      let n = min(tail, if repeat: i else: i - i0) - j
      if stdout.uriteBuffer(tBuf[j*m].addr, n*m) < n*m: return 1 # older [j:j+n]
      if stdout.uriteBuffer(tBuf[ 0 ].addr, j*m) < j*m: return 1 # newer [:j]

proc tailsInner(nf: var NFile, head=0, tail=0): int =
  let m = nf.rowFmt.bytes               # write [head:-tail]
  if not nf.m.mem.isNil:
    let n = nf.m.size div m
    if (let amt = n - head - tail; amt > 0):
      let adr = cast[pointer](cast[int](nf.m.mem) +% head * m)
      if stdout.uriteBuffer(adr, amt*m) < amt*m: return 1
  else:
    if tail > 0: # High resource case
      var buf, huge: string             # huge O(input - (head+tail)) buf
      var i = 0                         # final value is total num of rows
      while nf.read(buf):               # unknown row count => row@a time IO
        if i >= head: huge.add buf
        i.inc
      let n = max(0, i - head - tail)
      if huge.len > 0 and stdout.uriteBuffer(huge[0].addr, n*m) < n*m: return 1
    else: # Low resource case; Could use /dev/null to do 2 sendfile's on Linux
      var hBuf = newString(m * head)    # could do LCM(m, head) in a loop
      if head == 0 or nf.f.ureadBuffer(hBuf[0].addr, hBuf.len) == hBuf.len:
        var tBuf = newString(65536)     # switch to big buffer for IO loop
        while true:                     # just read & write until EOF
          let n = nf.f.ureadBuffer(tBuf[0].addr, tBuf.len)
          if n > 0: (if stdout.uriteBuffer(tBuf[0].addr, n) < n: return 1)
          if n < tBuf.len: break

proc tails*(head=0, tail=0, compl=false, repeat=false, paths: Strings): int =
  ## generalized tail(1); Does both head & tail of streams w/o tee FIFO.
  ##
  ## -h10 -t10 will pass *both* head & tail, -h10 -t10 --compl will pass the
  ## "main body" of a distribution (if rows are sorted).  -ch10 =~ tail -n+11.
  for path in paths:
    var nf = nOpen(path) # inner/complementary mode cannot repeat rows
    if compl: (if nf.tailsInner(head, tail) != 0: return 1)
    else    : (if nf.tailsOuter(head, tail, repeat) != 0: return 1)
    nf.close

#*** UTILITY CODE TO GET THINGS IN/OUT OF BINARY FILES: defType, load1, fromSV
proc defType*(names: Strings = @[], lang="nim", paths: Strings): string =
  ## print prog `lang` type defs for NIO rows from extensions in `paths`.
  ##
  ## E.g.: `fooBar.Nif` -> `struct fooBar { unsigned int foo; float bar; };`
  ## These types allow treating memory mapped files as unchecked arrays or ease
  ## row-wise calculation/streamed IO.
  case lang.toLowerAscii:
  of "nim":
    let ntype: array[IOKind, string] = ["int8", "uint8", "int16", "uint16",
      "int32", "uint32", "int64", "uint64", "float32", "float64", "float80"]
    var i = 0
    for path in paths:
      let (_, baseName, _) = path.splitPathName
      let (_, fmt, _) = metaData(path)
      result.add &"type {baseName}" & " {.packed.} = object\n"
      for c in fmt.cols:
       let nm = if i < names.len: names[i] else: "field" & $i
       result.add &"    {nm}: "
       if   c.cnts.len > 2 :
        result.add &"array[..,arr[{c.cnts[1]},arr[{c.cnts[0]},{ntype[c.iok]}]]]"
       elif c.cnts.len == 2:
        result.add &"array[{c.cnts[1]}, array[{c.cnts[0]}, {ntype[c.iok]}]]"
       elif c.cnts[0] > 1  : result.add &"array[{c.cnts[0]}, {ntype[c.iok]}]"
       else                : result.add &"{ntype[c.iok]}"
       result.add "\n"
       i.inc
  of "c":
    let ctype: array[IOKind, string] = ["signed char", "unsigned char",
      "short", "unsigned short", "int", "unsigned int", "long", "unsigned long",
      "float", "double", "long double"]
    var i = 0
    for path in paths:
      let (_, baseName, _) = path.splitPathName
      let (_, fmt, _) = metaData(path)
      result.add &"struct {baseName} " & "{\n"
      for c in fmt.cols:
        let nm = if i < names.len: names[i] else: "field" & $i
        result.add &"    {ctype[c.iok]} {nm}"
        if   c.cnts.len > 2 : result.add &"[...][{c.cnts[1]}][{c.cnts[0]}]"
        elif c.cnts.len == 2: result.add &"[{c.cnts[1]}][{c.cnts[0]}]"
        elif c.cnts[0] > 1  : result.add &"[{c.cnts[0]}]"
        result.add ";\n"
        i.inc
      result.add "} __attribute__((__packed__));\n"
  else: erru &"unknown programming language \"{lang}\"\n"; return ""

import std/parseutils
type Transform = proc(ixOut: pointer, inp: string, lno: int)

proc parse(s: MSlice; pathName: string; lno: int; colName: string; inCode: char;
           kout: IOKind; xfm: Transform; cnt=1; outp=stdout) {.inline.} =
  let s = s.strip                       #XXX could be conditional
  var nS, eoNum: int                    # widest types for number parsing
  var nU: uint
  var nF: float
  var obuf: array[16, char]             # actual output buffer

  template p(fn, n, k, u) =
    n = cast[type(n)](s.fn(eoNum))
    if eoNum == s.len:
      let lo = type(n)(`low u`[kout.int shr 1])
      let hi = type(n)(`high u`[kout.int shr 1])
      if n < lo: erru &"{pathName}:{lno} \"{$s}\" underflows\n"; n = lo
      if n > hi: erru &"{pathName}:{lno} \"{$s}\" overflows\n" ; n = hi
      convert kout, k, obuf[0].addr, n.addr
    else:
      setNA kout, obuf[0].addr
      erru &"stdin:{lno}: col:{colName} ic:{inCode} oc:{kout} !=~ \"{$s}\""

  if s.len == 0:                        # empty string ==> N/A
    setNA kout, obuf[0].addr; outp.nurite kout, obuf[0].addr
    return
  case inCode
  of 'c':
    let pc = padClip($s, cnt)
    discard outp.uriteBuffer(pc[0].unsafeAddr, cnt)
    return
  of 'b': (if kout.isSigned: p(parseBin,nS, lIk,S) else: p(parseBin,nU, LIk,U))
  of 'o': (if kout.isSigned: p(parseOct,nS, lIk,S) else: p(parseOct,nU, LIk,U))
  of 'd': (if kout.isSigned: p(parseInt,nS, lIk,S) else: p(parseInt,nU, LIk,U))
  of 'h': (if kout.isSigned: p(parseHex,nS, lIk,S) else: p(parseHex,nU, LIk,U))
  of 'f':
    nF = s.parseFloat(eoNum)
    if eoNum == s.len: convert kout, dIk, obuf[0].addr, nF.addr
    else: setNA kout, obuf[0].addr; erru &"stdin:{lno}: bad fmt \"{$s}\""
  of 'x': xfm obuf[0].addr, $s, lno
  else: IO ! &"{inCode}: inCode not in [bodhfx]"
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
  else: Value!"unknown transformer prefix"

proc load1*(inCode, oCode: char; delim='\n', xform="", count=1): int =
  ## load 1 ASCII column on stdin to NIO on stdout (for import,testing).
  let kout = kindOf(oCode)
  var xfm: Transform
  if inCode == 'x': xfm = initXfm(inCode, kout, xform)
  var lno = 1
  for (cs, n) in stdin.getDelims(delim):
    let n = if n > 0 and cs[n-1] == delim: n - 1 else: n
    MSlice(mem: cs, len: n).parse "stdin",lno, "COL0", inCode, kout, xfm, count
    lno.inc
  if inCode == 'x': xfm nil, "", 0      # tell Transform to close

proc maybeAppend(path: string): FileMode = # modes maybe weird from Windows
  var info: FileInfo
  try: info = getFileInfo(path) except Ce: discard
  result = if info.id.device == 0: fmWrite else: fmAppend

from cligen/argcvt import unescape  # pulls in much, but impacts comptime little
type                                #*** SHARED STATE FOR  parseCol/Sch/fromSV
  SchState = tuple[pp, outBase, outType, shrd, nameSep, schema: string;
                   doZip, onlyOut: bool; nHdr, mxLg, lno: int; dlm: char;
                   xfm0: Transform; stab: StringTableRef]
  SchCol = tuple[name: string; inCode: char; f: File; xfm: Transform;
                 kout: IOKind; count: int]  # A parsed schema column

proc parseCol(scols: seq[string], ss: var SchState): SchCol =
  if scols.len < 3: Value ! &"{ss.schema}:{ss.lno} < 3 cols"
  result.name = scols[0]
  result.kout = kindOf(scols[1][^1])
  result.inCode = scols[2][0]
  result.count = if scols[1].len > 1: parseInt(scols[1][0..^2]) else: 1
  if ss.doZip:                           # stdout zipped mode
    result.f = stdout
    if ss.outBase.len > 0: ss.outBase.add ss.nameSep
    ss.outBase.add scols[0]
    ss.outType.add scols[1]
  else:
    let path = scols[0] & ".N" & scols[1]
    if not ss.onlyOut: result.f = open(path, path.maybeAppend)
  if scols.len > 3:
    if not ss.onlyOut:
      if scols[3].startsWith("@@"):
        if ss.xfm0 == nil:
          ss.xfm0 = initXfm(result.inCode, result.kout, "@" & ss.shrd)
          result.xfm = ss.xfm0
        else: result.xfm = ss.xfm0
      else: result.xfm = initXfm(result.inCode, result.kout, scols[3] % ss.stab)
  elif result.inCode == 'x': Value ! "'x' but no transform arguments"

proc parseSch(schema,nameSep,dir,reps: string; onlyOut: bool): (SchState,
                                                                seq[SchCol]) =
  result[0].schema  = schema
  result[0].shrd    = "strings"
  result[0].stab    = {"REPS": reps}.newStringTable
  result[0].shrd    = "strings"
  result[0].nameSep = nameSep
  result[0].onlyOut = onlyOut
  result[0].nHdr    = 1
  result[0].mxLg    = 100
  let sep = initSep("white")
  var scols: seq[string]
  var didCd = false
  var ign: SchCol               # Tuple of empty GC'd types can be moved safely.
  for line in lines(schema):
    if not didCd and dir.len > 0:       # Done in loop so users need no special
      discard existsOrCreateDir(dir)    #..instructions Re: schema path.
      setCurrentDir dir; didCd = true
    inc result[0].lno
    let line = line.commentStrip
    let mcols = line.split({'=', ':'})  # parse schema-level arg
    let sarg = if mcols.len > 1: mcols[1] else: ""
    if   line.len == 0: continue
    elif line.startsWith("--preproc"): result[0].pp    = sarg
    elif line.startsWith("--delim"  ): result[0].dlm  = argcvt.unescape(sarg)[0]
    elif line.startsWith("--nHeader"): result[0].nHdr  = parseInt(sarg)
    elif line.startsWith("--maxLog" ): result[0].mxLg  = parseInt(sarg)
    elif line.startsWith("--zip"    ): result[0].doZip = true
#   elif line.startsWith("--dotfs"  ): result[0].dotfs = true #XXX auto dotfiles
    elif line.startsWith("--shared" ): result[0].shrd  = sarg % result[0].stab
    else:
      sep.split(line, scols, n=4)
      if scols.len > 0:
        result[1].add if scols[0]=="_": ign.move else: scols.parseCol(result[0])

proc fromSV*(schema="", nameSep="", dir="", reps="", onlyOut=false,
             SVs: Strings): int =
  ## parse *strict* separated values on stdin|SVs to NIO files via schemas.
  ##
  ## An example schema file (with default values but for `outSfx`):
  ##   --preproc=gzip -d $1|c2tsv   # popen() preproc to make strict TSV
  ##   --delim=\\0                   # NUL is c2tsv output delimiter
  ##   --nHeader=1                  # number of rows which are headers
  ##   --maxLog=100                 # limit same-kind log messages
  ##   --zip                        # stdout zip mode not default rip mode
  ##   --shared=strings.LS          # name of any common strings file
  ##   #name NC SC TRANSFORM:args   # NC=NIOcode; SC=(load|src)Code like load1
  ##   qty   i  d                   # parse input as decimal; emit uint32
  ##   Px    f  f                   # parse as float32; emit float32
  ##   _     ignore                 # ignore an input column
  ##   Id    8C c                   # embedded char arrays pad-clipped but can
  ##   Date  s  x  @dates.N9c       #..be transformed via intern into *fixed*
  ##   City  s  x  @cities.Dn       #..or *variable width* repositories,
  ##   Note  i  x  @@               #..with maybe a shared common string repo.
  ## create/appends *qtyPxIdDateCityNote.Nif8C2si*, *dates.N9c*, *cities.Dn*
  ## and length-prefixed *strings.LS*.  $REPS or ${REPS} is interpolated into
  ## both the argument of --shared and post `x` schema columns. Since this makes
  ## & CHDIRs into `dir` if non-empty, library callers may want save & restore.
  if schema.len == 0: erru "Provide a schema maybe from `inferT`\n"; return 1
  let (ss, cols) = parseSch(schema, nameSep, dir, reps, onlyOut)
  if ss.lno == 0: erru "Empty schema; Provide a real one\n"; return 1
  if onlyOut:
    if ss.doZip: outu ss.outBase, ".N", ss.outType, '\n'
    else: erru &"--onlyOut makes sense only for --zip schemas\n"
    return
  for path in SVs:                      # Now the actual parsing part!
    var lno = 0
    let inpFile = ss.pp.popenr(path)
    for (cs, n) in getDelims(inpFile):
      lno.inc
      if lno > ss.nHdr:                 # skip however many header rows
        var cix = 0
        for s in MSlice(mem: cs, len: n).mSlices(ss.dlm):
          if cix == cols.len:
            erru &"{path}:{lno}: ignoring columns past {cols.len + 1}\n"
            break
          let c = cols[cix]
          if c.f != nil:
            s.parse path, lno, c.name, c.inCode, c.kout, c.xfm, c.count, c.f
          cix.inc
    discard inpFile.pclose(ss.pp)
  for c in cols:
    if c.xfm != ss.xfm0 and c.xfm != nil: c.xfm nil, "", 0 # close `Transform`s
    if c.f != nil and c.f != stdout: c.f.close
  if ss.xfm0 != nil: ss.xfm0 nil, "", 0 # close common `Transform`

proc inferT*(ext=".sc", pre="", delim="\x00", nHdr=1, timeFmts: Strings = @[],
         iType='i', fType='f', sType="i.Dn", guess="f\tf", SVs: Strings): int =
  ## infer (approximate 4-type) schemas from strict TSV (e.g. from c2tsv)
  ##
  ## Types are assigned in (depending on consistent parsability as such) the
  ## order: [time, int, float, string] where (time means each format, in order).
  ##
  ## E.g.: `nio i -p'c2tsv<$1' foo.csv; nio f -sfoo.sc foo.csv` infers&parses.
  let sIType = sType[0]; let sExt = sType[1..^1]  #XXX error check
  var hdrs: seq[string]
  var i: BiggestInt
  var f: BiggestFloat
  var empty, iOk, fOk, tOk: seq[int]
  let m = timeFmts.len
  proc zero(x: var seq[int], m=1) = x.setLen 0; x.setLen hdrs.len*m
  proc check(s: string, j: int) =
    if s.len > 0:
      for k, timeFmt in timeFmts:
        try:
          discard parse(s, timeFmt)
          tOk[m*j + k].inc
        except Ce: discard
      if s.parseBiggestInt(i)   == s.len: iOk[j].inc
      if s.parseBiggestFloat(f) == s.len: fOk[j].inc
    else: empty[j].inc

  for path in SVs:
    var lno = 1
    let inpFile = pre.popenr(path)
    for row in lines(inpFile):
      if lno == 1:
        hdrs = row.split(delim); tOk.zero m; iOk.zero; fOk.zero; empty.zero
      if lno > nHdr:                    # skip however many header rows
        var j = 0
        for inp in row.split(delim):
          if j + 1 > hdrs.len: IO ! &"too many columns >{j+1} @line:{lno}"
          su.strip(inp).check j
          j.inc
        if j < hdrs.len: IO ! &"too few columns {j} @line:{lno}"
      lno.inc
    discard inpFile.pclose(pre)
    let o = open(path & ext, fmWrite)
    let pp = if pre.len > 0: &"\n--preproc={pre}     # preprocess" else: ""
    o.write &"""
# This is a parsing schema for nio fromSV.
--nHeader={nHdr}                      # number of rows which are headers{pp}
--delim={delim}                      # NUL is c2tsv output delimiter
#name	NC	SC TRANSFORM:args # NC=NIOcode;SC=(scan|src)Code like scan1
"""
    for j, hdr in hdrs:
      o.write hdr, "\t"
      var isT = false
      for k, timeFmt in timeFmts:
        if lno - 1 - nHdr - empty[j] == tOk[j] and tOk[j] > 0:
          o.write "l\tx\tT" & timeFmt; isT = true; break
      if not isT:
        if lno - 1 - nHdr - empty[j] == iOk[j] and iOk[j] > 1:
          o.write iType, "\td"    # assume decimal
        elif lno - 1 - nHdr - empty[j] == fOk[j] and fOk[j] > 1:
          o.write fType, "\tf"
        elif lno - 1 - nHdr == empty[j]: o.write guess
        else: o.write &"{sIType}\tx @{hdr}{sExt}"
      o.write '\n'

proc xsum*(outKind='d', paths: Strings): int =
  ## total zipped rows to stdout; Missing tails treated as zero
  if paths.len == 0: return 0 #NOTE: An easy but not useless example calculation
# if stdout.isatty:                     # block accidental tty corruption
#   erru "stdout is a terminal; pipe | redirect\n"; return 1
  var nfs: seq[NFile]                   # open inputs
  var nRow = 0
  let kout = kindOf(outKind)
  for path in paths:
    nfs.add nOpen(path)
    nRow = max(nRow, nfs[^1].len)
  var obuf: array[16, char]             # actual output buffer
  for i in 0..<nRow:
    var num, sum: float64               # It'd be more correct/intuitive to sum
    for nf in mitems(nfs):              #..in kout arith, not always float64.
      for c in nf.rowFmt.cols:
        for e in 0 ..< c.width:
          if nf.read(num): sum += num   # read & total each entry as a float64
    convert kout, dIk, obuf[0].addr, sum.addr
    stdout.nurite kout, obuf[0].addr    # convert & then emit row sum as `kout`.
  for nf in mitems(nfs): nf.close

when defined(useAdix):                  #*** NOT TOTALLY USELESS DEMO: moments
  import adix/mvstat
  type RunningStat = MovingStat[float32,uint32]
else: import std/stats                  # fall back to slow RunningStat

type MomKind = enum mkN="n", mkMin="min", mkMax="max", mkSum="sum",
                    mkAvg="avg", mkSdev="sdev", mkSkew="skew", mkKurt="kurt"

proc fmtStat(rs: RunningStat, mk: MomKind, fmt: string): string =
  case mk
  of mkN:    rs.n.float64        .formatFloat(fmt)
  of mkMin:  rs.min              .formatFloat(fmt)
  of mkMax:  rs.max              .formatFloat(fmt)
  of mkSum:  rs.sum              .formatFloat(fmt)
  of mkAvg:  rs.mean             .formatFloat(fmt)
  of mkSdev: rs.standardDeviation.formatFloat(fmt)
  of mkSkew: rs.skewness         .formatFloat(fmt)
  of mkKurt: rs.kurtosis         .formatFloat(fmt)

proc moments*(fmt=".4g", stats: set[MomKind]={mkMin,mkMax}, paths:Strings): int=
  ## print selected moments over all columns of all `paths`.
  for path in paths:                    # NOTE: This is intended as an easy,
    var inp = nOpen(path)               #..but not useless example calculation.
    when defined(useAdix): # Optimize 2 VERY special cases; Really just a demo.
      if inp.rowFmt.cols.len == 1 and inp.rowFmt.cols[0].cnts == @[1] and
         inp.rowFmt.cols[0].iok == fIk and stats == {mkMin,mkMax,mkAvg,mkSdev}:
        let p = cast[ptr UncheckedArray[float32]](inp.m.mem)
        outu path, ":0 ", basicStats(toOpenArray[float32](p,0,inp.len-1)), "\n"
        inp.close; continue
      elif inp.rowFmt.cols.len == 1 and inp.rowFmt.cols[0].cnts == @[1] and
         inp.rowFmt.cols[0].iok == dIk and stats == {mkMin,mkMax,mkAvg,mkSdev}:
        let p = cast[ptr UncheckedArray[float64]](inp.m.mem)
        outu path, ":0 ", basicStats(toOpenArray[float64](p,0,inp.len-1)), "\n"
        inp.close; continue
    var sts = newSeq[RunningStat](inp.rowFmt.cols.len)
    var num: float
    block fileLoop:
      while true:
        for j in 0 ..< sts.len:
          if not inp.read(num): break fileLoop
          if not num.isNaN: sts[j].push num
    for j in 0 ..< sts.len:
      outu path, ":", j
      for mk in [mkN, mkMin, mkMax, mkSum, mkAvg, mkSdev, mkSkew, mkKurt]:
        if mk in stats: outu " ", $mk, ": ", fmtStat(sts[j], mk, fmt)
      outu "\n"
    inp.close

proc up(cell: var seq[(int, seq[RunningStat])],
        rec: seq[(int, seq[float])], g: int64) {.inline.} =
  if cell.len == 0:                     # establish indexing layout.  Only..
    cell.setLen rec.len                 #..run for first row of a given group.
    for i in 0 ..< rec.len:
      cell[i][0] = i
      cell[i][1].setLen rec[i][1].len
      for j in 0 ..< rec[i][1].len:
        cell[i][1][j].clear
  for i in 0 ..< rec.len:               # update each running stat for this rec
    for j in 0 ..< rec[i][1].len:
      cell[i][1][j].push rec[i][1][j]

proc kreduce*(fmt=".4g", group: string, stats: set[MomKind] = {mkMin, mkMax},
              na=0.0, paths: Strings): int =
  ## keyed-reduce (*by group*); Right now like `moments` but per-`group`-key.
  var grpFil: NFile
  try: grpFil = nOpen(group) except Ce: Value ! &"cannot open file {group}"
  var inps: seq[NFile]                  # Open all the inputs
  for path in paths: inps.add nOpen(path)
  var g: int64                          # Integer group label
  var row: seq[float]                   # Per inp file associated data
  var rec: seq[(int, seq[float])]       # All associated data
  let empty: seq[(int, seq[RunningStat])] = @[ ]
  var gstats: Table[int64, seq[(int, seq[RunningStat])]]
  block fileLoop:                       # Breaks at whichever comes first:
    while true:                         #.. end of group or shortest file.
      var num: float
      if not grpFil.read(g): break fileLoop
      rec.setLen 0
      for i in 0 ..< inps.len:
        row.setLen 0
        for j in 0 ..< inps[i].rowFmt.cols.len:
          if not inps[i].read(num): break fileLoop
          row.add if num.isNaN: na else: num
        rec.add (i, row[0..^1]) #COPY
      gstats.mgetOrPut(g, empty).up rec, g
  for i in 0 ..< inps.len: inps[i].close
  for g, pathIxStats in gstats:
    outu &"{g}:"
    for (pathIx, sts) in pathIxStats:
      outu &" {paths[pathIx]}:"
      for st in sts:
        for mk in [mkN, mkMin, mkMax, mkSum, mkAvg, mkSdev, mkSkew, mkKurt]:
          if mk in stats: outu " ", $mk, ": ", fmtStat(st, mk, fmt)
    outu "\n"

type #*** MICRO "GROUPBY" FRAMEWORK, BUT USER'S LIKELY WANT THEIR OWN
  Grp*[K,V] = object
    ks*: FileArray[K] ## For nio q -b'var g=grp[array[9,char],float]("id.N9C")'
    vs*: seq[V]       ## 'g.up id,`+=`,v' -eg.report id.Ni v.Nf

proc grp*[K,V](path: string): Grp[K,V] =
  result.ks = initFileArray[K](path)

template up*[K,V](g: Grp[K,V], id, op, val) =
  if id >= g.vs.len: g.vs.setLen id + 1 # ensure room
  op g.vs[id], val                      # incremental update

proc grpInclude*[V: SomeNumber](v: V): bool = v != V(0)

proc `$`*[K,V](g: Grp[K,V]): string =
  for id, v in g.vs:
    if v.grpInclude:
      result.add $g.ks[id] & " " & $v & (if id + 1 < g.vs.len: "\n" else: "")

type #*** A DEFAULT SORT, MOST USEFUL (BUT ALSO AWKWARD) FOR STRING KEYS
  Comparator = object #*** (RADIX SORTS ARE BETTER FOR NUMBERS)
    nf:  NFile          # backing nio file
    at:  Repo           # indirect repository or nil if direct
    iok: IOKind         # IO type
    dir: bool           # embedded char array; use direct memcmp
    off: int            # byte offset within a row
    width: int          # length for memcmp
    sgn: int            # sense of comparison; +1 ascending; -1 descending

func compare(cmp: Comparator; i, j: int): int =
  let a = cast[pointer](cast[int](cmp.nf[i]) +% cmp.off)
  let b = cast[pointer](cast[int](cmp.nf[j]) +% cmp.off)
  if cmp.dir: cmpMem a, b, cmp.width
  elif cmp.at.isNil: compare cmp.iok, a, b
  else:                 # indirect case
    var ix, jx: Ix
    convert IxIk, cmp.iok, ix.addr, a
    convert IxIk, cmp.iok, jx.addr, b
    cmp cmp.at[ix], cmp.at[jx]

func compare(cmps: openArray[Comparator]; i, j: int): int =
  for k, cmp in cmps:
    let c = cmp.compare(i, j)
    if c != 0: return cmp.sgn * c

proc add(r: var seq[Comparator]; nf: NFile, fmt: string, atShr: Repo) =
  var cmp: Comparator # parse {[@[..]]+-N}* and add Comparators to running list
  cmp.nf  = nf
  cmp.sgn = +1
  if fmt.len == 0:
    for c in nf.rowFmt.cols:
      if c.cnts.len>1 or (c.cnts[0]!=1 and c.iok!=CIk): Value!"key too complex"
      cmp.iok = c.iok
      cmp.off = c.off
      cmp.width = c.width
      cmp.dir = cmp.iok == CIk and c.width > 1
      r.add cmp.move; cmp.nf = nf
  else:
    if fmt[0] != ':':
      erru &"expecting :<1-origin-colNum>[,...] not {fmt}\n"; return
    for spec in fmt[1..^1].split(","):
      cmp.at = nil
      var str = spec
      if spec.startsWith("@"):
        let ix = spec.find({'0'..'9', '-', '+'}, start=1)
        cmp.at = if ix == 1: atShr else: rOpen(spec[1..<ix])
        if ix > 1: str = spec[ix..^1]
      elif spec.len == 0:
        erru &"ignoring len==0 comma-separated specifier in: {fmt}\n"; continue
      var colNum: int           # "num" is 1-origin; "ix" is 0-origin
      if str.parseInt(colNum) != str.len or colNum == 0:
        erru &"ignoring 0/non-integral specifier {colNum} in: {fmt}\n"; continue
      let cIx = abs(colNum) - 1
      cmp.sgn = sgn(colNum)
      cmp.iok = nf.rowFmt.cols[cIx].iok
      cmp.off = nf.rowFmt.cols[cIx].off
      cmp.width = nf.rowFmt.cols[cIx].width
      cmp.dir = cmp.iok == CIk and nf.rowFmt.cols[cIx].width > 1
      r.add cmp.move; cmp.nf = nf

import std/algorithm
proc order*(at="", output: string, paths: Strings): int =
  ## write indices to make a multi-level order among `paths`.
  ##
  ## :commaSep{[@[..]]INT} after each input means cmp by that *1-Origin*-column
  ## (within each file); Negative => reverse. *@[..]* prefix indicates indirect
  ## values with shared *at* | column-specific repos.  Eg., ``at="bs.LS"``,
  ## ``a,b,c.Niif:@-2,@as.LS+1,-3`` makes a multi-level order first descending
  ## by `bs[]`, then ascending by `as[]`, then descending by `c`.
  var atShr: Repo
  try      : (if at.len > 0: atShr = rOpen(at))         # `nil` if at == ""
  except Ce: erru &"Cannot open \"{at}\"\n"; quit(1)
  let m = paths.len
  var nfs = newSeq[NFile](m)
  var cmps = newSeqOfCap[Comparator](m)
  var ofmt: string
  var n = -1
  for j, path in paths:                                 # open the inputs
    nfs[j] = nOpen(path, rest=ofmt.addr)
    if n == -1: n = nfs[j].len
    elif nfs[j].len != n:
      erru &"size mismatch: {path}.len = {nfs[j].len} != earlier {n}\n"
      return 1
    cmps.add(nfs[j], ofmt, atShr)                       # build comparators
  var index = newSeqOfCap[int](n)                       # make identity map
  for i in 0..<n: index.add i                           # could be FileArray?
  index.sort (proc(i, j: int): int = cmps.compare i, j) # do the sort
  index.save output                                     # save the answer

proc emerge*(prefix="_", order: string, paths: Strings): int =
  ## materialize data in (random access) `paths` according to `order`.
  ##
  ## Output column/path structure is identical to input.  Since this routine
  ## proceeds input-by-input for efficiency, early exit/cancellation yields
  ## partly populated outputs.
  var order = initFileArray[int](order) #Q: maybe int64
  let n = order.len; let high = n - 1
  let m = paths.len
  var inps = newSeq[NFile](m)
  var outs = newSeq[NFile](m)
  var size = newSeq[int](m)
  for j, p in paths:
    inps[j] = nOpen(p)
    size[j] = inps[j].rowFmt.bytes
    if inps[j].len != n:
      erru &"size mismatch: {p}.len = {inps[j].len} != order.len={n}\n"
      return 1
  for j, p in paths:
    outs[j] = nOpen(prefix & p, fmReadWrite, newFileSize = inps[j].m.size)
  for j in 0 ..< m:
    for i in 0 ..< n:
      if order[i] > high:   #Q: conditionalize on defined(danger)?
        erru &"order[{i}] = {order[i]} out of bounds [0..{high}]\n"
        return 1
      copyMem outs[j][i], inps[j][order[i]], size[j]
  for j in 0 ..< m:
    inps[j].close; outs[j].close
  order.close

#*** HIGHER ORDER STRUCTURE APIS/TIME-SERIES MATRICES
type AxisFile* = object     ## `AxisFile` is an NA-impossible `rkFixWid Repo`
  ix2k*: NFile              ## file of keys such that file[ix][0..<mKy] = key
  k2ix*: Table[string, int] ## map from key to its file[index]
  fK*: File                 ## ordinary File for appending to axis file

func nKy*(af: AxisFile): int {.inline.} = af.k2ix.len
  ## number of keys (in rows)
func mKy*(af: AxisFile): int {.inline.} = af.ix2k.width
  ## width of keys (bytes)
func `[]`*(af: AxisFile, key: string): int {.inline.} = af.k2ix[key]
func contains*(af: AxisFile,key: string): bool {.inline.} = key in af.k2ix

proc add*(af: var AxisFile, key: string) =
  ## add `key` to an open axis index file
  if af.fK.writeBuffer(key[0].unsafeAddr, af.mKy) != af.mKy:
    IO!"cannot append; disk full?"
  af.k2ix[key] = af.nKy

proc axOpen*(path: string, fixed=false): AxisFile =
  ## Create|open an axis index file & populate inverted map Table
  if not fixed:
    result.fK = open(path, fmAppend) # Creates if does not exist on Unix
  result.ix2k = nOpen(path)
  for i, k in pairs(result.ix2k):
    result.k2ix[k] = i

proc close*(af: var AxisFile) =
  if not af.fK.isNil: af.fK.close
  af.ix2k.close

type StacksUpdater* = object
  axT*, axI*: AxisFile          ## axis indices for "time" & "identity"
  nT*, nI*, padT*, padI*: int   ## matrix dimensions & padding amounts
  fixed*: bool                  ## fixed = read-only on axis indices
  pfx*, idVar*: string          ## time is in path names; is in data files
  wd*, tiDir*, itDir*: string   ## work & out dirs; ""=> skip that one.
  inpVars*: seq[string]         ## input variables to matricize (full name)
  outBase*: seq[string]         ## output variables (basenames; no .N*)
  vI*, vO*, vX*: seq[NFile]     ## output matrix handles

func roundUp(num, factor: int): int =
  result = num
  if factor > 1:
    let remainder = num mod factor
    if remainder != 0: result = num + factor - remainder

proc known(todo: seq[string]; af: AxisFile; ask, pad: int): int =
  result = af.nKy               # Calc total unique keys; pad if `ask` too small
  for k in todo: (if k notin af: inc result)
  result = if ask >= result: ask else: roundUp(result, pad)

proc suOpen*(idVar: string; tms,ids: seq[string]; nT,nI, padT,padI: int;
             ix2tm,ix2id, tiDir,itDir, wd: string; fixed=false): StacksUpdater =
  template initAx(K, ky, kys, name) =   # OPEN AXIS INDEX FILES
    try      : result.`ax K` = axOpen(`ix2 ky`, fixed)
    except Ce: erru "cannot open ", name, " AxisFile\n"; return
    result.`pad K` = `pad K`
    result.`n K` = kys.known(result.`ax K`, `n K`, result.`pad K`)
  initAx(T, tm, tms, "time")
  initAx(I, id, ids, "id")
  result.fixed = fixed          # read-only on indices mode or not
  result.wd = wd
  result.idVar = idVar
  result.tiDir = tiDir          # defer create/open `vO` until 1st ids batch
  result.itDir = itDir          # defer create/open `vX` until 1st ids batch

proc getVars(su: var StacksUpdater, wd: string) =
  for path in walkPatSorted(wd/"*.N*"):
    if su.idVar notin path: su.inpVars.add path
  for path in su.inpVars:
    let (_, name, _) = splitPathName(path)
    su.outBase.add name

proc extantAxSpc(su: var StacksUpdater): (int, int) =
  if su.tiDir.len > 0:
    let pat = su.tiDir/su.outBase[0] & ".N*" & su.inpVars[0][^1]
    let paths = walkPatSorted(pat)
    if paths.len > 0:
      return (int(paths[0].getFileSize) div metaData(paths[0])[1].bytes,
              metaData(paths[0])[1].cols[0].cnts[0])
  if su.itDir.len > 0:
    let pat = su.itDir/su.outBase[0] & ".N*" & su.inpVars[0][^1]
    let paths = walkPatSorted(pat)
    if paths.len > 0:
      return (metaData(paths[0])[1].cols[0].cnts[0],
              int(paths[0].getFileSize) div metaData(paths[0])[1].bytes)

proc maybeTouch(su: var StacksUpdater, vQ: seq[NFile], dir: string, m: int) =
  for v, nf in vQ:
    let pat = (dir / su.outBase[v] & ".N*") & ioCode[nf.rowFmt.cols[0].iok]
    let paths = walkPatSorted(pat)
    if paths.len > 0: touch paths[0]

proc ensureSpace(su: var StacksUpdater; vQ: var seq[NFile];
                 n, m: int; dir: string) =
  createDir(dir)
  if vQ.len == 0:       # n*m spc in vQ; Future IOTensor work can simplify this
    for v, vi in su.vI:
      let pat = (dir / su.outBase[v] & ".N*") & ioCode[vi.rowFmt.cols[0].iok]
      let paths = walkPatSorted(pat)
      if paths.len == 1:
        vQ.add nOpen(paths[0], fmReadWrite)
      elif paths.len > 1: erru "nmatch != 1 for {pat}\n"
  if vQ.len > 0:
    let tst = vQ[0]
    if tst.len >= n and tst.width >= m * ioSize[tst.rowFmt.cols[0].iok]:
      return            # assume if 1st big enough then rest are
  erru &"growing space to {n},{m} in {dir}\n" # slow IO op; so tell user
  var vN: seq[NFile]
  for v, vi in su.vI:                         # on only 1 of the 2 axes.
    let iok = vi.rowFmt.cols[0].iok
    let sz = ioSize[vi.rowFmt.cols[0].iok]    # size of base type
    let tmp = dir/("tmp_" & su.outBase[v] & ".N") & $m & ioCode[iok]
    vN.add nOpen(tmp, fmReadWrite, n*m*sz)
    let oldN = if vQ.len > 0: vQ[v].len else: 0
    let oldM = if vQ.len > 0: vQ[v].width div sz else: 0
    for i in 0 ..< oldN:
      for j in 0 ..< oldM:
        copyMem cast[pointer](cast[int](vN[^1][i]) +% j*sz),
                cast[pointer](cast[int](vQ[v][i]) +% j*sz), sz
      for j in oldM ..< m:
        setNA iok, cast[pointer](cast[int](vN[^1][i]) +% j*sz)
    for i in oldN ..< n:
      for j in 0 ..< m:
        setNA iok, cast[pointer](cast[int](vN[^1][i]) +% j*sz)
    let nw = (dir/su.outBase[v]&".N") & $m & ioCode[iok]
    if oldM != m:
      let p = (dir/su.outBase[v] & ".N") & $oldM & ioCode[iok]
      removeFile p
    try: moveFile tmp, nw except Ce: discard
    if vQ.len > 0: vQ[v].close
  vQ = vN

proc wdExpand(wd, tm: string): string = wd.replace("@TM@", tm)

proc update*(su: var StacksUpdater, tm: string): int =
  ## Returns number of rows incorporated into matrices
  var t: int                    # FIRST DECIDE NEEDED tm INDEX
  try      : t = su.axT[tm]
  except Ce:
    if su.fixed: erru &"ix2tm: no \"{tm}\" and static ix; skipping\n"; return 0
    else: t = su.axT.nKy        # `add` POST-UPDATE TO MARK COMPLETION
  var ids: NFile                # ids <- OPEN idVar
  try      : ids = nOpen(su.wd.wdExpand(tm)/su.idVar)
  except Ce: erru &"no id file: {su.wd.wdExpand(tm)/su.idVar}!\n"; return 0
  if not su.fixed:              # UPDATE Id AXIS
    for id in ids: (if id notin su.axI: su.axI.add id)
    su.axI.fK.flushFile
  if su.nI < su.axI.nKy:
    su.nI = roundUp(su.axI.nKy, su.padI)
  if su.inpVars.len==0: su.getVars su.wd.wdExpand(tm) # OPEN INPUT RIP/VEC FILES
  let (xT, xI) = su.extantAxSpc
  su.nT = max(su.nT, xT)        # Leave alone user-over provisioned
  su.nI = max(su.nI, xI)
  for v in su.inpVars:
    try: su.vI.add nOpen(v) except Ce: return 0
    if su.vI[^1].rowFmt.cols.len > 1 or
       su.vI[^1].rowFmt.cols[0].width > ioSize[su.vI[^1].rowFmt.cols[0].iok]:
      erru &"cannot make >2-D arrays for: {v}\n"; return 0
  if su.tiDir.len>0: su.ensureSpace su.vO, su.nT, su.nI, su.tiDir #MAYBE RE-..
  if su.itDir.len>0: su.ensureSpace su.vX, su.nI, su.nT, su.itDir #SHAPE/OPEN
  var k = newString(ids.width)  # MAIN WORK: LOOK UP id & INJECT INTO MATRICES
  template elt(nf; i, j, w: int): untyped =
    cast[pointer](cast[int](nf[i]) +% j*w)
  for j, id in ids:
    var i: int
    copyMem k[0].addr, id[0].unsafeAddr, ids.width
    try      : i = su.axI[k]
    except Ce: (if su.fixed: erru &"ix2id: no \"{k}\"\n"); continue
    for v in 0 ..< su.inpVars.len:
      if su.vI[v].ok:
        let w = su.vI[v].width
        if su.vO.len>0 and su.vO[v].ok:copyMem elt(su.vO[v],t,i,w),su.vI[v][j],w
        if su.vX.len>0 and su.vX[v].ok:copyMem elt(su.vX[v],i,t,w),su.vI[v][j],w
    inc result
  if result > 0: # OS may well not update mtime for files maybe written via mmap
    su.maybeTouch su.vO, su.tiDir, su.nT  # best effort stamp update (mmap
    su.maybeTouch su.vX, su.itDir, su.nI  # writes do not update file times.)
  ids.close                     # CLOSE ALL INPUTS
  for v in 0 ..< su.inpVars.len: su.vI[v].close
  su.vI.setLen 0
  if not su.fixed: su.axT.add(tm); su.axT.fK.flushFile # MARK COMPLETION

proc close*(su: var StacksUpdater) =
  for v in 0 ..< su.inpVars.len:
    if su.vO.len > 0: su.vO[v].close
    if su.vX.len > 0: su.vX[v].close
  su.axT.close; su.axI.close

proc getTimePaths*(pfxSfx: seq[string]): (seq[string], seq[string]) =
  let pfxLen = pfxSfx[0].len; let sfxLen = pfxSfx[1].len + 1
  for path in walkPatSorted(pfxSfx[0] & "*" & pfxSfx[1]):
    let time = path[pfxLen..^sfxLen]            # EXTRACT time FROM PATH
    if '.' in time: erru &"Inferred bad T={time} from {path}\n"; continue
    elif time.len == 0: erru &"Inferred empty T from {path}\n"; continue
    result[0].add time
    result[1].add path

proc upstack*(cmd="", idVar="", outDir=".", fixed=false, nT= -1, nI= -1,
    padT=1, padI=1, ix2tm="ix2tm", ix2id="ix2id", tiDir="tmId", itDir = "idTm",
    doTs="", ids="", wd="/tmp/up",clr=false, stamp="DONE", inpPat: seq[string])=
  ## make/update time series matrices from per-tm cross-sectional files.
  if inpPat.len != 1 or inpPat[0].split("@TM@").len != 2:
    Value!"non-option not @TM@ pattern; See --help."
  let (times, paths) = getTimePaths(inpPat[0].split "@TM@")
  if times.len == 0: echo &"No inputs match {inpPat[0]}"; return
  var su = suOpen(idVar, times, su.split(ids), nT, nI, padT, padI, ix2tm, ix2id,
                  tiDir, itDir, wd, fixed)      # SET UP OUTPUTS
  var didWk = false                             # LOOP OVER INPUTS
  var t0: Time
  try: t0 = getLastModificationTime(outDir/stamp) except Ce: discard # 0
  var fi: FileInfo
  for i in 0 ..< times.len:
    let time = times[i]
    let path = paths[i]
    try      : fi = path.getFileInfo
    except Ce: erru &"could not access {path}\n"; continue
    if doTs.len == 0: # XXX could also do any times > EndOf ix2tm mode
      if fi.lastWriteTime < t0: continue
    else:
      if time notin doTs: continue              # Could preprocess -> HashSet
    if path.getFileSize < 64: erru &"Skipping nearly empty {path}\n"; continue
# New ids appearing & output matrix resize conservation => 2 pass: ids & data.
# Id notin data by def. Id-segregated piped output may seem best BUT pipe writes
# block if buffers fill. More buffering works; Buffer may as well be /dev/shm.
    putEnv "F", path; putEnv "T", time; putEnv "OUTDIR", outDir
    if clr: clearDir wd                         # MAYBE CLEAR OLD
    if cmd.len != 0 and execShellCmd(cmd) != 0: # PASS 1: CREATE RIP/VEC FILES
      erru &"FAILED: {cmd}\n" # Instead of fromSV NimCall run `cmd` since fromSV
      break                   #..script wrapper|alt vector file gen may be nice.
    if su.update(time) == 0:                    # PASS 2: INJECT RIP/VEC FILES
      erru &"{path} Zero parsed rows; Check input filters/holiday sched\n"
    else:
      didWk = true
    echo "did time: ", time
  if didWk: touch outDir/stamp                  # Say we worked
  else: echo &"Up to date; No inputs seem newer than {stamp}."
  su.close

proc dfl(n: int): tuple[a,b: int] = parseSlice(getEnv("ROWS", "0:" & $n))
template queryRange*(n: int): untyped =
  cvtSlice(dfl(n), n)[0] ..< cvtSlice(dfl(n), n)[1]

import std/sugar
proc orD(s, default: string): string =  # little helper for accumulating params
  if s.startsWith("+"): default & s[1..^1] elif s.len > 0: s else: default
const es: Strings = @[]; proc jn(sq: Strings): string = sq.join("\n")
proc query*(prelude=es, vars=es, begin=es, where="true", stmtInputs: Strings,
            epilog=es, nim="", run=true, args="", cache="", lgLevel=0,
            outp="/tmp/qXXX", src=false, Warn=""): int =
  ## Gen & Run a *prelude*,*begin*,*where*,*stmt*,*epilog* IOTensor "query".
  ##
  ## Run against similarly indexed *inputs* NIO files (> 1st of *stmtInputs*).
  ## Likely works best for "column files" as per eg.s.  Within *where* & *stmt*:
  ##   *qI*: curr row index; (prefixed to help avoid `inputs` name clashes.)
  ##   Each row has `let INP=fINP[qI]` bindings (for *GenTime* basenames `INP`).
  ## Generated program left at *outp*.nim; Copy to "utilitize"|compile better.
  ## Knowing AWK/Nim/`rp`, you can learn this FAST.  This is much like SQL full
  ## table scans BUT type-check-compiled with access to all of Nim/SIMD.  Eg.:
  ##   nio q 'echo foo' foo.N\*                    # Extract `foo` as ASCII
  ##   nio q 'echo a,b,c' \*.N\* -w'nr mod 99==0'   # Echo 99th a,b,c ([abc].N*)
  ##   nio q -vt=0 t+=x -w'x>0' -e'echo t' \*.N\*   # Total >0 `x.N*` ints
  ##   nio q -pimport\\ stats -vr:RunningStat r.push\\ bar -eecho\\ r \*.N\*
  proc opens(inputs: seq[string]): string =
    var base0: string
    for j, input in inputs:
      let tail = input.splitPath.tail; let base = input.splitFile.name
      let rowT = $kindOf(tail[^1]) #NOTE: only works for IOTensor style files.
      result.add &"  var f{base} = initFileArray[{rowT}](\"{tail}\")\n"
      if j == 0: base0 = base
      else: result.add &"  if f{base}.len != f{base0}.len: quit \"badSize\"\n"
  proc lets(inputs: seq[string]): string =
    for j, input in inputs:
      let base = input.splitFile.name
      result.add &"    let {base} {{.used.}} = f{base}[qI]\n"
  let stmt    = if stmtInputs.len > 0: stmtInputs[0]     else: ""
  let inputs  = if stmtInputs.len > 1: stmtInputs[1..^1] else: @[]
  if inputs.len < 1: quit "`query` needs >=1 file inputs; --help says more", 1
  let base0 = inputs[0].splitFile.name
  let begin = collect(for v in vars: "var "&v) & (if begin.len>0:begin else: es)
  var program = &"""import nio
{prelude.jn} # [prelude]
proc qMain() = # [autoOpens]
{inputs.opens}
{indent(begin.jn, 2)} # [begin]
  for qI in queryRange(f{base0}.len):
{inputs.lets} # [inputs lets]
    if {where}: # [where] auto ()s?""" & "\n"
  if stmt.len == 0: program.add "      discard\n"
  else            : program.add "      " & stmt & " # {stmt}\n"
  program.add indent(epilog.jn, 2)
  program.add " # {epilogue}\nqMain()\n"
  if src: stderr.write program
  let oNm  = writeContAddrTempFile(outp, ".nim", program)
  let subc = if run: "r" else: "c" # sub-command; TODO cpp as well? Bubble-up?
  let args = args.orD("-d:danger ") & " " & cache.orD("--nimcache:/tmp/q ") &
             " " & Warn.orD("--warning[CannotOpenFile]=off ")
  let verb = "--verbosity:" & $lgLevel
  let cmd  = if nim.len > 0: nim else: "nim $1 $2 $3" % [subc, args, verb]
  execShellCmd cmd & (" -o:$1 $1" % oNm)

when isMainModule:
  import cligen, cligen/cfUt

  proc mergeParams(cmdNames: Strings, cmdLine=commandLineParams()): Strings =
    if cmdNames.len > 0:
      if cmdNames[0] == "multi":
        let bn = paramStr(0).lastPathPart
        if bn.startsWith("n-"):
          let bns = bn[2..^1]           # baseName suffix
          if bns in ["load1", "inferT", "fromSV", "stat", "print", "zip", "rip",
                     "cut", "tails", "moments", "defType", "order", "emerge",
                     "upstack"]: # allow n-foo links
            result.add bn[2..^1]
        return result & cmdLine
      let underJoin = su.toUpperAscii(cmdNames.join("_"))
      var cfPath = getEnv(underJoin & "_CONFIG")      # See if cfg file redirect
      if cfPath.len == 0:                             #..else use getConfigDir.
        cfPath = getConfigDir() / cmdNames[0] / "config"   # See if dir w/cfg
        if not fileExists(cfPath): cfPath = cfPath[0..^8]  #..else use file.
      result.add cfToCL(cfPath, if cmdNames.len > 1: cmdNames[1] else: "",
                        quiet=true, noRaise=true)
      result.add envToCL(underJoin)     # Finally add $NIO_PRINT..
    result = result & cmdLine           # and then whatever user entered

  dispatchMulti(
    [load1 , help={"inCode": """input code: [**bodh**] Bin|Octal|Dec|Hex int
**f** Float; **c** charArray(count); **x** transform""",
                   "oCode" : "Usual [**cCsSiIlLfdg**] NIO storage code",
                   "delim" : "alternate delimiter/terminator like '\\\\0'",
                   "xform" : "TransformParams; Eg. @file.[NDL].. {NOUP}",
                   "count" : "Output size; Only works for 'c'"}],
    [defType,help={"paths" : "[paths: 1|more paths to NIO files]",
                   "names" : "names for each column",
                   "lang"  : "programming language"}, echoResult=true],
    [inferT, help={"SVs"     : "[?SVs: input paths; empty|\"-\"=stdin]",
                   "ext"     : "parsing schema written to inp.`ext`",
                   "pre"     : "use `popen(pre%inp)` instead of `open(inp)`",
                   "delim"   : "strict TSV field delimiter",
                   "nHdr"    : "number of header lines (1st must be name)",
                   "timeFmts": "`times` module formats to try in order",
                   "iType"   : "integer nio code",
                   "fType"   : "float nio code",
                   "sType"   : "nio string code (iCode.repoExt)",
                   "guess"   : "guess type (output,input) for all empty"}],
    [fromSV, help={"SVs"   : "[?SVs: input paths; empty|\"-\"=stdin]",
                   "onlyOut": "only parse schema & gen output name",
                   "nameSep": "string to separate schema col names",
                   "dir"    : "maybe create&chdir here; SVs may need '..'",
                   "reps"   : "set $REPS for interpolation",
                   "schema": "path to the parsing schema file"}],
    [stat  , help={"nios"  : "paths to NIO files",
                   "format": """%[nrwzb]: name,rows,width,rowSize,lastBaseType
empty string => full information format"""},
             short={"format": 'c'}],
    [print , help={"sep"   : "separator for output columns",
                   "at"    : "shared default file for %s formats",
                   "t"     : "dump `at` keys terminated by `t` if != 'x'",
                   "fmTy"  : """format spec defaults for types; Default:
c%c C%d s%d S%d i%d I%d l%d L%d f%g d%g g%g
if AT=="" %s renders as a number via `fmTy`""",
                   "na"    : "how NA pointers render",
                   "paths" : "[paths: 0|more paths to NIO files]"}],
    [rip   , help={"input" : "NIO file to separate",
                   "names" : "pre.N names for output files"}],
    [zip   , help={"paths" : "[paths: 2|more paths to NIO files]"}],
    [widen , help={"io"    : "input output", "pad": "pad for wider char cols"}],
    [cut   , help={"path"  : "{paths: 1 path to a NIO file}",
                   "drop"  : "drop/delete field slice [a][:[b]]",
                   "pass"  : "pass/propagate field slice [a][:[b]]"}],
    [tails , help={"paths" : "{paths: 1|more NIO paths}",
                   "head"  : "initial fence post; 0=>none",
                   "tail"  : "final fence post; 0=>none",
                   "compl" : "pass complement/inside of fence posts",
                   "repeat": "repeat rows when head+tail>=n"},
            short={"help": '?'}],
    [xsum  , help={"paths" : "[paths: 1|more paths to NIO files]",
                   "outKind": "code for output row type"} ],
    [moments,help={"paths" : "[paths: 1|more paths to NIO files]",
                   "fmt"   : "Nim floating point output format",
                   "stats":"*n* *min* *max* *sum* *avg* *sdev* *skew* *kurt*"}],
    [kreduce,help={"paths" : "[paths: 1|more paths to NIO files]",
                   "fmt"   : "Nim floating point output format",
                   "group" : "nio file for group keys",
                   "stats":"*n* *min* *max* *sum* *avg* *sdev* *skew* *kurt*"}],
    [query, help={"stmtInputs": "{stmt} {input paths}",
                  "prelude": "Nim code for prelude/imports section",
                  "vars"   : "preface begin w/\"var \"+these shorthand",
                  "begin"  : "Nim code for begin/pre-loop section",
                  "where"  : "Nim code for row inclusion",
                  "epilog" : "Nim code for epilog/end loop section",
                  "nim"    : "\"\" => nim {if run: r else: c} {args}",
                  "run"    : "Run at once using nim r .. < input",
                  "args"   : "\"\" => -d:danger --mm:arc",
                  "cache"  : "\"\": --nimcache:/tmp/q (--incr:on?)",
                  "lgLevel": "Nim compile verbosity level",
                  "outp"   : "output executable; .nim NOT REMOVED",
                  "Warn"   : "\"\": --warning[CannotOpenFile]=off"}],
    [order , help={"at"    : "shared default repo for @ compares",
                   "output": "path + basename of output order file",
                   "paths" : "[paths: 0|more paths to NIO files]"}],
    [emerge, help={"prefix": "output path prefix (dirs are created)",
                   "order" : "output[i] = input[order[i]]",
                   "paths" : "[paths: 0|more paths to NIO files]"}],
    [upstack, help = {
      "cmd"   : "command to gen ripped vectors in `wd`",
      "idVar" : "ID variable; \"\"=>1st col of 1st schema",
      "outDir": "output dir; Can have 1 *.sc|Schema/@TM@",
      "fixed" : "fixed ix; Do not update indices",
      "nT"    : "pre-size matrices to >= this num Times",
      "nI"    : "pre-size matrices to >= this num Ids",
      "padT"  : "round time axis size up to nearest multiple",
      "padI"  : "round id axis size up to nearest multiple",
      "ix2tm" : "ix2tm name",
      "ix2id" : "ix2id name",
      "tiDir" : "tm,id output directory; \"\"=>skip",
      "itDir" : "id,tm output directory; \"\"=>skip",
      "doTs"  : "list of @TM@ to always do",
      "wd"    : "work dir/tmp dir for rip/vec files",
      "clr"   : "clear work dir/tmp pre-cmd",
      "inpPat": "[/path/to/my/@TM@/data]"},
      short = {"idVar": 'i', "ix2Tm": 'T', "ix2Id": 'I', "nT": 'n', "nI": 'm',
               "tiDir": 'o', "itDir": 'x', "padT": 'H', "padI":'W', "clr":'C'}])

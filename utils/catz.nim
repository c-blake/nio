import std/[os, posix, strutils, strformat] # LESSOPEN="|-catz %s"; tar t -Icatz -f -

proc errstr: string = $strerror(errno)      # helper proc
var av0, stdinName, catz_stderr: string     # global string vars

type Decoder = tuple[ext, hdr: string, inputSlot: int, av: seq[string]]

const htmlDecode = @["html2text", "-nobs", "-width", "8192", "-"]
const decs: seq[Decoder] = @[
  (".zip"    , "PK\x03"     , 2, @["unzip", "-p", ""]),
  (".ZIP"    , "PK\x03"     , 2, @["unzip", "-p", ""]),
  (".gz"     , "\x1Fã"   , 0, @["gzip", "-cdqf"]),
  (".tgz"    , "\x1Fã"   , 0, @["gzip", "-cdqf"]),
  (".Z"      , "\x1Fù"   , 0, @["uncompress"]),
  (".z"      , "\x1Fù"   , 0, @["uncompress"]),
  (".bz"     , "BZ0"        , 0, @["bunzip", "-Q"]),
  (".tbz"    , "BZ0"        , 0, @["bunzip", "-Q"]),
  (".bz2"    , "BZh"        , 0, @["bunzip2"]),
  (".tbz2"   , "BZh"        , 0, @["bunzip2"]),
  (".lzo"    , "âLZO"    , 0, @["lzop", "-df"]),
  (".toz"    , "âLZO"    , 0, @["lzop", "-df"]),
  (".lz"     , "]\x00\x00"  , 0, @["lzmadec", "-cd"]),
  (".lzma"   , "]\x00\x00"  , 0, @["lzmadec", "-cd"]),
  (".tlz"    , "]\x00\x00"  , 0, @["lzmadec", "-cd"]),
  (".xz"     , "˝7zX"    , 0, @["pixz", "-d"]),
  (".txz"    , "˝7zX"    , 0, @["pixz", "-d"]),
  (".Lz"     , "LZIP"       , 0, @["plzip", "-d"]),
  (".tLz"    , "LZIP"       , 0, @["plzip", "-d"]),
  (".lz4"    , "\x04\"M\x18", 0, @["lz4", "-cd"]),
  (".zst"    , "P*M\x18"    , 0, @["pzstd", "-cdqp8"]),
  ( ".zs"    , "P*M\x18"    , 0, @["pzstd", "-cdqp8"]),
  (".tzs"    , "P*M\x18"    , 0, @["pzstd", "-cdqp8"]),
  (".pdf"    , "%PDF"       , 1, @["pdftotext", "", "-"]),
  (".ps"     , "%!PS"       , 0, @["ps2ascii"]),
  (".ps.gz"  , "%"          , 0, @["pz2ascii"]), # hdr will never match
  (".ps.bz"  , "%"          , 0, @["pz2ascii"]), # hdr will never match
  ( ".ps.bz2", "%"          , 0, @["pz2ascii"]), # hdr will never match
  (".ps.xz"  , "%"          , 0, @["pz2ascii"]), # hdr will never match
  (".ps.zs"  , "%"          , 0, @["pz2ascii"]),
  (".html"   , "<!DO"       , 0, htmlDecode),
  (".html"   , "<htm"       , 0, htmlDecode),
  (".htm"    , "<htm"       , 0, htmlDecode)]
const PEEK = static: (var mx = 0; (for d in decs: mx = max(mx, d.hdr.len)); mx)

proc decode(decIx: int; path: string) = # Use above table to execvp a decoder
  var dc = decs[decIx]
  if dc.inputSlot != 0:
    if   path.len > 0     : dc.av[dc.inputSlot] = path
    elif stdinName.len > 0: dc.av[dc.inputSlot] = stdinName
    else: quit(&"catz: decoder {dc.av[0]} needs a path but has none", 2)
  if catz_stderr.len > 0:               #Q: optionally open per-path file?
    let fd = open(catz_stderr.cstring, O_WRONLY or O_APPEND or O_CREAT, 0o666)
    if fd != cint(-1): discard dup2(fd, cint(2))
  discard execvp(dc.av[0].cstring, allocCStringArray(dc.av))
  quit(&"catz: decoder \"{dc.av[0]}\": {errstr()}", 1)

proc sfx2ix(file: string): int =        # find decoder from sfx/extension
  result = -1
  for i, d in decs: (if file.endsWith(d.ext): return i)

proc hdr2ix(hdr: string): int =         # find decoder from hdr/magic number
  result = -1
  for i, d in decs: (if hdr.startsWith(d.hdr): return i)

proc file2ix(path: string; fd: cint; hdr: var string; n: var int): int =
  result = -1                           # find decoder from either
  hdr.setLen PEEK
  n = 0
  if path.len > 0:                      # try to infer via sfx & return if can
    if (result = sfx2ix(path); result != -1): return
  n = read(fd, hdr[0].addr, PEEK)       # ..then via hdr/aka magic number.
  if n == PEEK: result = hdr2ix(hdr)    # read enough bytes to classify
  if lseek(fd, -n, SEEK_CUR) != -1:     # rewind by whatever read, if can
    n = 0                               # register no stolen bytes.

proc writeAll(fd: cint; buf: var openArray[char]; n0: int): int =
  var n = n0; var off = 0               # loop control & buf offset
  while n > 0:
    if (let did = write(fd, buf[off].addr, n.int); did) > 0:
      inc off, did; dec n, did
    elif errno == EINTR: continue       # Good for SIGWINCH,SIGTSTP&such
    else: break
  return n0 - n

proc fdCopy(src, dst: cint) =           # file descriptor copy loop
  var buf: array[8192, char]
  var nR: int
  while (nR = read(src, buf[0].addr, buf.sizeof); nR) > 0:
    if writeAll(dst, buf, nR) != nR: quit(17)
  if nR < 0: quit("catz: read: {errstr()}", 3)

proc oneFile(path: string; do_fork: bool) = # dispatch just one file
  var hdr = newStringOfCap(PEEK)
  var n: int
  if path.len > 0:
    discard close(0)
    if open(path, O_RDONLY) != 0:
      stderr.write &"{av0}: open(\"{path}\"): {errstr()}\n"; return
  let decIx = file2ix(path, 0, hdr, n)
  if n > 0:                             # unseekable; fork to re-prepend hdr
    var fds: array[2, cint]
    if pipe(fds) == -1: stderr.write &"{av0}: pipe: {errstr()}\n"; return
    let pipe_writer = fork()
    case pipe_writer
    of 0:                               # child: stdin -> from parent
      discard dup2(fds[0], 0)
      discard close(fds[1])
      if decIx >= 0: decode(decIx, path)
      fdCopy(0, 1)
      quit(0)
    of -1: stderr.write &"{av0}: fork: {errstr()}\n"
    else:                               # parent:
      discard close(fds[0])
      if write(fds[1], hdr[0].addr, n) != n:  # put hdr back in place
        stderr.write &"{av0}: {errstr()}\n"; return
      fdCopy(0, fds[1])                 #  blocking RW loop
      discard close(fds[1])
  elif decIx >= 0:
    if not do_fork:                     # replace current process
      decode(decIx, path)
    let pipe_writer = fork()
    case pipe_writer                    # child shares stdout
    of 0:
      decode(decIx, path)
      stderr.write &"{av0}: {errstr()}\n"
    of -1: stderr.write &"{av0}: {errstr()}\n"
    else:                               # barrel onward upon fail
      var st: cint; discard waitpid(pipe_writer, st, 0)
  else: fdCopy(0, 1)

proc nFiles(av: seq[string]) =          # drive oneFile case for n>1
  var fd: array[2, cint]
  var didStdin = false
  var stdin_orig: cint
  if pipe(fd) == -1: quit(&"{av0}: pipe: {errstr()}", 1)
  let pipe_reader = fork()
  if pipe_reader != 0:                  # make pipe0->orig stdout copier
    discard close(fd[1])
    fdCopy(fd[0], 1)
    quit(0)
  discard close(1)                      # make pipe1 == stdout for current..
  if dup(fd[1]) != 1:                   #               ..and all subprocs
    discard close(fd[0])
    discard close(fd[1])
    quit(&"{av0}: cannot get stdout", 1)
  discard close(fd[1])
  stdin_orig = dup(0)                   # Save stdin
  for a in av:                          # create decoders arg-by-arg
    if a == "-":
      if not didStdin:
        discard close(0)
        if dup(stdin_orig) == 0: oneFile("", true)
        didStdin = true
    else: oneFile(a, true)
  discard close(1)                      # send EOF for out streamer child
  var st: cint; discard waitpid(pipe_reader, st, 0)

catz_stderr = getEnv("CATZ_STDERR")     # save some globals
av0 = paramStr(0)
let av = commandLineParams()            # CLoption potato parsing
var o = 0                               # virtual zero (after next stmt)
if av.len>0 and av[0] == "-d": inc o    # GNU tar -I option needs a "-d"
if av.len>o and av[o].startsWith("-v"): # specify envVar giving stdin name
  var stdinNameVar: string              # -v$ => stdinName = /dev/stdin
  if av[o].len == 2 and av.len > o + 1: # -v FOO ..
    stdinNameVar = av[o + 1]; inc o, 2  # shift for option&arg
  else:                                 # -vFOO
    stdinNameVar = av[o][2..^1]; inc o  # shift for option&arg
  stdinName = getEnv(stdinNameVar, "")
if av.len > o + 1: nFiles av[o..^1]     # dispatch to nFiles|oneFile
else: oneFile (if av.len > o and av[o] != "-": av[o] else: ""), false

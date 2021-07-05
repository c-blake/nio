# State machine from https://github.com/eBay/tsv-utils. The idea is to read into
# a fixed input buffer, usually make 1 byte changes in the same buffer.  A flush
# (shifting origin) at end of buffer or when removing CR/"|subbing tab/NL keeps
# copying down. Delims & quotes can only be 1-byte, but elsewise UTF-8 agnostic.
# 2-byte CRLF can be split across reads which is handled by 'CR' DFA states.

import posix, strformat, cligen/osUt # osUt: uriteBuffer, erru, outu, c_setvbuf

type Log      = enum eachSub, totalSubs
type DFAState = enum sFEnd, sNonQuo, sQuo, sQuoInQuo, sCRAtEnd, sCRInQuo

proc c2tsv(tab='\0', TabSub=" ", nl='\n', NlSub=" ", log: set[Log]={},
           si="stdin", bSz=65536): int =
  ## Convert rfc4180-ish quoted CSV UTF-8 on stdin to IANA *strictly separated
  ## TSV* on stdout.  Traits here that vary among CSV tools:
  ##  - Accepts {\\r|\\r\\n|\\n} for end-of-line; Always emits *nl* (incl @EOF).
  ##  - \\n & ',' inside quoted fields are supported (output must not collide).
  ##  - " is ok in unquoted fields; Fields starting w/" must obey quoting rules.
  ## Ease of IANA TSV parsing & pipeline parallelism imply you should implement
  ## any higher level ideas (header/table structure, etc.) via `popen("c2tsv")`.
  if tab in TabSub: erru &"`TabSub` must not contain `tab`\n"; return 1
  if nl  in NlSub : erru &"`NlSub` must not contain `nl`\n"; return 1
  var s     = sFEnd             # current parser state
  var rNo   = 1'u64             # 1-origin input line/record/row number
  var fNo   = 0                 # 0-origin field number within row
  var totNl, totTab: uint64     # counters for in-field delim statistics
  var buffr = newString(bSz)    # input buffer
  var inBuf = cast[cstring](buffr[0].addr)
  discard c_setvbuf(stdout, nil, 0, bSz.csize_t)  # match output w/input

  template eor = rNo.inc; fNo=0 # End Of Row/record

  template flush(sub="") =      # called @EOBlock; Does not emit ending byte
    discard stdout.uriteBuffer(inBuf[i0].addr, i - i0)  # inBuf[i0..<i]
    if sub.len > 0: outu sub
    i0 = i + 1                  # next blk always 1 past current block end

  template sub(x) =             # substitution
    if `x Sub`.len == 1: inBuf[i] = `x Sub`[0]  # single byte
    else               : flush `x Sub`          # multi byte
    if eachSub   in log: erru &"{si}:{rNo} {c.repr} substitution\n"
    if totalSubs in log: `tot x`.inc

  while (let n = read(0, inBuf[0].addr, bSz); n > 0):
    var i0, i: int              # block start: ix where next write starts from
    while i < n:
      let c = inBuf[i]          # current block is inBuf[i0..<i]
      case s                    # ODD CODE FMT MAKES READ-OFF-DFA-DIAGRAM EASY
      of sFEnd:                 # DFA: start of input|after eating field term
        fNo.inc
        if c == '"': flush           ; s = sQuo
        else:                          s = sNonQuo; continue
      of sNonQuo:               # DFA: doing a non-quoted field
        case c
        of ',' : inBuf[i] = tab      ; s = sFEnd
        of '\t': sub(Tab)
        of '\n': eor()               ; s = sFEnd
        of '\r': inBuf[i] = nl; eor(); s = sCRAtEnd
        else: discard
      of sQuo:                  # DFA: doing a quoted field
        case c                  # flush block w/o ". New state decides if emit "
        of '"' : flush               ; s = sQuoInQuo
        of '\t': sub(Tab)
        of '\n': sub(Nl)
        of '\r': sub(Nl)             ; s = sCRInQuo
        else: discard
      of sQuoInQuo:             # DFA:  just did '"' in a quoted field; Buf w/o
        case c                  #..'"' just flushed.  Legal bytes now: ",\n
        of '"' :                       s = sQuo
        of ',' : inBuf[i] = tab      ; s = sFEnd
        of '\n': eor()               ; s = sFEnd
        of '\r': inBuf[i] = nl; eor(); s = sCRAtEnd
        else: erru &"{si}:{rNo} bad quoting @char '{c}'\n"; return 1
      of sCRInQuo:              # DFA: last char was a CR in a quoted field
        if c == '\n': flush          ; s = sQuo
        else:                          s = sQuo; continue # NakedCR
      of sCRAtEnd:              # DFA: last char was a CR terminating a row/line
        if c == '\n': flush          ; s = sFEnd
        else:                          s = sFEnd; continue # NakedCR
      i.inc
    discard stdout.uriteBuffer(inBuf[i0].addr, n - i0)
    i0 = 0                      # inBuf[i0..<n] @EOBuf above *slightly* slower
  if fNo > 0: outu nl           # ensure newline termination
  if totalSubs in log:
    erru &"{si}: {totNl} \\n substitutions {totTab} \\t substitutions\n"
  if s == sQuo:
    erru &"{si}:{rNo} unterminated quote @EOF\n"; return 1

when isMainModule:  # Build with --gc:markAndSweep & PGO for best performance.
  import cligen; dispatch c2tsv, help = {
    "tab"   : "output field delimiter byte (e.g. '\\\\t')",
    "TabSub": "how to spell output delim inside fields",
    "nl"    : "output record terminator byte",
    "NlSub" : "how to spell output termin inside fields",
    "si"    : "StdIn name for name:lineNumber messages",
    "log"   : "stderr log {eachSub, totalSubs}",
    "bSz"   : "size of IO buffers (in bytes)"}
# On files like nfl_all_plays.csv|worldcitiespop.csv across 4 CPUs/1 decade,
# LnxGcc11-compiled speed is about: 40% of tr , \\0, 6X gdc-csv2tsv, 3-8X xsv.

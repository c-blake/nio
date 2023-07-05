# State machine from https://github.com/eBay/tsv-utils. The idea is to read into
# a fixed input buffer, usually make 1 byte changes in the same buffer.  A flush
# (shifting origin) at end of buffer or when removing CR/"|subbing tab/NL keeps
# copying down. Delims & quotes can only be 1-byte, but elsewise UTF-8 agnostic.
# 2-byte CRLF can be split across reads which is handled by 'CR' DFA states.

import std/[posix, strformat], cligen/osUt # uriteBuffer, erru, outu, c_setvbuf
when not declared(File): import std/syncio

type Log      = enum eachSub, totalSubs
type DFAState = enum sFEnd, sNonQuo, sQuo, sQuoInQuo, sCRAtEnd, sCRInQuo

proc c2tsv(tab='\t', TabSub="\\t", nl='\n', NlSub="\\n", log: set[Log]={},
           si="stdin", bSz=65536): int =
  ## THERE IS NO ONE SPEC FOR "CSV".  This program converts rfc4180-ish quote-
  ## escaped CSV UTF-8 on stdin to *strictly separated*, \\-escaped IANA TSV on
  ## stdout.  Traits here that vary among CSV tools:
  ##  - Accepts {\\r|\\r\\n|\\n} for end-of-line; Always emits *nl* (incl @EOF).
  ##  - \\n & ',' inside quoted fields are supported.
  ##  - " is ok in unquoted fields; Fields starting w/" must obey quoting rules.
  ## Output is soundly record&field split-parsable w/{\\\\, \\t, \\n} needing
  ## de-escape post-framing to get original 8-bit vals.  Parsing ease & pipeline
  ## | file parallelism imply higher level ideas (header | table struct..) are
  ## best done via `popen("c2tsv")` | temp files.
  if tab in TabSub: erru &"`TabSub` must not contain `tab`\n"; return 1
  if nl  in NlSub : erru &"`NlSub` must not contain `nl`\n"; return 1
  const BackSub = "\\\\"
  var s     = sFEnd             # current parser state
  var rNo   = 1'u64             # 1-origin input line/record/row number
  var fNo   = 0                 # 0-origin field number within row
  var totNl,totTab,totBack: uint64 # counters for in-field delim stats
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
        if c in {',', '\t', '\n', '\r', '\\'}:
          case c
          of ',' : inBuf[i] = tab    ; s = sFEnd
          of '\t': sub(Tab)
          of '\n': eor()             ; s = sFEnd
          of '\r': inBuf[i]=nl; eor(); s = sCRAtEnd
          of '\\': sub(Back)    # single \ in input --> \\ in output
          else: discard
        else: discard
      of sQuo:                  # DFA: doing a quoted field
        if c in {'"', '\t', '\n', '\r', '\\'}:
          case c                # flush block w/o ". New state decides if emit "
          of '"' : flush             ; s = sQuoInQuo
          of '\t': sub(Tab)
          of '\n': sub(Nl)
          of '\r': sub(Nl)           ; s = sCRInQuo
          of '\\': sub(Back)    # single \ in input --> \\ in output
          else: discard
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
    erru &"{si}: {totNl} \\n subs {totTab} \\t subs {totBack} \\\\ subs\n"
  if s == sQuo:
    erru &"{si}:{rNo} unterminated quote @EOF\n"; return 1

when isMainModule:  # Build with --gc:markAndSweep & PGO for best performance.
  import cligen; dispatch c2tsv, help = {
    "tab"   : "output field delimiter byte (e.g. '\\\\t')",
    "nl"    : "output record terminator byte",
    "TabSub": "how to spell delim inside output fields",
    "NlSub" : "how to spell termin inside output fields",
    "si"    : "StdIn name for name:lineNumber messages",
    "log"   : "stderr log {eachSub, totalSubs}",
    "bSz"   : "size of IO buffers (in bytes)"}
# On files like nfl_all_plays.csv|worldcitiespop.csv across 4 CPUs/1 decade,
# LnxGcc11-compiled speed is about: 40% of tr , \\0, 6X gdc-csv2tsv, 3-8X xsv.

import std/[strformat, parsecsv, streams, strutils], cligen/osUt
when not declared(File): import std/syncio

type Log = enum eachSub, totalSubs

proc c2tsvs(comma=',', quote='"', escape='\0', skip=false, tab='\t',
            TabSub="\\t", nl='\n', NlSub="\\n", log: set[Log]={}, si="stdin",
            bSz=65536, raw=false, addEmpty=false): int =
  ## Use Nim stdlib `parsecsv` to convert quoted/escaped CSV on stdin to
  ## *strictly separated*, \\-escaped TSV on stdout.  Output is soundly newline
  ## char-frameable & split-parsable { un-escape of (backslash, TabSub, NlSub)
  ## is nice to render for '\\'-unfamiliar users }.  Parsing ease & pipeline |
  ## file parallelism imply higher level ideas (header/table struct..) are best
  ## done via `popen("c2tsv")` | a temp file.  See also 7X faster `c2tsv`.
  var totNl, totTab, totBack: int       # counters for in-field delim stats
  var p: CsvParser
  discard c_setvbuf(stdin , nil, 0, bSz.csize_t)  # boost input
  discard c_setvbuf(stdout, nil, 0, bSz.csize_t)  # match output w/input
  open(p, newFileStream(stdin), si, comma, quote, escape, skip)
  var rNo = 1
  let outDelims = {tab, nl}
  while readRow(p):
    if p.row.len > 0:         # non-blank line
      for i, fld in p.row:
        if fld.find(outDelims) >= 0:
          var fix = newStringOfCap(fld.len + 4)
          for c in fld:
            if c == tab:
              fix.add TabSub
              totTab.inc
              if eachSub in log: erru &"{si}:{rNo} {c.repr} substitution\n"
            elif c == nl:
              fix.add NlSub
              totNl.inc
              if eachSub in log: erru &"{si}:{rNo} {c.repr} substitution\n"
            elif c == '\\':
              fix.add "\\\\"
              totBack.inc
              if eachSub in log: erru &"{si}:{rNo} {c.repr} substitution\n"
            else: fix.add c
          stdout.urite (if raw: fix else: fix.strip)
        else:
          stdout.urite (if raw: fld else: fld.strip)
        if i < p.row.len - 1:
          stdout.urite tab
    if addEmpty:
      stdout.urite tab
    stdout.urite nl
    rNo.inc
  if totalSubs in log:
    erru &"{si}: {totNl} \\n subs {totTab} \\t subs {totBack} \\t subs\n"

when isMainModule:import cligen;include cligen/mergeCfgEnv;dispatch c2tsvs,help={
  "comma"   : "CSV delimiter",
  "quote"   : "CSV quote character",
  "escape"  : "CSV escape character",
  "skip"    : "skip leading whitespace",
  "tab"     : "output field delimiter byte (e.g. '\\\\t')",
  "TabSub"  : "how to spell output delim inside fields",
  "nl"      : "output record terminator byte",
  "NlSub"   : "how to spell output termin inside fields",
  "si"      : "StdIn name for name:lineNumber messages",
  "log"     : "stderr log {eachSub, totalSubs}",
  "bSz"     : "size of IO buffers (in bytes)",
  "raw"     : "don't strip leading/trailing field space",
  "addEmpty": "add extra field delim pre-NL (term v dlm)"}

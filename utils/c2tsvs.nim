import strformat, parsecsv, streams, strutils, cligen/osUt

type Log = enum eachSub, totalSubs

proc c2tsvs(comma=',', quote='"', escape='\0', skip=false, tab='\0', TabSub=" ",
            nl='\n', NlSub=" ", log: set[Log]={}, si="stdin", bSz=65536,
            raw=false, addEmpty=false): int =
  ## Convert quoted/escaped CSV on stdin to IANA *strictly separated TSV* on
  ## stdout via Nim stdlib `parsecsv`.  Ease of IANA TSV parsing & pipeline
  ## parallelism imply you should implement higher level ideas (header/table
  ## analysis, etc.) via `popen("c2tsvs")`.  See also 7X faster `c2tsv`.
  var totNl, totTab: int      # counters for in-field delim stats
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
              totTab.inc
              fix.add TabSub
              if eachSub in log: erru &"{si}:{rNo} {c.repr} substitution\n"
            elif c == nl:
              totNl.inc
              fix.add NlSub
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
    erru &"{si}: {totNl} \\n substitutions {totTab} \\t substitutions\n"

when isMainModule: import cligen; dispatch c2tsvs, help = {
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

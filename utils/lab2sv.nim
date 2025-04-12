import std/[tables, sequtils, sugar], cligen/[sysUt, osUt, mslice], cligen
when not declared(stdin): import std/syncio

# Return Table mapping keys in `oa` to their positions.  Dups raise.
proc colNos[T](oa: openArray[T]): Table[T,int] = (for i,k in oa: result[k] = i)

proc lab2sv(iDlm=",", oDlm="\t", na="", hdr="ROW", row=0, col=1, val=2,
            skip=0, max=1024, colKeys: seq[string]) =
  ## Convert labeled [ rowKey colKey val ] on stdin to SV on stdout.  Column key
  ## values not appearing on the command-line are dropped.  E.g.:
  ##   Input      Output (after `lab2sv -o, A B C`):
  ##     b,B,4    ROW,A,B,C
  ##     a,A,1    a,1,,2
  ##     b,A,3    b,3,4,
  ##     a,C,2
  ## This is hash-based.  For large data, see also labs2sv.
  if colKeys.len == 0: Help!"Need >0 column keys; Full ${HELP}"
  let colKeys = collect(for j in 0..<colKeys.len: colKeys[j].toMSlice)
  let colNo   = colNos(colKeys)                             # colKey->slot
  let newRow  = repeat[string](na, colKeys.len)             # a fresh row
  var rows    = initTable[string, seq[string]]()            # table of rows
  var lno     = 0
  let sep     = initSep(iDlm)
  var cols: seq[MSlice]
  for (ln, nLn) in stdin.getDelims:
    inc lno
    if lno > skip:
      sep.split(MSlice(mem: ln, len: nLn - 1), cols, max)
      var j: int
      try      : j = colNo[cols[col]]
      except Ce: continue                                   # unnamed colKey
      rows.mgetOrPut($cols[row], newRow)[j] = $cols[val]
  stdout.urite hdr                                          # print header
  for ck in colKeys: stdout.urite oDlm, ck
  stdout.urite '\n'
  for r, cs in rows:                                        # then rows
    stdout.urite r
    for c in cs: stdout.urite oDlm, c
    stdout.urite '\n'

when isMainModule: include cligen/mergeCfgEnv; dispatch lab2sv, help={
  "colKeys": "in-order values of column keys/hdrs for SV",
  "iDlm": "input delimiter",
  "oDlm": "output delimiter",
  "na"  : "string to use for missing values",
  "row" : "0-origin input column for row key",
  "col" : "0-origin input column for col key",
  "val" : "0-origin input column for value key",
  "hdr" : "a header for the row label",
  "skip": "number of input lines to skip",
  "max" : "max number of input file columns"}

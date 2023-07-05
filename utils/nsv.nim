import cligen, cligen/[osUt, textUt, mslice, mfile]

proc nsv(input="-", delim="\t", maxCol=0, prLen=false, bulkSt=false,
         convert=false, sepOut="\t"): int =
  ## N)umber of S)eparated V)alues.  This is useful in what people lately call
  ## ETL contexts -- in service of building pre-parsed binary files.  You can
  ## almost generate a parsing schema for fixed width text fields by massaging:
  ##   `paste <(head -n1 < $datSV | tr , \\\\n) <(tail -n+2 < $datSV | nsv -b)`
  ## `tail -n+2 < $datSV | nsv -c | bu/cstats ..` can give detailed width stats.
  if not bulkSt and not convert:
    raise newException(HelpError, "No work! Full ${HELP}")
  template maxEq(mx,x) = mx = max(mx,x) # Accumulate in analogy with += | *=
  var n, m: int                         # Input line number, Num of columns
  let dlm = delim.initSep               # Compile delim string into a splitter
  var cols: seq[MSlice]
  var ws: seq[int]
  for line in input.mSlices:            # stdio RO mmap | slices
    inc n                               # COLLECT WIDTHS & DATA
    dlm.split line, cols, maxCol
    m.maxEq cols.len
    if bulkSt and ws.len < m: ws.setLen m
    for j, col in cols:
      let w = if prLen: printedLen(col.toOpenArrayChar) else: col.len
      if convert:
        if j > 0: outu sepOut
        outu w
      if bulkSt: ws[j].maxEq w
    if convert: outu '\n'
  if bulkSt:
    for w in ws: outu w, '\n'
    erru n, " rows ", m, " cols\n"

when isMainModule: include cligen/mergeCfgEnv; dispatch nsv, help={
  "input"  : "path to mmap|read as input; \"-\" => stdin",
  "delim"  : "inp delim chars; Any repeats => foldable",
  "maxCol" : "max columns to form for aligning;0=unlimited",
  "prLen"  : "adjust for ANSI SGR/utf8 in assessing width",
  "bulkSt" : "emit number of rows & columns seen to stderr",
  "convert": "convert every field into just its width",
  "sepOut" : "separator for converted output table"}

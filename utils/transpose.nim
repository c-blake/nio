import std/[strutils, strformat], cligen/osUt
when not declared(stdin): import std/syncio

proc transpose(delim='\0', sep='\t', null="N/A", verbose=false) =
  ## A stdin-stdout filter that transposes strict \\t-\\n-delimited TSV.  When
  ## the table is irregular, *null* is substituted for a real value.  This is
  ## useful to extract schemas as in `c2tsv < foo.csv | head -n4 | transpose`.
  var tab: seq[seq[string]]
  var nCol = 0
  var nRow = 0
  for line in stdin.lines:
    nRow.inc
    let cols = line.split(delim)
    tab.add cols
    if cols.len > nCol:
      if nCol != 0 and verbose: erru &"new max col {nCol} at row {nRow}\n"
      nCol = cols.len
    elif cols.len < nCol and verbose:
      erru &"stdin:{nRow}: short row; only {nCol} columns\n"
  for j in 0 ..< nCol:
    for i in 0 ..< nRow:
      if i > 0: outu sep
      outu (if j < tab[i].len: tab[i][j] else: null)
    outu "\n"

when isMainModule:
 import cligen; include cligen/mergeCfgEnv; dispatch transpose, help={
  "delim": "input delimiter", "sep": "output separator", "null": "N/A code",
  "verbose": "identify table irregularities"}

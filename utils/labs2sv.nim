import cligen, cligen/[osUt, mslice], std/sugar
when not declared(stdout): import std/syncio

proc setFrom*(str: var string, slc: MSlice) =
  str.setLen slc.len; copyMem str[0].addr, slc.mem, slc.len

proc labs2sv(iDlm=",", oDlm="\t", na="", hdr="ROW", row=0, col=1, val=2,
             skip=0, max=1024, colKeys: seq[string]) =
  ## Unfold row,col-sorted & keyed lines on stdin with column keys that match
  ## `colKeys`, writing rows to stdout as they complete.  E.g.:
  ##   Input     Output (after `labs2sv -o, A B C`):
  ##     a,A,1   ROW,A,B,C
  ##     a,C,2   a,1,,2
  ##     b,A,3   b,3,4,
  ##     b,B,4
  ## Something like `sort -t, -k1 -k2` is useful for preprocessing.  Input lines
  ## with missing row keys are ignored.  See also `lab2sv`.
  if colKeys.len == 0:
    raise newException(HelpError, "Need >0 column keys; Full ${HELP}")
  stdout.urite hdr                      # print header
  for ck in colKeys: stdout.urite oDlm, ck
  stdout.urite '\n'
  let colKeys = collect(for j in 0..<colKeys.len: colKeys[j].toMSlice)
  var oCs = newSeq[string](colKeys.len + 1) # The output row's columns
  template wr =                             # Emit row to stdout
    if oCs[0].len != 0:
      for j, c in oCs.mpairs:
        if j != 0: stdout.urite oDlm
        stdout.urite if c.len != 0: c else: na
        c.setLen 0
      stdout.urite '\n'
  let sep = initSep(iDlm)
  var iCs: seq[MSlice]
  var i, j: int                         # Output row & Cyclic column index
  for (a, n) in stdin.getDelims:
    inc i
    if i > skip:
      sep.split MSlice(mem: a, len: n - 1), iCs, max
      if iCs[row].len == 0: continue    # No row key => See no evil
      if iCs[row] == oCs[0].toMSlice:   # Same row key
        var k = j + 1
        while k < colKeys.len and colKeys[k] != iCs[col]: inc k
        if k < colKeys.len:             # Known col key
          j = k                         # Accept column advance edit
          oCs[j+1].setFrom iCs[val]     # Copy value to appropriate slot
      else:                             # New row key
        wr()                            # Write the old row
        oCs[0].setFrom iCs[row]         # Copy the new row key
        var k = 0
        while k < colKeys.len and colKeys[k] != iCs[col]: inc k
        if k < colKeys.len:             # Known col key
          j = k
          oCs[j+1].setFrom iCs[val]     # Copy value to appropriate slot
        else: j = -1                    # So k = j + 1 starts at 0
  wr()                                  # Write any final row

when isMainModule: include cligen/mergeCfgEnv; dispatch labs2sv, help={
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

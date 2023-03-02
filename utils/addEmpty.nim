when not declared(File): import std/syncio
import cligen/[osUt, unsafeAddr]

proc addEmpty*(tab='\0', nl='\n', bSz=32768): int =
  ## stdin-stdout filter to add an extra *tab* before every *nl*. This adds an
  ## empty field to make all internal fields C strings (often well supported).
  ## For me this runs 1.3x faster than GNU coreutils `LC_ALL=C tr`.  This tool
  ## mostly just allows `c2tsv` to have all the functionality of `c2tsvs` with
  ## no flushing expansion to slow things down & instead more parallelism.
  discard c_setvbuf(stdin , nil, 0, bSz.uint)
  discard c_setvbuf(stdout, nil, 0, bSz.uint)
  var tabNl = $tab & $nl
  for (row, nRow) in stdin.getDelims(nl):
    if stdout.uriteBuffer(row[0].unsafeAddr, nRow - 1) < nRow - 1 or
       stdout.uriteBuffer(tabNl[0].addr, 2) < 2: break

when isMainModule: import cligen; dispatch addEmpty

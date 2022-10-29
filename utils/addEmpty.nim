when not declared(File): import std/syncio
when not declared(csize_t):
  type csize_t = uint64

proc setvbuf(f: File, b: pointer, m: cint, z: csize_t): cint {.
  importc, header: "stdio.h".}

proc getdelim(p: ptr cstring, nA: ptr csize_t, dlm: cint, f: File): int {.
  importc, header: "stdio.h".}

proc uriteBuffer(f: File, buffer: pointer, len: Natural): int {.inline.} =
  when defined(linux) and not defined(android):
    proc fwrite(buf: pointer, size, n: csize_t, f: File): cint {.importc: "fwrite_unlocked", header: "stdio.h".}
  else:
    proc fwrite(buf: pointer, size, n: csize_t, f: File): cint {.importc, header: "stdio.h".}
  fwrite(buffer, 1, len.csize_t, f)

proc addEmpty*(tab='\0', nl='\n', bSz=32768): int =
  ## stdin-stdout filter to add an extra *tab* before every *nl*. This adds an
  ## empty field to make all internal fields C strings (often well supported).
  ## For me this runs 1.3x faster than GNU coreutils `LC_ALL=C tr`.  This tool
  ## mostly just allows `c2tsv` to have all the functionality of `c2tsvs` with
  ## no flushing expansion to slow things down & instead more parallelism.
  discard setvbuf(stdin , nil, 0, bSz.csize_t)
  discard setvbuf(stdout, nil, 0, bSz.csize_t)
  var row: cstring
  var nRow: csize_t
  var tabNl = $tab & $nl
  while true:
    let length = getdelim(row.addr, nRow.addr, cint(nl), stdin)
    if length == -1 or
       stdout.uriteBuffer(row[0].addr, length - 1) < length - 1 or
       stdout.uriteBuffer(tabNl[0].addr, 2) < 2:
         break

when isMainModule: import cligen; dispatch addEmpty

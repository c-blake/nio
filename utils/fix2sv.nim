import std/parseutils as pu, std/[strutils, strformat],
       cligen, cligen/[mfile, mslice, osUt]
when not declared(stdout): import std/syncio

type Log = enum eachSub, totalSubs

proc stripParse(s: string, side: char, doIt: var seq[bool],
                chars: set[char]): set[char] =
  if (let ix = s.find(side); ix >= 0):
    doIt.add true
    for c in s[ix+1..^1]:
      if c in {'L', 'R'}: return
      result.incl c
    if result.len == 0:
      result = chars
  else:
    doIt.add false

proc fix2sv(inp="/dev/stdin", tab='\t', TabSub="\\t", log: set[Log]={},
            nSkip=0, si="stdin", chars={' ', '\t'}, specs: seq[string]) =
  ## Move fixed-byte-width fields in `inp` to split-parsable TSV on stdout, a
  ## notional inverse to `bu/align` mostly intended for bulk conversion.  Each
  ## `spec` gives a field width optionally ending in L|R (e.g., \"9L\") to strip
  ## `chars` on the Left|Right, optionally suffixed by a local `chars`.  E.g.:
  ##   fix2sv -n1 2 '6L 0' 10LR 3R < in > out
  ## skips 1 header row & converts 21 byte + endLine rows to SV like so:
  ##   "ST  ZIP5 COUNTY   DONE " => dropped
  ##   "NY 10001 NEW\\tYORK YES" => "NY\\t10001\\tNEW\\\\tYORK\\tYES\\n"
  ##   "MA 02139 BOSTON   NO " => "MA\\t2139\\tBOSTON\\tYES\\n"
  ## NOTE: In the above *3R 5..* works like *2 6L..*, but either way "gaps" must
  ## be explicitly assigned to some field & data rows must total 21 bytes.
  ## NOTE: Unlike `c2tsv`, this input format cannot encode EOLs within fields.
  ## NOTE: Local chars may induce a need for shell escaping/quoting.
  var totTab, totBack: uint64   # Counters for substitution stats
  const BackSub = "\\\\"
  template sub(x, rep) {.dirty.} =
    o.add `x Sub`
    if eachSub   in log: erru &"{si}:{rNo} " & rep & " substitution\n"
    if totalSubs in log: `tot x`.inc
  if specs.len<1: raise newException(HelpError, "Too few `specs`; Full ${HELP}")
  var stripL, stripR: seq[bool]
  var charsL, charsR: seq[set[char]]
  var offs, lens: seq[int]
  var roff, w, rNo: int
  for spec in specs:
    let eoN = pu.parseInt(spec, w) # End Of Number in spec
    offs.add roff
    lens.add w
    roff += w
    charsL.add spec[eoN..^1].stripParse('L', stripL, chars)
    charsR.add spec[eoN..^1].stripParse('R', stripR, chars)
  var o: string
  for row in inp.mSlices:
    inc rNo
    if rNo > nSkip:
      if row.len == roff:
        for i, off in offs:
          var f = MSlice(mem: row.mem +! off, len: lens[i])
          if stripL[i]: f.stripLeading  charsL[i]
          if stripR[i]: f.stripTrailing charsR[i]
          o.setLen 0            # Escaping copy into re-used buffer
          for c in f:
            if   c == tab : sub(Tab, "\\t")
            elif c == '\\': sub(Back, "\\")
            else: o.add c
          o.add (if i == offs.len - 1: '\n' else: tab)
          stdout.urite o
      else:
        erru &"{si}:{rNo}- line len {row.len} != {roff} chars: \"{$row}\"\n"
  if totalSubs in log:
    erru &"{si}: {totTab} \\t subs {totBack} \\\\ subs\n"

when isMainModule: dispatch fix2sv, help={
  "specs" : "width1[Lchars][Rchars] width2[Lchars][Rchars]..",
  "inp"   : "input file; reg files get mem.mapped",
  "tab"   : "output field delim byte (e.g. '\\\\t')",
  "TabSub": "how to spell delim inside out fields",
  "log"   : "stderr log {eachSub, totalSubs}",
  "nSkip" : "num header rows to skip",
  "si"    : "StdIn name for name:lineNo messages",
  "chars" : "Add to *default* strip char set"}

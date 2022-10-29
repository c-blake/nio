import std/[re, sugar]
when not declared(stdin): import std/syncio

proc nmatch*(nCt = -1, xCt = -1, delim=":", patterns: seq[string]): int =
  ## Read lines from stdin, counts number of in-line matches of all patterns,
  ## and echo such counts and original lines to stdout (separated by `delim`).
  ## This is mostly a useful atom for analyzing/understanding file formats.
  let patterns = collect(for p in patterns: p.re)
  for line in stdin.lines:
    var sum = 0
    for p in patterns:
      for mch in line.findAll(p):
        sum.inc
      if nCt != -1 and sum < nCt:
        continue
      if xCt != -1 and sum > xCt:
        continue
      stdout.write sum, delim, line, '\n'

when isMainModule:
  import cligen
  dispatch nmatch, help={}

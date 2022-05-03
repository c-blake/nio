import nio, cligen/osUt, cligen

proc lp2term*(eor='\0', subEor=" ", index="", paths: Strings) =
  ## Concatenate len-prefixed buf repos `paths` to stdout as `eor`-terminated
  ## with optional index creation.
  var ix: NFile
  var off: int; var obuf: array[8, char]
  if index.len > 0:
    ix = nOpen(index, fmWrite)
  for path in paths:
    var r = rOpen(path)
    for k, i in r.keysAtOpen:
      if ix.f != nil: #XXX replace below mess with nice `ix.write off` API
        convert(ix.rowFmt.cols[0].iok, lIk, obuf[0].addr, off.addr)
        ix.f.nurite ix.rowFmt.cols[0].iok, obuf[0].addr
      off.inc k.len + 1
      stdout.replacingUrite k, eor, subEor
      stdout.urite eor
    r.close

when isMainModule:
  dispatch lp2term, help={"paths" : "{1|more paths to a .L file}",
                          "eor"   : "output record terminator",
                          "subEor": "string for rec term inside bufs",
                          "index" : "optional binary start-of-buf NIO file"}

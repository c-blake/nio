when not declared(addFloat): import std/[formatfloat, objectdollar]
import nio; from os import `/`

# Trailing '/' matters to disambiguate given name vs. given dir
let dir = "/dev/shm/"           # /dev/shm is reliably fast

type
  Foo {.packed.} = object
    ix: int32
    fl: float32
    name: array[16, char]

var data = newSeq[Foo](1_000_000)

for i in 0 ..< data.len:
  data[i].ix = int32(i mod 10)
  data[i].fl = i.float * i.float
  let buf = $data[i].fl
  copyMem data[i].name[0].addr, buf[0].unsafeAddr, max(buf.len, 15)

data.save dir                   # No arg => use PWD

# Another process/instance of this program would ordinarily be what
# reads/loads it back.  We do it here just to be self-contained.

var loaded = load[Foo](dir/"ix,fl,name.Nif16C")

for i in 0 ..< data.len:        # cmp old & new..just for kicks
  if data[i] != loaded[i]:
    echo "mismatch at ", i
    quit(1)

loaded.close

var alter = load[Foo](dir/"ix,fl,name.Nif16C", fmWrite)
alter[54321].fl = 12345.0       # edit example
alter.close

let loaded2 = load[Foo](dir/"ix,fl,name.Nif16C")

for i in 0 ..< data.len:        # show our edit..just for kicks
  if data[i] != loaded2[i]:
    echo "mismatch at ",i," got: ",loaded2[i]," orig: ",data[i]

Some feedback from people has seemed to be not really understanding how to
replicate certain styles of analysis with these tools which begs a question
for a worked out concrete problem.  That was done by others here:
https://h2oai.github.io/db-benchmark/ and we just replicate that in nio.  (You
have to click on the 5GB button to see comparable numbers, but on a different
machine.)

### Step 1: Make a CSV data set

install data.table in R
```
$ git clone https://github.com/h2oai/db-benchmark
$ Rscript \_data/groupby-datagen.R 1e8 1e2 0 0
```
### Step 2: Generate a fromSV parsing schema

Will want id strings to be dense integer labels; So use .N16C strings
```sh
nio i -si.N16C -d, G1_1e8_1e2_0_0.csv
```
This takes about 75 seconds & 6 MiB RSS for me.  Value fields are floats, not
auto-inferred integers & `float32` for a parsed value is probably good enough.
So manually edit the v[123] to be 'f f' with:
```sh
sed -i 's/^v\(.*\)i<TAB>d/v\1f<TAB>f/' *.sc # decimal ints->float32's
```
(In the above <TAB> is a hard-tab character...Yeah, yeah. Ctrl-V is not so
horrible.)

### Step 3: Run the parser to get some binary column files

```sh
nio f -s *.sc G1_1e8_1e2_0_0.csv
```
This takes about 130 seconds & 130 MiB for me, but could probably be brought
down to about 60% of that time with some re-used temporary variables rather than
malloc-free cycles in the parser, but the bigger speed-up would be parallelizing
the parsing which could probably yield single digit seconds.  Then all you have
is a lot of L1 resident hash tables managing the string -> dense integers ids.
Note that performance-wise it is virtually always better to do this hashing
exactly once, at induction of data into your system.

Anyway, you get:
```sh
ls -s
total 8601384
5070092 G1_1e8_1e2_0_0.csv           4 id2.N16C   390628 id4.Ni   390628 v2.Nf
      4 G1_1e8_1e2_0_0.csv.sc   390628 id2.Ni     390628 id5.Ni   390628 v3.Nf
      4 id1.N16C                 15628 id3.N16C   390628 id6.Ni
 390628 id1.Ni                  390628 id3.Ni     390628 v1.Nf
```

### Step 4: Run a "simple" query

If you had a real data set you use often with pre-prepared data repositories,
then this step would be all you needed.
```sh
nio q -b'var gs:seq[float]; let ids=initFileArray[array[16,char]]("id1.N16C")'\
      '(if id1 >= gs.len: gs.setLen id1+1); gs[id1] += v1' \
      -e'for i,s in gs: echo ids[i], " ", s' id1.Ni v1.Nf
```
If you care about run times on "big" data then it is faster to do an optimized
compile first and then run that, but this is less "REPL/ad hoc", naturally.

### Step 5: Now time it more "for real" since "benchmark" sets people off:

```sh
nim c --cc:gcc -t:-ffast-math -d:danger /tmp/qC3D.nim
/usr/bin/time /tmp/qC3D > out
0.08user 0.03system 0:00.12elapsed 100%CPU (0avgtext+0avgdata 783212maxresident)k
0inputs+0outputs (0major+12329minor)pagefaults 0swaps
```
Note that 783 MiB is much less than 5 GiB because only two 390 MiB iles need be
paged in.  The fact that all pagefaults are minor tells us this was DRAM only.
The fact that there were 12329 (\*4=49316) and not 783212 (./4=195803) tells us
the kernel was paging in about 4 pages at a time.  That might be boostable with
some `madvise` calls and is definitely boostable with Huge Page TLBs, probably
down to 80 ms.

As is, we get `390*2/.12=~6.5 GB/s` bandwidth on an i7-6700k that can do about
35 GB/s single core with 65ns latency.  So, this can likely be sped up a bit
even w/out parallelization.  With parallelization it can likely saturate my
DIMMs at about 8X faster, BUT it's already faster by a large margin than any
numbers I see on the results portion of that db-bench website.

For comparison, pandas-1.3.5 on the same machine takes ~4X longer at 0.45 sec {
not nearly 3 sec.  It seems pandas may have seen substantial speed ups in the
past 2 yrs. } No, I do not have patience to try to learn how to install and
configure the many other alternatives, but I am happy to try to help anyone
reproduce *this* addition. :-)

### Step 6: Maybe abstract & generalize

If this computational pattern arises often, then you could simplify your future
life with a little work.  A goal might be to be able to enter this instead:
```sh
nio q -b'var g=grp[array[16,char],float]("id1.N16C")' 'g.up id1,`+=`,v1' \
      -e'echo g' id1.Ni v1.Nf
```
If `nio.nim` did not already support the above then you could add
`~/.config/nio` with `-p'import gBy'`, etc. to make it available by default.

`nio.nim` does support this in ~15 lines of code, though.  So, you can just
`nim c --cc:gcc -t:-ffast-math -d:danger /tmp/qC09` to get a faster running
program.  With proper imports `float` can become `adix/stat.MovingStat` and
`+=` can become `push` or other such amendments.  For the curious/lazy, here
is that code, slightly trimmed for pedagogy:
```Nim
type #*** MICRO "GROUPBY" FRAMEWORK,BUT USER'S LIKELY WANT THEIR OWN
  Grp*[K,V] = object
    ks*: FileArray[K]
    vs*: seq[V]
proc grp*[K,V](path: string): Grp[K,V] =
  result.ks = initFileArray[K](path)
template up*[K,V](g: Grp[K,V], id, op, val) =
  if id >= g.vs.len: g.vs.setLen id + 1 # ensure room
  op g.vs[id], v1                       # incremental update
proc `$`*[K,V](g: Grp[K,V]) =
  for i, v in g.vs: result.add g.ks[i] & " " & v & "\n"
```
OR you might Step 6': take the `/tmp/qC3D.nim` program as a template & hack away
at it OR you could potentially take Step 6'' & do various Nim macro abstraction.

----------------------------------------------------------------------------

As a performance note, if you are tempted to make a type for concatenated keys
as in other elements of the db-bench suite, then depending upon your key entropy
/ data scales you may want to resist naive `Table`/`LPTabz` lookups inside the
main loop.  Hash lookups are much slower than array lookups -- even for integer
keys (e.g. `id1*100+id2`).  Instead you can create a new synthetic id and put it
in a new `.N16C` or whatever file that pre-computes every possible catenation of
2, say.  For `100*100` this is a not-so-bad L2 resident 10,000.  While you must
still do hash lookups in constructing new merged ids for each row, you need only
do this work *once*.

So, it may be much faster *IF* you have many follow-on queries to run, but the
"system" cannot guess whether one or many queries will happen in the future.
In this specific case, if the system can observe enough free storage, it may be
feasible to save the answer as you loop, BUT even this is not always possible,
e.g. under ENOSPC conditions.  So, maybe you grow a toggle - "fail on ENOSPC" |
"fallback to slower".  These toggles will then proliferate like rabbits and need
to be specified and be no easier to understand than the code itself, IMO.  For
reasons like this, I believe that "general purpose" DBs can never truly be as
fast as programmer-user optimized analysis pipelines.  The question is more "how
much" you lose -- 2X/5X/50X/1000X -- not "whether".  So, if data is big enough
to make performance a real concern, the answer to me is to make this programming
as easy as it can be which is in many ways a simpler problem.

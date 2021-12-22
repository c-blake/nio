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
head -n20000 G1_1e8_1e2_0_0.csv|nio i -si.N16C -d, ''
mv .sc G1.sc
```
Value fields are floats, not auto-inferred integers & `float32` for a parsed
value is probably good enough.  So manually edit the v[123] to be 'f f' with:
```sh
t=$(echo a|tr a \\t)
sed -i "s/^v\\(.*\\)i${t}d/v\1f${t}f/" G1.sc # decimal ints->float32's
```

### Step 3: Run the parser to get some binary column files

```sh
nio f -s G1.sc G1_1e8_1e2_0_0.csv
```
This takes about 64 seconds for me yielding:
```sh
ls -s
total 8601384
5070092 G1_1e8_1e2_0_0.csv      4 id2.N16C  390628 id4.Ni  390628 v2.Nf
      4 G1.sc              390628 id2.Ni    390628 id5.Ni  390628 v3.Nf
      4 id1.N16C            15628 id3.N16C  390628 id6.Ni
 390628 id1.Ni             390628 id3.Ni    390628 v1.Nf
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
nim c --gc:arc --cc:gcc -t:-ffast-math -d:release -d:danger /tmp/q1C4.nim
/usr/bin/time /tmp/q1C4 > out
0.08user 0.03system 0:00.12elapsed 100%CPU (0avgtext+0avgdata 783212maxresident)k
0inputs+0outputs (0major+12329minor)pagefaults 0swaps
```
Note that 783 MiB is much less than 5 GiB because only two 390 MiB iles need be
paged in.  The fact that all pagefaults are minor tells us this was DRAM only.
The fact that there were 12329 (\*4=49316) and not 783212 (./4=195803) tells us
the kernel was paging in about 4 pages at a time.  That might be boostable with
`madvise` and is definitely boostable with Huge Page TLBs, likely down to 80 ms.

As is, we get `390*2/.12=~6.5 GB/s` bandwidth on an i7-6700k that can do about
35 GB/s single core with 65ns latency.  So, this can likely be sped up a bit
even w/out parallelization, BUT it's already faster by a large margin than any
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
`nim c --cc:gcc -t:-ffast-math -d:danger /tmp/q3C6` to get a faster running
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
OR you might Step 6': take either program as a template & hack away at it OR you
could potentially take Step 6'' & do various Nim macro abstraction.

### Extra Credit: Going Parallel

```sh
nio q -p'import os' -b'var g=grp[array[16,char],float32]("id1.N16C")' \
      'g.up id1,`+=`,v1' -e'g.vs.save "out" & getEnv("p")' id1.Ni v1.Nf

nim c -d:danger /tmp/qF87.nim # compile fast running version

/usr/bin/time sh -c 'for p in 0 1 2 3
  do p=$p ROWS=$((25000000*p)):$((25000000*(p+1))) /tmp/qF87 &
  done; wait'    # run it in parallel
```
I get about 0.040 seconds.  So, ~3.0X speed-up with 4 cores or now ~11X faster
than pandas-1.3.5.

The output here is pure binary in `outK.Nf`.  To merge it, just do sub-millisec:
```sh
nio xsum -of out?.Nf > out.Nf
```
and then if you really need ASCII output, say to compare:
```sh
nio pr id1.N16C out.Nf
```
(Yes, yes, this summation could also be parallelized if it were a big enough
problem to warrant such.  For this calculation on my test box, the combined time
for both nio xsum & print is < 2 ms which is below measurement error, TBH.)

### Post Script: Some other observations

As a performance note, if you are tempted to make a type for concatenated keys
as in other elements of the db-bench suite, then depending upon your key entropy
/ data scales you may want to resist naive `Table`/`LPTabz` lookups inside the
main loop.  Hash lookups are much slower than array lookups -- even for integer
keys (e.g. `id1*100+id2`).  For example, the built-in `nio kreduce` is about 50X
slower (although some of this time is computing unneeded stats incrementally):
```
nio kreduce -f.0f --s,= -ssum -g id1.Ni v1.Nf
```
Instead you can create a new synthetic id and put it in a new `.N16C` style
file that pre-computes every catenation.  For `100*100` this is a not-so-bad L2
resident 10,000 accumulators.  While you must still do hash lookups in
constructing new merged ids for each row, you need only do this work *once*
if these new ids are useful anyway.

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

### Double Extra Credit: Parallel Parses

The bulk of the calculation is clearly parsing/loading the data.  Now, maybe the
data will be used many times and the one-time cost at induction into your system
is no big deal.  Or maybe, as in this demo, it dominates for a one/few times
calculation and you'd like to speed it up.

This is harder than it might first appear.  It's easy to spit an input file and
search for newlines to create independent segments (assuming newlines are a
reliable record delimiter).  BUT interning string data into dense ids (to avoid
hashing in the group loop) does not lend itself to either reliable unlocked or
efficient post facto merge methods.

Data statistics of this particular benchmark are misleading here.  Almost all
novel ids are introduced in a very small fraction of the data set.  So, locks
to update hash tables will almost never be needed/contended.  This may be a
pretty unrepresentative situation.

To speed up the situation more generally, you must shard the *whole calculation*
and merge the small (by presumption) final answers.  This, however, fixes the
amount of parallelism in your parsed data repository making it not scale up to
"bigger computers".  Here is an example shell script doing this:
```
#!/bin/sh
data="G1_1e8_1e2_0_0.csv" #prof PS4='+$EPOCHREALTIME ' sh -x pgby
head -n10000 G1_1e8_1e2_0_0.csv|nio i -si.N16C -d, '' #mk schema
t=$(echo a|tr a \\t)
sed -i "s/^v\\(.*\\)i${t}d/v\1f${t}f/" .sc  # adjust schema
sed -i "s/^id[2-6].*/_${t}ignore/" .sc      # only parse needed
sed -i "s/^v[2-3].*/_${t}ignore/" .sc       # only parse needed
# Only ~0.010 sec up to here                # Now: Actual work
part -i1 -n0 $data                          # partition 1.12 sec

for d in 0*; do (cd $d; nio f -s ../.sc $data) & done
wait  # 4.93 sec from 1st cd                # parallel parse

nio q -b'var g=grp[array[16,char],float]("id1.N16C")' \
      'g.up id1,`+=`,v1' -e'echo g' id1.Ni v1.Nf -r=off -oq
nim c --verbosity:0 --cc:gcc -d:release -d:danger /tmp/q.nim
# 2.93 sec to do optimized build

for d in 0*; do (cd $d; /tmp/q >out) & done
wait                                        # parallel groupBy
# Combine results; About 0.002 sec more
cat 0*/out | awk '{c[$1]+=$2} END{for(k in c)print k,c[k]}' >out
: dummy # 0.068 sec total from 1st cd       # get last timestamp
```
This reduces the "load" time to under 5 seconds.  Meanwhile, using the same
trimmed schema parsing only `id1` & `v1` takes about 16 seconds.  So, parallel
scale up is again about 3x on 4 cores.  Also, again we get a rather large "it
all just depends on what you *really* need and *when*" factor.

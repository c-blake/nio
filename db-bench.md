Some feedback from people has seemed to be not really understanding how to
replicate certain styles of analysis with these tools which begs a question
for a worked out concrete problem.  That was done by others here:
https://h2oai.github.io/db-benchmark/ and we just replicate that in nio.  (You
have to click on the 5GB button to see comparable numbers, but on a different
machine.)

Step 1: Make a CSV data set
===========================

install data.table in R
```
$ git clone https://github.com/h2oai/db-benchmark
$ Rscript \_data/groupby-datagen.R 1e8 1e2 0 0
```
Step 2: Generate a fromSV parsing schema
========================================

Will want id strings to be dense integer labels; So use .N16C strings
```sh
nio i -si.N16C -p'c2tsv<$1' G1_1e8_1e2_0_0.csv
```
Value fields are floats not auto-inferred integers and `float32` for a parsed
vaue is probably good enough.  So manually edit the v[123] to be 'f f' with:
```sh
sed -i -e 's/^v\(.*\)i     d/v\1f  f/' *.sc   # decimal ints->float32's
```

Step 3: Run the parser to get some binary column files
======================================================
```sh
nio f -sG1_1e8_1e2_0_0.csv.sc G1_1e8_1e2_0_0.csv
```
This takes about 140 seconds & 130 MiB for me, but could probably be brought
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

Step 4: Run a "simple" query
============================
If you had a real data set you use often with pre-prepared data repositories,
then this step would be all you needed.
```sh
nio q -b'var gs:seq[float]; let ids=initFileArray[array[16,char]]("id1.N16C")'\
      '(if id1 >= gs.len: gs.setLen id1+1); gs[id1] += v1' \
      -e'for i,s in gs: echo ids[i], " ", s' id1.Ni v1.Nf
```
If you care about run times on "big" data then it is faster to do an optimized
compile first and then run that, but this is less "REPL/ad hoc", naturally.

Step 5: Now time it more "for real" since "benchmark" sets people off:
======================================================================
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
down to 80 ms.  As is, we get `390*2/.12=~6.5 GB/s` bandwidth on an i7-6700k
that can do about 35 GB/s single core with 65ns latency.  So, this can likely be
sped up a bit even w/out parallelization, likely to about 8X total to saturate
my DIMMs, BUT it's already faster by a large margin than any numbers I see on
the results portion of that db-bench website.  pandas-1.3.5 on the same machine
takes ~4X longer at 0.45 seconds not nearly 3 seconds.  It seems pandas may have
seen substantial speed ups in the past 2 yrs.  No, I do not have patience to try
to learn how to install and configure the many other alternatives.

Step 6: Maybe abstract & generalize
===================================
If this computational pattern arises often, then you could simplify your future
life with a little work.  A goal might be to be able to enter this instead:
```sh
nio q -b'var g=IGrouper("id1")' 'g.add(id1,`+=`,v1)' -eg.report id1.Ni v1.Nf
```
with maybe a `~/.config/nio` file with `-p'import groupBy'` or such.  The idea
is some constructor/add pair can work with any incremental operator, like `+=`
or `adix/stat.MovingStat.push` or whatnot and besides dense integer `IGrouper`
there might be an `HGrouper` helper for hash keys instead.

OR you might Step 6': take the `/tmp/qC3D.nim` program as a template & hack away
at it OR you could potentially take Step 6'' & do various Nim macro abstraction.

Some feedback from people has seemed to be not really understanding how to
replicate certain styles of analysis with these tools which begs a question
for a worked out concrete problem.  That was done by others here:
https://h2oai.github.io/db-benchmark/ and we just replicate that in nio.  (You
have to click on the 5GB button to see comparable numbers, but on a different
machine.)

Step 1. Make a CSV data set.

install data.table in R

```
$ git clone https://github.com/h2oai/db-benchmark
$ Rscript \_data/groupby-datagen.R 1e8 1e2 0 0
```

Step 2. Generate a fromSV parsing schema.

Will want id strings to be dense integer labels; So use .N16C strings

```
$ nio i -si.N16C -p'c2tsv<$1' G1_1e8_1e2_0_0.csv
```

`float32` on both input and output is probably good enough so manually edit the
produced v[123] to be 'f f'

Step 3. Run the parser to get some binary column files.

```
$ nio f -sG1_1e8_1e2_0_0.csv.sc G1_1e8_1e2_0_0.csv
```

Step 4. Run a "simple" query.  (If you had a real data set you use a lot with pre-prepared data
repositories, then this would be all you needed to do.)

```
$ nio q -b'var gs: seq[float]; let ids=initFileArray[array[16,char]]("id1.N16C")'\
        '(if id1 >= gs.len: gs.setLen id1+1); gs[id1] += v1' \
        -e'for i,s in gs: echo ids[i], " ", s' id1.Ni v1.Nf
```

If you care about run times on "big" data then it is faster to do an optimized
compile first and then run that, but this is less "REPL/ad hoc", naturally.

Step 5.: ***Now time it*** more for real:
```
$ nim c --cc:gcc -t:-ffast-math -d:danger /tmp/qC3D.nim
$ /usr/bin/time /tmp/qC3D > out
0.08user 0.03system 0:00.12elapsed 100%CPU (0avgtext+0avgdata 783212maxresident)k
0inputs+0outputs (0major+12329minor)pagefaults 0swaps
```
--------------------------------

If this comes up a lot, then you could Step 6. Abstract at the library level.
E.g., you could pretty easily reduce the above query to something like

```
$ nio q -b'gbSetup("id1", `+=`)' 'gBadd()' -ereport id1.Ni v1.Nf
```

with maybe a `~/.config/nio` file with `-p'import groupBy'` or maybe
`import pandasALike` or something like that.  The idea being some `gB`
macro/template might work with any incremental operator, like `+=` or
`adix/stats.MovingStat.push` or whatnot and `gbSetup` just sets up some
flag that `report` can consult to produce the right thing.

OR you might Step 6': take the `/tmp/qC3D.nim` program as a template and hack
away at it.

You could potentially take Step 6'' and do a bunch of in Nim macro abstraction.

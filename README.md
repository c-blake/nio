### Overview

The basic idea here is to take fully seriously the old chestnut that file name
extensions tell users contained formats in order to make data access efficient.
Specifically, extensions are a terse syntax inspired by Python "struct formats",
but using a simpler rule - the type code is just the first letter of each C type
(with the exception of long double, an already exceptional thing).  Uppercase is
unsigned; Lowercase is signed.  Elaborated, the full syntax is one or more:
```
  [<COUNT[,..]>]<c|C|s|S|i|I|l|L|f|d|g>
```
where
```
  c: signed (c)har    C: unsigned (C)har
  s: signed (s)hort   S: unsigned (S)hort
  i: signed (i)nt     I: unsigned (I)nt
  l: signed (l)ong    L: unsigned (L)ong
  f: (f)loat          d: (d)ouble          g: lon(g) double
```
The number of rows is inferred from a file size (which could be a length-prefix
in some message buffer context) or in stdio streams just the number of buffers
read.  Some examples:
```
  hey.NS        a column-vector (Nx1 matrix) of unsigned shorts
  foo.N10f      an Nx10 matrix of floats
  bar.N2i4d     a table of int 2-vectors and double 4-vectors
  covs.N10,10f  a vector of 10 by 10 covariance matrices
```
While learning the syntax is needed to do streaming/pipe style calculations, you
can also stow the format inside a file of a parallel name (e.g. "foo" & ".foo").

This setup is usable, flexible, & efficient.  It can perhaps cure you from your
likely addiction of parsing & re-parsing ASCII numbers which is up to hundreds
of times slower than modern SIMD FP operations.[^1]

Unpacking other linearized/serialized marshal formats often requires at least
iterating over all data, but often has decompression work woven in.  NIO tries
to allow "mmap & go" when feasible.  This is "***infinite GB/s***".  In a more
accurate sense, start-up cost is as fixed as opening random access files can be.
(This is what DB software has always done and should not surprise the savvy.)

More documentation can be had by just running `nio` with no arguments or `nio h`
for a big help dump.  `nio` is both a library usable via `import nio` and a
[cligen](https://github.com/c-blake/cligen) multi-command.  So, the shortest
unique prefix for subcommand names (and long option names) is sufficient.  The
[FAQ](https://github.com/c-blake/nio/tree/main/FAQ.md) has more motivation.

One can do some things with the `nio` command, but the main point of the design
is to be extensible by actual programmers `import`ing, `nOpen`/`initFileArray`
ing, etc., writing their own libs & tools either on top or off to the side even
in other programming languages.  Extended tools/logic must document themselves,
but they can, e.g. share `n-foo` namespaces if desired.  (Note that `nio zip`
naming comes from functional PLs|real world clothing zippers - not data
compression tools.)

ETL convenience tools live in `utils/`.  E.g., `transpose` can be useful in the
context of schema writing (as in `c2tsv < foo | head | transpose > editMe.sc`).

### CLI Usage Vignette

Here is a little usage vignette using simulated data.  First we will show some
steps and then explain them.  To start, you will first need to compile & install
`demo/tabGen.nim` in your `$PATH`.  Something like this may do the trick:
```sh
nimble install cligen   # may not be needed
git clone https://github.com/c-blake/nio
cd nio
n="nim c -d:danger"
$n nio && $n utils/c2tsv && $n demo/tabGen &&
  install -cm755 nio utils/c2tsv demo/tabGen ~/bin
```
After that, you can save this blurb to some demo.sh and run "sh demo.sh":
```sh
#!/bin/sh
t=/usr/bin/time
tabGen 1_000_000 4 > f 2> f.sc        # generate million*4 table
head -n3 < f                          # look @top
head f.sc                             # look @parsing schema
$t nio fromSV -s f.sc /dev/stdin < f  # parse the data into NIO
ls                                    # peruse some files
nio pr a.Nf b.Nf%.5f | head -n3       # print human readable
$t nio zip a.Nf b.Nf > ab.Nff         # stitch cols together
nio pr ab.Nff%.9f%.5f | head -n3      # print same vals another way
$t nio moments [a-z].N*               # compute some summary stats
```
and get output that looks like this (i7-6700k @4.7GHz; 8MiB L3):
```
a,b,c,d
2.380153181848329,-2.279945642690398,-0.6395001602969651,7.233130606792596
-0.025225344508444,2.21176551984741,0.494893265790349,0.4640431696829914
0.79user 0.04system 0:00.72elapsed 114%CPU (0avgtext+0avgdata 2512maxresident)k
0inputs+0outputs (0major+392minor)pagefaults 0swaps
a.Nf
b.Nf
c.Nf
d.Nf
f
f.sc
2.380153	-2.27995
-0.02522535	2.21177
0.1616571	3.55054
0.02user 0.00system 0:00.02elapsed 100%CPU (0avgtext+0avgdata 7920maxresident)k
0inputs+0outputs (0major+250minor)pagefaults 0swaps
2.380153179	-2.27995
-0.025225345	2.21177
0.161657065	3.55054
a.Nf:0 min: -4.772 max: 4.811
b.Nf:0 min: -9.172 max: 10.99
c.Nf:0 min: -11.89 max: 16.50
d.Nf:0 min: -16.27 max: 25.47
0.04user 0.00system 0:00.04elapsed 97%CPU (0avgtext+0avgdata 18268maxresident)k
0inputs+0outputs (0major+377minor)pagefaults 0swaps
```
### Going Faster

Performance savvy readers may note, of the final line, that 40 ms for 4 million
numbers is weak performance.  10 nanosec/number or 50 clock cycles/num or lowly
16 MB/40ms = 400 MB/s is not great for what could be vectorized min/max in a
perfectly predictable pipeline.  This is partly because `nio moments` uses
`std/stats.RunningStat` which is accuracy semi-optimized, not speed-optimized.

[demo/datGen](https://github.com/c-blake/nio/tree/main/demo/datGen.nim) shows
how easy it is to just ***stay in binary the whole time***:
```sh
#!/bin/sh
t=/usr/bin/time
$t datGen 1_000_000 4               # generates abcd.Nffff
$t nio rip -i abcd.Nffff a b c d    # rip apart into column files
$t favg [a-d].Nf                    # Does 100 loops by default!
```
Compiling the tiny [demo/favg](https://github.com/c-blake/nio/tree/main/demo/favg.nim)
with `-d:danger` for me results in a run-time on that same machine of 0.045 sec
for 100 passes or 0.45 ms/pass.  This is 40ms/.45=~ ***90X faster*** or about
16/.45 = 35.5GB/s.  Memory BW on this particular 2016 Linux box tops out at
~45GB/s (with >=3 cores pulling).

It is straightforward (but demo-messy) to [split the loop into `p`
parts](https://forum.nim-lang.org/t/9115#59567) & grand total over thread/proc
subtotals to saturate memory BW which here would be (45/35=~1.3X) better.[^2] In
this example, since outputs are tiny subtotals, it's fine to memory map files &
then `fork` to engage hardware parallelism with processes via `cligen/procpool`.
Were outputs giant, kids could write to NIO files and return pathnames.[^3]

See [db-bench.md](https://github.com/c-blake/nio/tree/main/db-bench.md) for
another worked out example, perhaps easier to compare to other systems.

[^1]: Seriously -- SIMD's go at L1 cache bandwidth which are order 100s of GB/s
while parsing at even 1 GB/s is a challenge and printing/binary->ASCII is even
slower.

[^2]: More recent server/HEDT models have much higher peak parallel/peak single
core BW ratios than 1.3 (more like 15+X).  This pushes optimizers to parallel
complexities simply to saturate DIMMs.

[^3]: Once you are whole CPU/system optimizing, what idea is best quickly
becomes "it depends".

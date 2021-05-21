The basic idea here is to just take fully seriously the old chestnut that file
name extensions inform users as to the contained format.  We just append to
binary files a terse syntax inspired by but simpler than various Perl/Python
"pack formats" and build up APIs and tools around that.  The syntax is simple
and easy to remember (I think) because it is derived from the C programming
language family.  Most programmers have those basic "CPU types" memorized.
With the lone exception of long double (an already exceptional thing), the type
code is just the first letter of each C type.  Capitals are unsigned while
lowercase are signed.  That's it.

The full syntax is one or more:
```
  [<COUNT[,..]>]<c|C|s|S|i|I|l|L|f|d|g>
```
where
```
  c: signed char    C: unsigned char
  s: signed short   S: unsigned short
  i: signed int     I: unsigned int
  l: signed long    L: unsigned long
  f: float          d: double           g: long double
```

Some examples:
```
  hey.NS        a column-vector (Nx1 matrix) of unsigned shorts
  foo.N10f      an Nx10 matrix of floats
  bar.N2i4d     a table of int 2-vectors and double 4-vectors
  covs.N10,10f  a vector of 10 by 10 covariance matrices
```

I have found this to be surprisingly usable and efficient.  It can perhaps cure
you from your addiction of parsing & re-parsing ASCII numbers which is up to
hundreds of times slower than modern SIMD FP operations.  (Seriously, those are
memory bandwidth bound even in L1 cache and L1 can go at 200 GB/s while parsing
at even 1 GB/s is a challenge and printing/formatting is even slower.)

More documentation can be had by just running `nio` with no arguments or `nio h`
for a big help dump.  `nio` is a [cligen](https://github.com/c-blake/cligen)
multi-command.  So the shortest unique prefix for subcommand names (and long
option names) is sufficient.

While one can do a few things with the `nio` command, the main point of the
design is to be extendable by actual programmers doing `import nio` and nOpen,
read, write, mOpen to mmap the whole thing, etc.  Such extended tools/logic must
have their own documentation, but they can share the `n-foo` namespace if they
want.  Note that `nio zip` is named after functional PL terms|real world
clothing zippers and is unrelated to data compression.  Convenience tools live
in `utils/`.  E.g., `transpose` is often useful in the context of schema writing
(as in `c2tsv < foo | head | transpose > editMe.schema`).

Here is a little usage vignette using simulated data.  First we will show some
steps and then explain things.  To start, you will first need to compile &
install in your $PATH utils/tabGen.nim.  Something like this may do the trick:

```
git clone https://github.com/c-blake/niohttps://github.com/c-blake/nio
cd nio
n="nim c -d:danger"
$n nio && $n utils/c2tsv && $n utils/tabGen &&
  install -cm755 nio utils/c2tsv utils/tabGen ~/bin
```
After that, you can save this blurb to some demo.sh and run "sh demo.sh":
```
#!/bin/sh
t=/usr/bin/time
tabGen 1_000_000 4 > f 2> f.sc        # generate a million*4 table
head -n3 < f                          # look at the top
head f.sc                             # look at the parsing schema
$t nio fromSV -s f.sc /dev/stdin < f  # parse the data into NIO.
ls                                    # peruse some files
nio pr a.Nf b.Nf%.5f | head -n3       # print human readable
$t nio zip a.Nf b.Nf > ab.Nff         # stitch/zip together a couple cols
nio pr ab.Nff%.9f%.5f | head -n3      # print those, too
$t nio moments [a-z].N* )             # compute some summary stats
```
and get output that looks like this:
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
a.Nf:0 min: -4.771832466125488 max: 4.811161041259766
b.Nf:0 min: -9.17171573638916 max: 10.99259853363037
c.Nf:0 min: -11.8895788192749 max: 16.49841499328613
d.Nf:0 min: -16.26732063293457 max: 25.47092437744141
0.04user 0.00system 0:00.04elapsed 97%CPU (0avgtext+0avgdata 18268maxresident)k
0inputs+0outputs (0major+377minor)pagefaults 0swaps
```

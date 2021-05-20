0) Hey!  This FAQ is more like a design document than "frequent" questions.

    That's not a question. ;-)  Also, please just forgive the poetic license.

1) What is NIO?

    "Nio" is a wrath-filled & muscular but "benevolent" King guardian of the
    Buddha, outside many Buddhist temples.  Homonymic "Neo" is the messiah in
    The Matrix trilogy and a Nim linear algebra package. ;)

    This NIO is a "Native/Numerical IO" system.  It consists of a library and a
    set of command-line tools designed to manipulate simple arrays of structs
    stored in files.  One dimensional/vector layouts are like structs of arrays
    while rank3 and above also enjoy some support (eg. time series of matrices).

    The basic idea is to have files be self-describing with zero cost "parsing".
    I.e. mmap & go or read a row at a time into one fixed buffer where packed
    field access is mediated by optimizing compilers.  In addition to this zero
    overhead access, NIO libraries also provide very low cost converting reads
    that can translate missing value/NA codes.  This allows heterogeneous rows
    to be read into heterogeneous program variables.  Data may not fit in memory
    and being able to mmap & go lets one be "fluid" about data/program lifetime
    & computations, subdividing however is convenient.

1a) Why not bio for binary IO?

   To avoid confusion with block/buffered IO which is "similar but different"
   and focus on the main points of nativeness/number-hood.

2) How do I express a row format?

    The syntax is simple and easy to remember because it is derived from the C
    programming language family.  Most programmers have those basic "CPU types"
    memorized.  With only one exception, the type code for a type is first
    letter of each C type.  Capitals are unsigned while lowercase are signed.
    That's it.  The exception is that long doubles are signified by a lower g
    (like the printf %g format specifier).  (You may want to avoid long doubles
    as their availability/size varies across CPUs.) Some examples:

      hey.NS        a column-vector (Nx1 matrix) of unsigned shorts
      foo.N10f      an Nx10 matrix of floats
      bar.N2i4d     a table of int 2-vectors and double 4-vectors
      covs.N10,10f  a vector of 10 by 10 covariance matrices

    The full syntax is one or more:
      [<COUNT[,..]>]<c|C|s|S|i|I|l|L|f|d|g>
    where
      c: signed char    C: unsigned char
      s: signed short   S: unsigned short
      i: signed int     I: unsigned int
      l: signed long    L: unsigned long
      f: float          d: double           g: long double

3) How can it be "programming language agnostic" if it uses C type names?

    Dependence upon C is only mnemonic.  C's ubiquity at the system level means
    most/almost all prog.langs have exposure to C, e.g. for foreign function
    interfaces.  A great many programmers who would never call themselves "C
    programmers" nevertheless find the above table easy.  I have accessed NIO
    files from C, C++, Python, and Nim.  The format is all the same.

4) NIO isn't CPU architecture neutral?  Aaaawhaaaaa?

    Architecture neutrality was always over-rated for files for data analysis or
    programmatic interaction, the main use case of NIO.  Neutrality has become
    ever more outmoded since the early 2000s as the Intel-ARM hegemony has grown
    ever more dominant.  To preserve mmap&go capability, one would need at least
    2 copies, possibly made with some new `nio bswap` tool.  `dd swab` may be
    enough for simple row formats.  It is possible to have the nio.(read|write)
    interfaces do any needed byte swaps, at some IO performance hit.  I haven't
    personally needed this capability in decades using these ideas, though I am
    not averse to some PR for it.

    While rare today, if you really have a cluster of heterogeneously endian
    machines all computing against shared data and you cannot store two copies
    then NIO may not add much value over other approaches since it loses full
    optimizing compiler mediated access, though it may still be "simpler".
    Nothing can be all things to all folks in all circumstances.  As they say,
    your mileage may vary, but those are many "ands" and perfect is the enemy of
    the good.

5) Why is the type syntax so darn terse?  Why no file headers?

    People use terse codes for outputs (like printf).  Why not for inputs?
    The input side is simpler since there is no base-10/16/.. variation.

    One of the draws of the so-called Unix Philosophy is a simple consistent
    newline delimited row format, but text is particularly inefficient for
    numbers.  One way to view NIO is the simplest possible generalization of
    this to binary formats.  Simplest means no headers.

    When used for shell pipelines prototyped interactively a terse syntax that
    does not require shell quoting is helpful.  The utility of such pipelines
    also makes users confront & thus quickly learn/memorize row format syntax.
    Row format transformations (such as combining columns) render explicitly.
    Being explicit has pros as well as cons & we won't settle that debate here.

6) What about case-insensitive file systems?

    I never use these myself and fail to see the appeal, but there are (at
    least) two kinds of case-insensitivity: A) fully forgetful and B)
    store/present with case variation but match insensitively.  Since it is
    already a type violation to alter a row format after creation, and since
    users present pathnames often generated by case-preserving operations, case
    B may cause little trouble.  For case A) (and any case B gotcha/aesthetics),
    when there is no .N filename extension, NIO tools will look for "dot files"
    of the same name but with a leading '.'.  These can contain a string just
    like the filename extension.  For example, "dateFoo" might have ".dateFoo"
    with contents "if@dates%s%.3f".  Then users can simply say `nio p dateFoo`
    or otherwise `nOpen("dateFoo")`.

7) What do you mean "NIO formalizes/generalizes existing practice"?

    Unix /var/run/utmp & /var/log/wtmp have had this format for decades.  This
    sort of works as a poor man's utmpdump/last:
      ln -s (/var/run/utmp|/var/log/wtmp) \
        typePidLineIdUserHostExitSessTvIp6rsv.Nsi32C4C32C256C2sl2l4i10C
      nio p typePid*
    (or on BSD about typePadTvIdPadPidUserLineHostRsv.Ns6c2l8C4ci32C16C128C64C)

    Python's NumPy has had a `save` method to do this from the very beginning..
    simply without a corresponding "reshape" metadata on load or mmap.  NYSE TAQ
    data also used to ship .BIN files that were back-to-back structs.  I believe
    the 2010s saw option exchange data feeds move to this.  Similarly, a 128xM
    24-bit/3-byte color image raster file can literally just be a binary file
    "foo.N128,3c".  Standard numerical code using this access library could then
    just access the raster or similar variants directly.

    So, the general notion at play here is in use and has been for decades, but
    its use is clunky/ad hoc requiring C FFIs, Perl/Python "pack" modules.  One
    standard such as the NIO suffix format suffices to write *general* tools
    that can handle any layout, transformation, multiple OSes, etc. as well as
    avoiding mucking bout with `hexdump`, `od`, etc.

8) Ok...So, how do I use these tools...

    Just run `nio` with no arguments or `nio h` for a big help dump.  The whole
    point of the design is to be extendable by programmers doing import nio and
    then nOpen/reads/writes/etc. but such extended tools/logic must have their
    own documentation, but they can share the `n-foo` namespace if they want.
    Note that `nio zip` is named after functional PL terms|real world clothing
    zippers and is unrelated to data compression.  `transpose` is often useful
    in the context of schema writing (as in `c2tsv < foo | head | transpose >
    editMe.schema`).

9) Why not a relational database?

    NIO is for use by programmer data analysts..perhaps advanced programmers who
    think they can IO optimize better than query analyzers or who have custom
    analytics or other needs to integrate with "real" prog.lang libraries that
    is all too painful in SQL/SQL stored procedures (the latter of which are
    often very non-DB portable).  Repayment for low-levelness is true zero
    overhead IO (and easy access to SIMD speeds, as yet another example).
    Updates are also often rare to never; Yet analyses can hit large data sets
    hundreds if not hundreds of thousands of times.  So, vectors/tables/tensors
    are apt while ACID is over-engineered and any cost is waste.  In short,
    there seems definite value to non-DB persistence formats.  The closest
    analogue to envisioned NIO use cases is HDF5.

9a) Ok..Why not HDF5?

   HDF5 heralds from NetCDF and earlier formats all designed to work with very
   limited OS FSes of the 1970s & 1980s..E.g. DOS 8.3 filenames or VMS limits.
   Consequently, these formats re-create archive functionality (like tar/zip)
   instead of just using the filesystem.  The 1980s are mostly behind us all.
   Using regular files has many charms.  Not the least charm is that users all
   know how to find & manage files & directories.  This includes many & varied
   compression programs to compress files - not just library access to codecs
   but programs like zstd xyz or lz4 pdq etc.  Bundling functionality in HDF5
   might *seem* nice, but can also be a limiting luxury trap, e.g. to support
   compression libs, ACLs, rsync optimization or all other things files & dirs
   provide.  Meanwhile, files & dirs are universal; Users know what to do with
   tar/zip archive files or with dirs of files they want to bundle.

9b) Doesn't KDB do this already?

   Sort of, but not really.  For example, back around the turn of the 2010
   decade one could use `plzip` or `pixz` to get multi-GB/s scale IO from
   multi-threaded decompression via backing store IO >20x less.  Nothing like
   this was at all available for kdb, though it has surely grown support for
   more compression modes.  Like HDF5, kdb is "overly bundled" in its concept
   and better factoring wins the day.  To do this with the NIO model is easy.
   Similar comments probably apply to other efforts like Apache Parquet&Arrow.

   As far as I can tell, NIO is alone in striving for a flexible column/vector
   |matix|tensor "store" that strives to just solve **just one simple problem**
   - not parsing & reparsing - and understands that filesystems are already
   hierarchical (and some even support random access to compressed data, but
   streaming row-at-a-time perf is usually better).  All that said, the NIO
   solution is *so* simple that it seems not improbable *someone* else has
   devised a close analogue.

10) Ok..Why not a full object graph?

   This could be a good addition.  Generalizing how string repositories work to
   allow more arbitrary pointers may not even be hard.  Always insert-at-end/
   mark deleted with some kind of eventual GC may retain a mostly usable "run
   right off of files" for broader use cases, but notably will need parallel
   GC'd types like `seq` in Nim and possibly a lot of GC machinery.  PRs like
   this are welcome, but note that relDBs/HDF5/etc. have somehow been useful
   for decades without this feature.

11) Why no bit fields?

    This is a good question.  ".N3:i5:i" instead of ".NC" with some prohibition
    on prefix multipliers might work as a syntax.  The problem is mostly that it
    is much less obvious what zip, rip, cut and similar transformations mean in
    the presence of bit fields.  And obviousness is good.

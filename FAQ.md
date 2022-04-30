# Index

Various questions/rejoinders:

 0.  [Hey!  This FAQ is more like a design document than "frequent" questions.](#0-hey--this-faq-is-more-like-a-design-document-than-frequent-questions)
 1.  [What is NIO?](#1-what-is-nio)
 2.  [How can it be "programming language agnostic" if it uses C type names?](#2-how-can-it-be-programming-language-agnostic-if-it-uses-C-type-names)
 3.  [NIO isn't CPU architecture neutral?  Aaaawhaaaaa?](#3-nio-isnt-cpu-architecture-neutral--aaaawhaaaaa)
 4.  [Why is the type syntax so darn terse?  Why no file headers?](#4-why-is-the-type-syntax-so-darn-terse--why-no-file-headers)
 5.  [What about case-insensitive file systems?](#5-what-about-case-insensitive-file-systems)
 6.  [What do you mean "NIO formalizes/generalizes existing practice"?](#6-what-do-you-mean-nio-formalizesgeneralizes-existing-practice)
 7.  [Why not Pandas/R dataframes/Excel/JSON/XML/etc.?](#7-why-not-pandasr-dataframesexceljsonxmletc)
 8.  [Why not a relational database like SQLite/MySql/etc.?](#8-why-not-a-relational-database-like-sqlitemysqletc)
 9.  [Ok..Why not HDF5?](#9-okwhy-not-hdf5)
 10. [Don't KDB/APL derivative xyz/Tea Files do this already?](#10-dont-kdbapl-derivative-xyztea-files-do-this-already)
 11. [Ok..Why not a full object graph?](#11-okwhy-not-a-full-object-graph)
 12. [Why no bit fields/discriminated unions/even fancier things?](#12-why-no-bit-fieldsdiscriminated-unionseven-fancier-things)
 13. [What about filename limits, like 255 chars?](#13-what-about-filename-limits-like-255-chars)
 14. [Why don't you just ALWAYS do column IO?](#14-why-dont-you-just-always-do-column-io)
 15. [Why don't you just always do simple tensor IO like x.N10,10f?](#15-why-dont-you-just-always-do-simple-tensor-io-like-xn1010f)
 16. [Isn't the "type system" barely worthy of the name?](#16-isnt-the-type-system-barely-worthy-of-the-name)
 17. [Why so many string repository styles?](#17-why-so-many-string-repository-styles)
 18. [Why no variable width inline strings like protobufs/flatbuffers/etc.?](#18-why-no-variable-width-inline-strings-like-protobufsflatbuffersetc)
 19. [What about "schema versioning"/evolution?](#19-what-about-schema-versioningevolution)
 20. [This is all hopelessly hard to use compared to SQL](#20-this-is-all-hopelessly-hard-to-use-compared-to-sql)
 21. [Ok. I am sold, BUT what about MY programming language?](#21-ok--i-am-sold-but-what-about-my-programming-language)

### 0: Hey!  This FAQ is more like a design document than "frequent" questions.

That's not a question. ;-)  Also, please just forgive the poetic license.

### 1: What is NIO?

"Nio" are [two wrath-filled & muscular but "benevolent" King
guardians](https://en.wikipedia.org/wiki/Nio) of the Buddha, outside many
Buddhist temples.  Homonymic "Neo" is the messiah in The Matrix trilogy and a
Nim linear algebra package. ;)  (And you can pronounce it like "eye-oh" if you
prefer).

This NIO is a "Native/Numerical IO" system.  It consists of a library and a set
of command-line tools designed to standardize & simplify manipulation of simple
arrays of structs stored in files.  One dimensional/vector layouts are like
structs of arrays while rank3 and above also enjoy some support (eg. time series
of matrices).

The basic idea is to have files be self-describing with zero cost "parsing".
I.e. mmap & go or read a row at a time into one fixed buffer where packed field
access is mediated by optimizing compilers.  In addition to this zero overhead
access, NIO libraries also provide very low cost converting reads that can
translate missing value/NA codes.  This allows heterogeneous rows to be read
into heterogeneous program variables.  Data may not fit in memory and being able
to mmap & go lets one be "fluid" about data/program lifetime & computations,
subdividing however is convenient.

If your data does fit in, say /dev/shm on Unix, then you can use the filesystem
as "new calling convention".  The cost to rebuild memory maps in some other
process is often quite low (order 1 Python funcall) compared to whatever the
desired calculation.  There is no burden of immediate control flow transfer,
though.  You can re-open the files whenever is convenient.

#### 1a: Why not bio for binary IO?

To avoid confusion with block/buffered IO which is "similar but different" in
the same context and focus on the main points of nativeness/number-hood.  Also,
"BIO" would introduce at least one pronunciation alternative that sounds like
"B.O." which in English speaking cultures abbreviates body odor and we try to
avoid "name smell" as well as "code smell". Lolz. ;-)

### 2: How can it be "programming language agnostic" if it uses C type names?

Dependence upon C is only mnemonic.  C's ubiquity at the system level means
most/almost all prog.langs have exposure to C, e.g. for foreign function
interfaces.  A great many programmers who would never call themselves "C
programmers" nevertheless find the suffix syntax easy to remember.  I have
accessed NIO files from C, C++, Python, and Nim.  The format is all the same.

### 3: NIO isn't CPU architecture neutral?  Aaaawhaaaaa?

Architecture neutrality was always over-rated for files for data analysis or
programmatic interaction, the main use case of NIO.  Neutrality has become ever
less needed since the early 2000s as the Intel-ARM hegemony has grown ever more
dominant.  To preserve mmap&go capability, one needs at least 2 copies, possibly
made with some new `nio bswap` tool.  `dd swab` may be enough for simple row
formats.  It is possible to have the nio.(read|write) interfaces do needed byte
swaps, at some IO performance hit.  I haven't personally needed this capability
in decades using these ideas, though I am not averse to some PR for it.

While rare today, if you really have a cluster of heterogeneously endian
machines all computing against shared data ***and*** cannot store 2 copies then
NIO may not add much value over other approaches since it loses full optimizing
compiler mediated access, though it may still be "simpler".  Those are both
"big ands", though (meaning "rare in practice") esp. in the 2020s & beyond.

### 4: Why is the type syntax so darn terse?  Why no file headers?

People use terse codes for outputs (like printf) all the time.  Why not for
inputs?  The input side is much simpler.  There is no base-10/16/.. variation,
no left/right adjustment or number of decimals or alternate formats.

One of the draws of the so-called Unix Philosophy is a simple consistent
newline-delimited row format, but text is particularly inefficient for numbers
(possibly << 1 GB/s vs >> 100 GB/s).  One way to view NIO is the simplest
possible generalization of this "simple, consistent format" to binary numeric
formats.  To me, simplest implies no headers since there are various ways you
can get ahold of an IO buffer, but this is admittedly somewhat subjective.

A terse syntax that does not require shell quoting is helpful when used in shell
pipelines prototyped interactively.  The utility of such pipelines also makes
users confront & thus quickly learn/memorize row format syntax.  Row format
transformations (such as combining columns) render explicitly.  Explicitness has
pros as well as cons & we won't settle that debate here.

And, yes, every pipeline command tool could replicate row format headers at the
beginning of any sliced stream, say.  To me this reaffirms the extra complexity
of that IO protocol.  Why combine and then force re-separation when things can
stay separated at the outset (either in 2 files or via traditional filename
extension syntax -- maybe the most well known "syntax" in all computerdom)?

### 5: What about case-insensitive file systems?

I never use these myself and fail to see the appeal, but there are (at least)
two kinds of case-insensitivity: A) fully forgetful and B) store/present with
case variation but match insensitively.  Since it is already a type violation to
alter a row format after creation, and since users present pathnames often
generated by case-preserving operations, case B may cause little trouble.  For
case A) (and any case B gotcha/aesthetics), when there is no .N filename
extension, NIO tools look for "dot files" of the same name but with a leading
'.'.  These can contain a string just like the filename extension.  For example,
"dateFoo" might have ".dateFoo" with contents "if@dates%s%.3f".  So users can
just say `nio p dateFoo` or otherwise `nOpen("dateFoo")`.  Note the text after
`[@%]` is really only special `nio print` syntax and ignored in other contexts.
The file could also contain just "if" for most purposes.

### 6: What do you mean "NIO formalizes/generalizes existing practice"?

Unix /var/run/utmp & /var/log/wtmp have had this format for decades.  This sort
of works as a poor man's utmpdump/last:
```
ln -s (/var/run/utmp|/var/log/wtmp) \
  typePidLineIdUserHostExitSessTvIp6rsv.Nsi32C4C32C256C2sl2l4i10C
nio p typePid*
# or on BSD approx typePadTvIdPadPidUserLineHostRsv.Ns6c2l8C4ci32C16C128C64C
```
Python's NumPy has had a `save` method to do this from its beginning..just
without a corresponding "reshape" metadata on load or mmap.  (That came later in
the form of an optional file header, but see
[Q4](#4-why-is-the-type-syntax-so-darn-terse--why-no-file-headers) .)

NYSE tick-and-quote (TAQ) data also used to ship .BIN files as back-to-back
structs.  I believe the 2010s saw option exchange data feeds move to this.
Similarly, a 128xM 24-bit/3-byte color image raster file can literally just be a
binary file "foo.N128,3c".  Standard numerical code using this access library
could then just access the raster or similar variants directly.

So, the general notion at play here is in use and has been for decades, but its
use is clunky/ad hoc requiring C FFIs/.h files, Perl/Python "pack" modules.  One
standard, such as the NIO suffix format, suffices to write *general* tools that
can handle any layout, transformation, multiple OSes, etc. as well as avoiding
mucking about with `hexdump`, `od`, etc. leveraging described data semantics.

### 7: Why not Pandas/R dataframes/Excel/JSON/XML/etc.?

The culture in this space is not to run directly off stored files, but to reload
or re-parse constantly.  This is a fundamentally expensive way to go and only
works well for small data.  They often are like re-making an FS in program
memory.  Why not just use the existing FS?  Why not fully "save the answer" so
almost no re-doing of work is needed?  Data analysis is usually not like "emacs"
where you can have a very long running program loaded up in a costly way once.
Imposing such restrictions to acquire performance seems quite unnatural.  As
explained in the README these costs can be factors of 100s..1000s, even 10s of
thousands.  Such ratios may well be the difference between "fitting" on one fast
server/laptop vs. needing a huge cluster/distributed compute framework.

### 8: Why not a relational database like SQLite/MySql/etc.?

NIO is for use by programmer data analysts or more advanced programmers whose
domain knowledge lets them IO optimize better than query analyzers or who write
custom analytics or have other needs to integrate with "real PL" libraries.
This is all too painful in SQL FFIs/stored procedures (which are usually non-DB
portable).  Last I checked, it was a trick to even get Gaussian deviates out of
PostGres.  Trying to build a machine learning algo like this sounds like a
nightmare or at least a major project requiring more work than NIO.  Beyond this
the SQL standard is labyrinthine.  Another answer is "Because no one knows all
of/fully implements all SQL anyway".  That limitation leads to bad assumptions,
portability problems, vendor lock in, and all sorts of other things avoidable
with a simple file format.

Repayment for low-levelness is true zero overhead IO & access to SIMD speed.
Updates are also often rare-to-never; Yet analyses can hit large data sets 100s
if not millions of times.  So, vectors/tables/tensors are apt while ACID is just
over-engineered.  Any cost is waste.  There may be a way to get mostly what you
want IO-wise from LMDB but specifying data substructure will need something like
NIO anyway.  In short, there seems real value to non-DB persistence formats.
The closest analogue to envisioned NIO use cases is HDF5.

### 9: Ok..Why not HDF5?

Files & directories are a done deal.  HDF5 heralds from NetCDF & earlier formats
designed to work with very limited OS FSes of the 1970s & 1980s..E.g. DOS 8.3
filenames/VMS limits.  Consequently, these formats re-create/duplicate archive
functions (like tar/cpio/zip) instead of just using the FS.  Using regular files
has many charms, not the least of which is that users all already know how to
list, find & manage files & directories within the already hierarchical FS.
This includes many & varied compression programs - not only library access to
codecs but programs like zstd xyz or lz4 pdq etc.  Functionality bundles *seem*
nice, but can also be a limiting luxury trap, e.g. to support compression libs,
ACLs, rsync optimization, encryption, or all other things provided for files &
dirs.  NIO separates structure from storage.  Such independence is usually good.

### 10: Don't KDB/APL derivative xyz/Tea Files do this already?

Somewhat, but not fully.  For example, back around the turn of the 2010 decade
one could use NIO with `pixz` to get multi-GB/s scale IO from multi-threaded
decompression via backing store IO >20x less.  kdb had nothing comparable and
its over-bundled design made users need to wait.

[Tea Files](http://discretelogics.com/teafiles/) are quite similar in spirit,
but not tensor-rank general and oriented too specifically against time as a
primary table key.  Similar comments apply to other efforts like Apache
Parquet&Arrow.

As far as I can tell, NIO is alone in striving for a flexible column/vector
|matrix|tensor "store" that strives to just solve **just one simple problem**:
running "live" off pre-parsed data, but solve that problem as generally as is
easy in a programming language-neutral way.  As said elsewhere, it's mostly just
`numpy.save` format with an external header (but, at least for me, it pre-dates
things like numpy/pytables by years and they punt on PL-neutrality - except by
using the simplest possible format).

### 11: Ok..Why not a full object graph?

This could be a good addition.  Generalizing how string repositories work to
allow more arbitrary pointers may not even be hard.  Always insert-at-end/mark
deleted with some kind of eventual GC may retain a mostly usable "run right off
of files" for broader use cases, but notably will need to mimic GC'd types like
`seq` in Nim and possibly a lot of GC machinery.  PRs like this are welcome, but
note that relDBs/HDF5/etc. have somehow been useful for near a half century
without this feature and that object-relational mappings are usually considered
thorny.

Another aspect is that simpler structures tend to encourage discipline rewarded
via IO & calculational performance.  Mapping conceptual entities to a few file
types (as in [my suggest](https://github.com/c-blake/suggest) spellcheck DB) can
also provide a nice organization.  One file can be nice, but a few can also be a
nice organization and maybe all you need.

### 12: Why no bit fields/discriminated unions/even fancier things?

This is a good question.  Choosing the best least common denominator is hard.
".N3:i5:i" instead of ".NC" with some prohibition on prefix multipliers might
work as a syntax for bit fields.  Discriminated unions (Nim's "variants")
might be .N?C10C for a 10Byte union.  One problem is that it is much less
obvious what zip, rip, cut, and similar transforms mean in the presence of
either.  Obviousness is good, as are general tools.

Another problem is PL portability.  While available in C/C++/Nim/.., they may
well be unavailable in NumPy/Julia/R/.. This fanciness just slightly exceeds
the floor of what many PLs consider necessary.  If you are willing to give up
the general tools & portability then you may be able to retain other nice
aspects of NIO & handle these with a native typedef/object and some kind of
sizeof, losing only data file portability to other PLs (or maybe to other
compilers within the same PL).  At this point the value of language-external
format collapses.

{ `R` does not even do `float32` - to use NIO, you need `double` everywhere.
Similar comments apply to `unsigned`.  So, some might say the type system is
already "a half-step too fancy", but no `float` is a rare choice (one that has
aged poorly with SIMD popularity) and support for `unsigned` is easy to both
do & understand. }

In any event, these cases can also be handled by the current NIO format but with
higher-level interpretation, as with strings.  Sadly, the interpretation is
fancier than "pointers are repo indices".  (Well, with discriminated unions you
might just have however many files with the concrete types.)

### 13: What about filename limits, like 255 chars?

If you are packing that many fields into single rows then you (or some upstream
dependency you have) are likely on the wrong track, if for no other reason than
IO bw.  It is *very* unlikely you want all those fields in every table scan.  In
any event, you can still use dot files for the type information to get somewhat
more space in the filename and there is also no requirement to name every field.
If you have enough names to blow out your filesystem limit then an abbreviation
syntax like `yadda1-7,dates1-7,..` is more user-friendly anyway.

### 14: Why don't you just ALWAYS do column IO?

Column stores became all the rage in the 2010s and it's true in 2002 when I
began doing things like this they had charm (and still do) for some situations.
That said, sometimes you always want pairs/triples or in general tuples that do
not data compress much more as a tuple than they do in columns.  Or you may not
be compressing at all.  Spreading your IO requests over 4 files could have
Winchester disk seek risk or other inefficiencies.  "To zip or rip?" ultimately
depends on hardware deployment & data context.  Since there is no way to always
know such answers at abstraction-creation-time, it is better not to decide ahead
of time.

Various zips of files like this would be called "materialized views" in the
database world.  As with almost everything in NIO, they are "available but
manual" since we assume users can code & reason about their data processing/
analysis needs "when it matters" at large scales.  At large scales things can
take hours, days, or weeks and factors of 2-10x can make enormous usability
differences.  So, "no compromises" data access can be critical.

### 15: Why don't you just always do pure tensor IO like x.N10,10f?

This special case, like column IO, can be exactly what you want sometimes.
Other times it can be helpful to zip tensors with identifying tags or other
metadata..perhaps only transiently, but transiently is "enough" to need support
in the format.

### 16: Isn't the "type system" barely worthy of the name?

Yes & no.  It's basically the CPU type system (sans less portable latterday SIMD
types) rather than a more sophisticated programming language type system.  How
much of a problem this is depends upon what you are doing.  The main use case
for NIO is when performance matters which means big data which almost always
means big loops with low complexity data.  Low complexity data is not usually
too hamstrung by weak types.

Also, adding a type tag is not a crazy application of the above question's zip.
If you want then you can pair up everything .NCCff for a pair of distinct floats
with, say, units of measure encoded as the short integers.  You just need a
higher level of the system to interpret or enforce the types.  (Yes, it might be
more efficient to add this extra metadata just once not for each record..maybe
as a paired .Txyz file or as another row in the dotfile.)

### 17: Why so many string repository styles?

Because no one can really agree on what is convenient and text varies so much.
Length-prefixed is the most general autonomous 8-bit clean format, but is harder
to edit by hand.  Delimited is nice, but not 8-bit clean.  Fixed width is 8-bit
clean and even affords smaller integer row number indices, but then is fixed
width, meaning it has to truncate and takes up a lot of space.

There are actually (at least) two more unrepresented useful styles both of which
are welcome additions as PRs/etc.  First is newline-delimited but line number-
indexed (rather than byte offset-indexed).  With this, you can just fire up a
text editor & hack away on string defs with no regard to keeping string lengths
the same.  The downside is that, at load time, you must parse newlines which can
be slow for large repositories.  (Yes, an off-to-the-side updated-just-once
line-start cache can soften this performance blow.)  Second is back-to-back
undelimited string data with external length, index data (like "hihothere").
This is as fast & general as length-prefixed, but is also non-autonomous -
external data is needed to identify string boundaries.

### 18: Why no variable width inline strings like protobufs/flatbuffers/etc.?

NIO embraces files & directories (and their context) rather than trying to make
"autonomous buffers".  Fixed size records allow easy random access.  Fast subset
access is viewed as important..say take the last 1% of rows or random samples.
To preserve this with VW inline strings, you need (at least) a separate global
index giving offsets of records as well as a (probably) in-record index giving
starts of fields (at least after the first inline string).  While the latter
could precede each row, that'd be somewhat expensive in space (possibly doubling
use if average pointer size =~ average datum size).

If one shares one string repo across many fields then one can compare for
equality by address, sometimes called string interning.  This optimization falls
naturally out of the repository/"multi buffer" arrangement.  The indirection is
potentially costly, but for many analyses one would often prefer class numbers
anyway (another way to think of interning is like an "automatic enum") for
intermediate calculation, stringifying only at the end.  So, the indirection is
helpful for many use cases, especially with indirect fixed-width strings where
it is cheap to make the address a row number that can be thought of like a file
descriptor or other "dense, only as big as needed" label.

If one can ensure sort order on the repo all in-set string compares (not just
equality) can even be reduced to integer compares.  That is rather costly
requiring either another level of indirection (say via a B-tree which itself
might have a level or three) or a batch sort with a re-org of all the pointers
which is only practical for very static data.  Since NIO is not a string-focused
facility, this is unlikely to be popular feature anyway.

### 19: What about "schema versioning"/evolution/mutation?

Since files are autonomous, you can usually just add new columns as new files
with compatible indexing.  This gives you schema addition with no need to even
version.  fileExists API calls give a sort of "duck type" check of what is
available.  This is especially easy if you are doing a pure-column files setup,
but is also fine with various tables.  *Deleting* columns within zipped tables
is not easy, but it is always hard to do that with code that may depend on
structure.

Semantics often not fully covered by types can also bite, e.g. a differential
field becoming a cumulative field.  At some level, you must understand data that
you calculate against.  In any case, you can always just drop a `.VERSION` file
in the directory if you think some string can capture the needed labeling.

### 20: This is all hopelessly hard to use compared to SQL

Also not a question.  But is it, though?  The SQL standard is as big or bigger
than that of C++.  Many people know trivial basics yet almost nothing of the
full feature set or even its scope.  `nio qry` is not so different from such
trivial basics.  I do think reasonable folks can differ on "hard" (it often
comes down to background knowledge which varies tremendously) and I am open to
usability suggestions coherent with the overall design.

The core idea is kind of "between" the IO parts of DBs and access/query parts.
So, you could think of it as a way to layer building a DB in such a way that
preserves "no compromise IO" by programmers willing to put in some effort..An
"open architecture database", if you will.  E.g., query language functionality
can be layered on top, and transactional ideas could be stuffed underneath,
maybe optionally to preserve efficiency.  NIO is just a supplementary point in
the design space, arguably at its simplest crux, rather than an outright
replacement.  "Not appealing to all in all cases" is just another way of saying
"Yup.  It's software." ;)

In any case, my ideas here have received conservatively under 0.0001% of the
man hours, monetary support, advertising, and usability work SQL has received.
So, you know, maybe they compete pretty favorably considering this? ;)

### 21: Ok.  I am sold, BUT what about MY programming language?

Hey..Glad you like the idea (and even read to the end).  There are only so many
hours in the day, though.  My hope is that the core idea is simple enough to be
replicated in any PL with any kind of low-level IO.  The core filename extension
parser, `nio.initIORow`, is like 25 lines of non-comment Nim.

The missing value|N/A convention is perhaps less critical in a first pass.  A
full suite of tools like loaders/parsers/printers/zip/rip is not necessary *if*
you are merely willing to depend upon the Nim `nio` CLI tool.

Once you have a basic access library, you could write n-foo CL tools OR libs in
a dozen PLs if you want..n-awk, n-plot, n-histo, `import nregress`, etc.  As
already mentioned, the main use case is custom calculations over big data where
all you really need is the row stream/mmap interfaces and your own code which is
often very special purpose.  OTOH, maybe there are many general things to do.
Have at it!

NIO is so simple/easy that its main value to the world might not be *any* one
implementation, tool, or library, but merely a standard suffix syntax/naming
convention as a rendezvous point.  With one convention, the world benefits from
all impls in all PLs, like CSV or TSV or whatever - but with less parsing.

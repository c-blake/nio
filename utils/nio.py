"""Module to make accessing NIO files as easy as referring to Python vars.
Eg., assuming a NIO repo with a "ret" matrix, a max cross sectional return
for 2007/01/03 is: print RO.ret[RO.Tx["20070103"]].max()."""

import sys, os, numpy as np, glob, stat, re; E = os.environ.get
try   : import npy.mmap as mmap
except: import mmap
ERR = sys.stderr.write

########## File Suffix Convention ##########

dType = {                                               # NioCode -> dType
  'c':np.int8 , 'C':np.uint8 , 's':np.int16, 'S':np.uint16,
  'i':np.int32, 'I':np.uint32, 'l':np.int64, 'L':np.uint64,
  'f':np.float32, 'd':np.float64, 'g':np.longdouble }
Code  = dict( (v,k) for (k,v) in dType.items() )        # inverse: dType->NCode

def Ncode(x, ncode=None):
    if ncode is not None: return ncode
    return Code[x.raw.dtype.type] if hasattr(x, "raw") else Code[x.dtype.type]

codes = 'cCsSiIlLfdgp'                                  # dType.keys() as str
Dtype = { 'c':'i1', 'C':'u1', 's':'i2', 'S':'u2', 'i':'i4', 'I':'u4', # code->
          'l':'u8', 'L':'i8', 'f':'f4', 'd':'f8', 'g':'fL'}           #..typStr

########## Missing Value Convention ##########

na = { 'c': -2**7, 'C': 2**8-1, 's': -2**15, 'S': 2**16-1, 'i': -2**31,
       'I': 4294967295, 'l': -9223372036854775808, 'L': 18446744073709551615,
       'f':np.nan, 'd':np.nan, 'g':np.nan }

def isna(x, ncode=None):
    "Return boolean NumPy array true where equal to nio NA values."
    if type(x) == str: return len(x) == 0   # "" === NA for strings
#   if isinstance(x, Indirect): return x.raw == 0
    c = Ncode(x, ncode)
    if c in ('f', 'd', 'D'): return x != x
    return x == na[c]

def isnotna(x, ncode=None):
    "Return boolean NumPy array true where not equal to nio NA values."
    if type(x) == str: return len(x) > 0
#   if isinstance(x, Indirect): return x.raw != 0
    c = Ncode(x, ncode)
    if c in ('f', 'd', 'D'): return x == x
    return x != na[c]

########## Suffix Utility Code ##########

def sfx_add(Path, M=None, code=None):
    "Return last NIO dirent given extensionless Path.  Maybe require M cols."
    if ".N" in Path: return Path
    if M and code: return "%s.N%d%s" % (Path, M, code)
    if M: paths = glob.glob("%s.N%d[%s]" % (Path, M, codes))
    else: paths = glob.glob("%s.N*[%s]"  % (Path, codes))
    paths.sort()
    if len(paths) > 1:
        ERR("nio.py:sfx_add multiple matches for %s: %s\n" % (Path, paths))
    return paths[-1]

def sfx(Path):
    "Find '.N' suffix in Path; Raise ValueError if no .N sfx."
    Path = Path.replace(".N,", ".N")        # Elide leading identity dimension
    return Path[Path.rindex(".N") + 2 : ]

def sfx_rm(Path):
    "Return Path with any '.N' suffix removed; Raise ValueError if no .N sfx."
    return Path[ : Path.rindex(".N") ]

def sfx_mk(x, ptr=False):
    "Generate an appropriate .N suffix for a NumPy array."
    if len(x.shape) > 3:
        raise Exception("only vectors, matrices, and 3-tensors supported")
    if len(x.shape) > 2:
        nT, n, m = x.shape
        return ".N%d,%d%s" % (n, m, Ncode(x, ptr=ptr))
    if len(x.shape) > 1:
        n, m = x.shape
        return ".N" + str(m) + Ncode(x, ptr=ptr)
    return ".N" + Ncode(x, ptr=ptr)

########## IO Utility Code; Regular & MemMapped ##########

def save(Path, x, ptr=False):
    "Save NumPy array 'x' to path-prefix 'Path' with appropriate .N suffix."
    x.tofile((open(Path + sfx_mk(x, ptr), "wb+")))
    return x

def sfx2dtype(s):   #Convert <c1>b1<c2,c3,...>b2... to dtype (c1)b1,(c2,c3)b2
    "Convert a '.N' suffix to a Numpy dtype object describing rows."
    dt = ""
    for c in re.sub(r'([0-9,]+)', r'(\1)', s):      # parenthesize [dig,] seqs
        if c in codes: dt += Dtype[c] + ','         # translate codes & delimit
        else: dt += c                               # (trailing , killed below)
    dt = re.sub(r'\(([0-9]*)\)', r'\1', dt[:-1])    # (digs) => digs
    try   : return np.dtype(dt)                     # rowSz==dtyp.itemsize
    except: ERR('problematic filename suffix: %s %s\n' % (s, dt)); raise

def mapA(Path, protCB, protSTD, reprs=None):
    "Internal call that sets things up for perms protCBmmap/protSTDmmap"
    try   : Path = sfx_add(Path)
    except: raise Exception('cannot find NIO file like "%s"' % Path)
    f  = open(Path, "r")
    fd = f.fileno()
    buf = mmap.Mmap(fd, protCB) if hasattr(mmap, "Mmap") \
          else mmap.mmap(fd, os.fstat(fd).st_size, access=protSTD)
    dtyp = sfx2dtype(sfx(Path))
    if len(buf) % dtyp.itemsize:
        raise Exception('non-integral number of records in "%s"' % Path)
    x = np.frombuffer(buf, dtype=dtyp)
    x.flags['WRITEABLE'] = ("w" in protCB)
    return x

def mapR(Path, reprs=None):
    "Map .N file into an appropriately shaped NumPy array: READ-ONLY."
    return mapA(Path, "r", mmap.ACCESS_READ, reprs)

def mapP(Path, reprs=None):
    "Map .N file into appropriately shaped NumPy array: RW_CPoWr-PRIVATE"
    return mapA(Path, "rwp", mmap.ACCESS_COPY, reprs)

def mapW(Path, n=None, M=None, code=None, fill=None, wipe=False, reprs=None):
    "Create or Map RW .N file as an appropriately shaped NumPy array."
    try   : Path = sfx_add(Path, M, code)
    except:
        if n: raise Exception('"%s": not found or missing .N sfx' % Path)
        else: raise Exception('"%s": not found' % Path)
    dtyp = sfx2dtype(sfx(Path))
    if n is None:
        n = os.stat(Path).st_size // dtyp.itemsize
    sz = n * dtyp.itemsize
    f  = open(Path, "a+")
    fd = f.fileno()
    if fill is not None:
        SZ = os.fstat(fd).st_size
    if hasattr(mmap, "Mmap"):
        buf = mmap.Mmap(fd, "rws", sz)
    else:
        os.ftruncate(fd, sz)
        buf = mmap.mmap(fd, sz, access=mmap.ACCESS_WRITE)
    x = np.frombuffer(buf, dtype=dtyp)
    if fill is not None:                                # Need to fill..
        if fill == na: fill = na[Path[-1]]
        if wipe: SZ = 0                                 # fill WHOLE file
        if sz > SZ:
            x[SZ // x.strides[0] : ] = fill             # blast values in
    return x

_isDir = {}
def isDir(path):                            # Few syscalls dir-hood test
    try: return _isDir[path]
    except:
        try   : r = _isDir[path] = stat.S_ISDIR(os.stat(path).st_mode)
        except: r = _isDir[path] = False
        return r

def classify_dirents(path):
    "Return (NIOfileStubs, directories, other) lists for directory 'path'."
    Nstubs, dirs, other = [], [], []
    path = path if path.endswith("/") else path + "/"
    for ent in os.listdir(path):
        nsfx = ent.rfind(".N")
        if   nsfx != -1       : Nstubs.append(ent[ : nsfx]) # NIO File Stub
        elif isDir(path + ent): dirs.append(ent)            # Subdir
        else                  : other.append(ent)           # Other
    return Nstubs, dirs, other

############ Index Descriptor/Representation Helper Functions ############

def Str(charArray):
    "Convert array of char w/NUL pad to a python string, stripping NUL bytes"
    return charArray.tobytes().decode(encoding="ascii", errors="ignore").strip("\000")

def CharArr(s, m=None):                     # mapP/W returns byte not string
    "Convert a Python string to a NumPy char array of size m (len + 1)."
    if m is None: m = len(s) + 1
    return np.frombuffer(s[ : m - 1].ljust(m, '\000'), dtype=np.byte)

def mkId2ix(x):
    "Build the reverse mapping from row contents to offset index."
    if len(x.shape) == 2 and Ncode(x) in ('c', 'C'):
        return dict( (Str(row), i) for (i, row) in enumerate(x) )
    elif len(x.shape) == 1:
        return dict( (row, i) for (i, row) in enumerate(x) )
    else: raise Exception('can only have string or scalar column indices')

def mapIx(Path):
    'Usage: ix2foo, foo2ix = mapIx("ix2foo")'
    x = mapP(Path)
    if len(x.shape) == 2 and Ncode(x) in ('c', 'C'):
        return [ Str(e) for e in x ], mkId2ix(x)
    else: return x, mkId2ix(x)

def Id(t, i, id, _id, lookback=2, level=0):
    "Resolve a matrix index to a Python string via id[_id]"
    if level < lookback:
        try:    return Str(id[_id[t - level, i]])
        except: return Id(id, _id, t, i, level + 1, lookback)
    else:
        return "(%d)" % i

def ixCompl(ixs, m):
    "Compute complement index set to 'ixs' for an m-vector. XXX Generalize"
    complement = np.zeros(m, dtype=np.int32)
    complement[ixs] = -1
    return np.where(complement != -1)

Root = "."
class Loader:                                 ### Helper Class For Auto-Loading
    """On-demand getattr-to-mapped files. Ob.attr yields mapP("attr") if attr
    is a regular file or if a directory a new Loader bound to that dir,
    effectively mirroring an FS hierarchy with nested Python namespaces.
    This module provides convenience instances, RP, RO, RW."""

    def __init__(o, root=None, load=mapR):
        o._cache = {}
        o._load  = load
        if root is None: root = Root
        o._root  = root if root.endswith("/") else root + "/"
        try:
            o.Tm = [ Str(e) for e in o.ix2tm ]
            o.Tx = mkId2ix(o.ix2tm)
            o.Ix = mkId2ix(o.ix2id)
            o.Id = [ Str(e) for e in o.ix2id ]
        except Exception as e:
            ERR("Initializing Loader(%s): %s\n" % (o._root, e))

    def __getattr__(o, nm):
        try   : return o._cache[nm]
        except:
            if nm in { "Tm", "Tx", "Ix", "Id" }:
                rval = o.__dict__[nm]
            else:
                qual = o._root + nm
                rval = Loader(qual, o._load) if isDir(qual) else o._load(qual)
            o._cache[nm] = rval
            return rval

    def __dir__(o):                                 # For TAB auto-complete,etc.
        try:
            Nfiles, dirs, other = classify_dirents(o._root)
            return Nfiles + dirs
        except OSError as ex:                       # Report likely perm issue
            ERR("%s: %s\n" % (o._root, ex.args[1]))

def init(root=None):
    'Change default root from ".", Build "RO" Loader'
    global Root, RO
    Root = root if root else E("DPATH", ".")
    RO = Loader()                                   # READ-ONLY Loader

########## NumPy Utility Functions ##########

np.seterr(all='ignore')                              # Tweak numpy options
np.set_printoptions(threshold=10000000, precision=int(E("NIO_DIG", "7")))

def AND(*conds):
    "Smarter logical_and(); And's all boolean vectors in its varargs."
    if len(conds) <= 2: return np.logical_and(*conds)
    else:               return np.logical_and(conds[0], AND(*(conds[1:])))

def OR(*conds):
    "Smarter logical_or(); Or's all boolean vectors in its varargs."
    if len(conds) <= 2: return np.logical_or(*conds)
    else:               return np.logical_or(conds[0], OR(*(conds[1:])))

NOT = np.logical_not

def everIx(U, t0 = 0, t1 = -1):
    "Compute index array of subset of ids ever in U over times t0 <= t < t1"
    U = U.copy()
    U[isna(U)] = 0                          # NA -> 0
    nT, nI = U.shape
    col_sum = np.zeros(nI, dtype=np.int32)
    if t1 != -1:
        nT = t1
    for t in range(t0, nT):
        col_sum += U[t]
    return np.where(col_sum)[0], nI          # basic index selector, its span

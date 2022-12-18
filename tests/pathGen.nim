import nio

type
  Foo = object
    a, b: int32
  Bar = object
    c, d: float32
    e: Foo
  Baz = array[3, Bar]
echo typedPath[Foo]("")
echo typedPath[Bar]("")
echo typedPath[array[2, char]]("")
echo typedPath[array[3, array[2, float]]]("")
echo typedPath[array[3, array[2, array[3, int16]]]]("")
# These break and error out; Would need a () syntax for .N
try:
  echo typedPath[array[3, array[2, Bar]]]("")
except CatchableError:
  echo "exception"
try:
  echo typedPath[Baz]("")
except CatchableError:
  echo "exception"

type
  FooT = tuple[a, b: int32]
  BarT = tuple[c, d: float32; e: FooT]
  BazT = array[3, BarT]
echo typedPath[int]("")
echo typedPath[FooT]("")
echo typedPath[BarT]("")
# These break and should at least error out
try:
  echo typedPath[array[3, array[2, BarT]]]("")
except CatchableError:
  echo "exception"
try:
  echo typedPath[BazT]("")
except CatchableError:
  echo "exception"

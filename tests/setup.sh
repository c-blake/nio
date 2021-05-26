# This is a library intended to be invoked as ". ${0%script}cd.sh".
# It sets up $d and cd's into a data directory or dies trying.
: ${d:="/dev/shm/d"} # or maybe /dev/shm/data or something
: ${nio:="nio"}      # Use installed `nio`; Could be, e.g. "nim r path/nio.nim"
if [ -r "$d" -a -w "$d" -a -x "$d" -a -d "$d" ]
then
    cd "$d" || exit 1
else
    mkdir "$d"
    cd "$d" || exit 2
fi

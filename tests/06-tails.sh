#!/bin/sh -e
. ${0%06-tails.sh}setup.sh
t=09
seq 0 9 | nio l -id -oi > $t.Ni
ns="0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 6 17 18 19"

# first test mmap/rand access impls
(for extra in "" "-c" "-r" "-rc"; do
  for n in $ns; do
    for m in $ns; do
      echo x=$extra h=$m t=$n
      nio t $extra -h$m -t$n $t.Ni | nio p .Ni
    done
  done
done) > tailsMap

# then streaming impls
(for extra in "" "-c" "-r" "-rc"; do
  for n in $ns; do
    for m in $ns; do
      echo x=$extra h=$m t=$n
      cat $t.Ni | nio t $extra -h$m -t$n .Ni | nio p .Ni
    done
  done
done) > tailsStm

# Consider the impls of mmap/one-pass streaming distinct enough to be a
# good cross-check.
diff -uw tailsMap tailsStm

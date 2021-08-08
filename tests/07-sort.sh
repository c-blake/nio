#!/bin/sh -ex
. ${0%07-sort.sh}setup.sh

# Here we want to test multi-level sorting.  So we need a few more rows with
# some rows equal in all but 1, etc.
set +x
printf ghidefabcghiabcjkl > emb3.N3C
echo 17 0 13 17 13 0 | tr ' ' '\n' | nio l -id -oi > indir.Ni
echo 2  4 3  5  3  1 | tr ' ' '\n' | nio l -id -oi > nums1.Ni
echo 5  3 1  2  4  6 | tr ' ' '\n' | nio l -id -oi > nums2.Ni

nio z indir.Ni nums1.Ni emb3.N3C nums2.Ni > i1e2.Nii3Ci
set -x
nio p i1e2.Nii3Ci
nio o -o Ord1 indir.Ni nums1.Ni emb3.N3C nums2.Ni
nio p Ord1.Nl
nio o -o Ord2 emb3.N3C indir.Ni nums1.Ni nums2.Ni:-1
nio p Ord2.Nl
nio o -o Ord3 emb3.N3C indir.Ni:@strs.LS+1 nums1.Ni nums2.Ni
nio p Ord3.Nl
nio o -o Ord4 i1e2.Nii3Ci
nio o -o Ord5 i1e2.Nii3Ci:3,1,2,-4
nio o -o Ord6 i1e2.Nii3Ci:3,@strs.LS+1,2,4
cmp Ord1.Nl Ord4.Nl || echo "order 1,4 MISMATCH"
cmp Ord2.Nl Ord5.Nl || echo "order 2,5 MISMATCH"
cmp Ord3.Nl Ord6.Nl || echo "order 3,6 MISMATCH"

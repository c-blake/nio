#!/bin/sh -ex
. ${0%2-strings.sh}setup.sh

(echo hi
 echo neighborino
 echo MoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256charsMoreThan256chars
) > strings

nio l -ix -oi < strings -x@strs > strs.Ni
nio p strs.Ni@strs%s

nio l -ix -oi < strings -x@strs.Dn > strsDn.Ni
nio p strsDn.Ni@strs.Dn%s

nio l -ix -oi < strings -x@strs.D0 > strsD0.Ni
nio p strsD0.Ni@strs.D0%s

nio l -ix -oi < strings -x@strs.LC > strsLC.Ni
nio p strsLC.Ni@strs.LC%s

nio l -ix -oi < strings -x@strs.LS > strsLS.Ni
nio p strsLS.Ni@strs.LS%s

nio l -ix -oi < strings -x@strs.N6C > strs6C.Ni
nio p strs6C.Ni@strs.N6C%s

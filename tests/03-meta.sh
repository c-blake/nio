#!/bin/sh -ex
. ${0%03-meta.sh}setup.sh

nio st .Nifd9c
nio st -c%z .Nifd9c
echo # last should have had no newline

nio def .Nifd9c

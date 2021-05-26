#!/bin/sh -ex
. ${0%3-meta.sh}setup.sh

nio me .Nifd9c
nio me -c%z .Nifd9c
echo # last should have had no newline

nio def .Nifd9c

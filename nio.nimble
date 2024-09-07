# Package
version     = "0.7.2"
author      = "Charles Blake"
description = "Low Overhead Numerical/Native IO library & tools"
license     = "MIT/ISC"

# Deps
requires    "nim >= 1.6.12"
requires    "cligen >= 1.7.4"

bin         = @[
  "nio",              # A big binary file manipulating multi-command
  "utils/addEmpty",   # Add an empty column at the end of each row.
  "utils/c2tsv",      # escape-quoted CSV => TSV via a custom state machine
  "utils/c2tsvs",     # escape-quoted CSV => TSV via `import parsecsv`
  "utils/lab2sv",     # labeled/folded => separated-values (hash-based)
  "utils/labs2sv",    # labeled/folded => separated-values (sort-based)
  "utils/lp2term",    # length-prefix => terminated
  "utils/nmatch",     # print out lines prefixed by number of matches
  "utils/transpose",  # transpose a text matrix
  "utils/nsv",        # number of entries in SV data
]

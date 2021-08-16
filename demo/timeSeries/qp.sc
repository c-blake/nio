# This is a parsing schema for nio fromSV.
--nHeader=1                      # number of rows which are headers
--delim=,                        # NUL is c2tsv output delimiter
#name NC SC TRANSFORM:args       # NC=NIOcode;SC=(scan|src)Code like scan1
sym	8C	c	# sym must be an embedded string
trdTm	S	x @$REPS/trTm.N8C # 'S' just for type diversity; 65536 too small
last	f	f	# last trade px corresponding to TIME
dchg	f	f	# day change from prior close
pctchg	f	f	# percentage change
close	f	f	# day close
svlm	i	x @$REPS/svlm.N8C # converted to humanRd, e.g. 30G shr
open	f	f	# day open
low	f	f	# day low
high	f	f	# day high

import nio
nio.init()
v = nio.RO
print(v.idTm.last[v.Ix["SPY"]])

# TAB-completion even works:
# $ PYTHONPATH=../../utils py3
# [0]>>> import nio; nio.init()
# [0]>>> v = nio.RO
# [0]>>> v.idTm.l<TAB>
# v.idTm.last  v.idTm.low

# Companion command script for gdb_inlined.rr — see the RUN lines there.
# Line 24 is the `let next` line of the inlined `sum`; the test's first check
# fails if the source moves it.
break gdb_inlined.rr:24
run
bt
print acc
continue
print acc

       >>SOURCE FORMAT FREE
identification division.
program-id. HELLOWORLD.

environment division.

data division.
working-storage section.
01 xmtr-power      pic 9(4)      value 1000.
01 feedline-length pic 9(2)      value 73.
01 duty-cycle      pic 9(1).9(2) value .5.
01 per-30          pic 9(1).9(2) value .5.
01 cablevalues.
       02 k1 pic 9(1).9(6) value .122290.
       02 k2 pic 9(1).9(6) value .000260.

procedure division.
display duty-cycle.
stop run.

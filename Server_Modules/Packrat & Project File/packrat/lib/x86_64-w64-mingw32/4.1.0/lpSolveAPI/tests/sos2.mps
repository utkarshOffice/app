ROWS
 L  c1
 L  c2
 N  COST
COLUMNS
    x1        c1                 -1.   c2                  1.
    x1        COST               -1.
    x2        c1                 -1.   COST               -1.
    x3        c1                  1.   c2                  1.
    x3        COST               -3.
    x4        c1                  1.   c2                 -3.
    x4        COST               -2.
    x5        COST               -2.
RHS
    RHS       c1                 30.   c2                 30.
BOUNDS
 UP COLBND    x1                 40.
 UP COLBND    x2                  1.
 UP COLBND    x5                  1.
SOS
 S2 SOS       SOS                 1.
    SOS       x1                  1.
    SOS       x2                  2.
    SOS       x3                  3.
    SOS       x4                  4.
    SOS       x5                  5.
ENDATA

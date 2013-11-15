#!/usr/bin/env python

import sys

from decimal import *
from math import *


# this mapper parses std input to get x and y coordinates.  it assigns these
# x and y coordinates to appropriate bins using the floor function
# Mapper prints to stdout a key and value as:  "x_lo,x_hi,y_lo,y_hi  1"

# Note:  Since the coordinates typically have long decimal expansions, the mapper uses the
# decimal datatype to avoid any truncations.
# this ensures that the comparisons done for the binning are accurate

# Note:  For some reason the interpreter tacks on a bunch of extra digits to 0.1
# when 0.1 is converted to the decimal datatype.  However, float(0.1) is interpreted
# as 0.1 with no extra digits.  Thus, when deciding the bin boundaries, all relevant numbers are converted to
# the float datatype


# since the AWS machines use python 2, we need a user-defined function to
# convert from float to decimal
def float_to_decimal(f):
    "Convert a floating point number to a Decimal with no loss of information"
    n, d = f.as_integer_ratio()
    numerator, denominator = Decimal(n), Decimal(d)
    ctx = Context(prec=60)
    result = ctx.divide(numerator, denominator)
    while ctx.flags[Inexact]:
        ctx.flags[Inexact] = False
        ctx.prec *= 2
        result = ctx.divide(numerator, denominator)
    return result



# input comes from STDIN (standard input)
for line in sys.stdin:

    # remove leading and trailing whitespace
    line = line.strip()

    # split the line into x and y coordinates
    coords = line.split()

    # extract the x and y coordinates
    x = Decimal(coords[0])
    y=  Decimal(coords[1])

    # truncating x and y coordinates to one decimal place
    truncx = float_to_decimal(floor(x*10))/Decimal(10)
    truncy = float_to_decimal(floor(y*10))/Decimal(10)

    # deciding bins for x and y coordinates
    if x > truncx:
        x_lo = truncx
        x_hi = float(truncx) + 0.1
    else:
        x_lo = float(truncx) - 0.1
        x_hi = float(truncx)

    if y > truncy:
        y_lo = float(truncy)
        y_hi = float(truncy) + 0.1
    else:
        y_lo = float(truncy) - 0.1
        y_hi = float(truncy)

    # tab-delimited; the trivial bin count is 1
    print "%s,%s,%s,%s\t%s" % (str(x_lo), str(x_hi), str(y_lo), str(y_hi), 1)












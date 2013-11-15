#!/usr/bin/env python


# We borrow with minor modifications the reducer used by Dr.Baines for his word count example
# in lecture.  Since the mapper's key is already in the prescribed format for the reducer's output,
# the reducer simply counts the number of occurrences of a particular key and then prints -
# "x_lo,x_hi,y_lo,y_hi,count"


from operator import itemgetter
import sys

current_bin = None
current_count = 0
bin = None

# input comes from STDIN
for line in sys.stdin:
        # remove leading and trailing whitespace
        line = line.strip()

        # parse the input we got from mapper.py
        bin, count = line.split('\t', 1)

        # convert count (currently a string) to int
        try:
                count = int(count)
        except ValueError:
                # count was not a number, so silently
                # ignore/discard this line
                continue

        # this IF-switch only works because Hadoop sorts map output
        # by key (here: bin) before it is passed to the reducer
        if current_bin == bin:
                current_count += count
        else:
                if current_bin:
                        # write result to STDOUT
                        print '%s,%s' % (current_bin, current_count)
                current_count = count
                current_bin = bin

# do not forget to output the last bin if needed!
if current_bin == bin:
        print '%s,%s' % (current_bin, current_count)


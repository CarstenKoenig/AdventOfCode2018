# Day 12

the puzzle for today can be found [here](https://adventofcode.com/2018/day/12).

first part is rather straight forward (I got into some trouble because I intially added only
2 pots left and right)


After some thinking I changed the naive list representation (that was inspired by the problem)
to one where the current state is represented as a set of all filled pots. This makes
the problem a lot easier.

second part needs some guessing - turns out after some point the difference in the sums remain
constant - the algorithm there looks for such a place and stops

I have no *proof* that this will work for each input but I have a hunch:

This resembles a cellular automaton which is more or less a finite-state machine, so after some
point we should see the same state again and get a cycle

what I don't see exactly right now is why this cycle is of length 1 here - but hey: it works for me ;)

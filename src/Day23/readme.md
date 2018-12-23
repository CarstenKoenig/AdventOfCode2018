# Day 23 - Experimental Emergency Teleportation

the puzzle for today can be found [here](https://adventofcode.com/2018/day/23).

Todays part 1 was quite quick.

Part 2 was a bit of a puzzle - I initially thought it would be enough to check all
the 8 cube-coords for each bot but that was wrong.

Instead I went with a incremental cube-division search (starting with a cube over all points)
and then slicing that into 8 sub-cubes and looking for the one which is in range on most
points until I end up with a cube of length 1.

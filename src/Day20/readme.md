# Day 20 - A Regular Map

the puzzle for today can be found [here](https://adventofcode.com/2018/day/20).

quite a nice little puzzle - I enjoyed this one.

It has two parts: it asks you to expand a RegEx to all possible strings (no Kleene-stars of course) which will
turn into a Grid/Map.

Part1 asks you to find the maximum number of doors (every second step is always a door) - always using the shortest
path - to a room.

Part2 asks you to count all rooms to which you have to take at least 1000 doors to get to.

After parsing the input and writing a simple recursive function to exanpd the grid this was easily done
by annotating the rooms (I annotate the doors too - easier) with the numbers of doors required to get there.
If you come to a room twice you just take the minimum of the steps already annotated and in your current path.

In general **this is not correct** but for my input it worked out ;)

---

TODO: for my input only one room is visited twice

obviously I should redo this with a valid algorithm - maybe after the holidays ;)

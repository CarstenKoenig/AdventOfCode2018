# Day 7

the puzzle for today can be found [here](https://adventofcode.com/2018/day/7).

todays puzzle was about topological sorting and working on a Graph
sadly `Data.Graph` was not using the right sort when more than one nodes
where having no incomming edge - so I had to trash that solution and
reimplement the damn thing

part 2 is just simulating workers - used a very simple scheme where I
step through time one second by the next (quick enough)

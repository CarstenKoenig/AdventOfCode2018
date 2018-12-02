# Day 2

the puzzle for today can be found [here](https://adventofcode.com/2018/day/2).

The puzzle asks us to find two specific containers with *similar* IDs. The ID is just a string of characters.

Part1 is a simple counting: count the IDs where a character occurs twice and again count where a character occurs three-times (meaning this one should not count for twice!). Multiply those two numbers.

I just sorted, grouped the characters and counted those with 2 or 3 blocks.

Part2 is now asking to find similar IDs where similar means that they only differ by one character (as proof we are asked to return the common characters).

I failed to see the direct connection between part 1 and 2 (maybe my idea for 1 was wrong) so I solved this one by just comparing pairwise
and finding the first match (which is a shameful $O(n^2)$ but seems no problem for the input size)

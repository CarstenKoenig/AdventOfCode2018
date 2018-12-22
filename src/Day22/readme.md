# Day 22 - Mode Maze

the puzzle for today can be found [here](https://adventofcode.com/2018/day/22).

Today was a bit more interesting - it's obvious from the start that we are going
to need some *dynamic programming* to calculate the `regionTypes`.

Aside from this it's a common A* problem and my algorithm was as good as the
alternatives I tried from `search-algorithms` and `astar` packages.

My biggest trouble was indeed **memoization** [Data.MemoTrie](https://www.stackage.org/haddock/lts-12.24/MemoTrie-0.6.9/Data-MemoTrie.html#v:memoFix)
worked well enough but the problem here is that it won't keep the table
in between calls to the resulting function (at least I was not able to convince GHC to keep it somehow).

So I ended up using a *lazy map* which is considerable slower for part 1 but ok for part 2.

Compared to yesterday this one was quite fun.

---

update: switched to `Array` which reduced the time a bit

runtime is around 3s which is shameful but enough for me right now.

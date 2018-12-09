# Day 9

the puzzle for today can be found [here](https://adventofcode.com/2018/day/9).

a game using a circular list (thankfully I found a good implementation [pointedlist](https://hackage.haskell.org/package/pointedlist-0.6.1/docs/Data-List-PointedList-Circular.html))

the problem is rather straight forward - sadly I totally messed up becaues I tried to answer with the `1` from `part 1` ... damn

after that part 2 was easy - it takes around 4s on my PC which is fine for me


---

## Update

after a bit of looking around I found a really nice double-ended-queue implementation [deque](https://hackage.haskell.org/package/deque-0.2.7/docs/Deque.html), which is a bit faster than the pointed-list from above

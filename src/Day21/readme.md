# Day 21

the puzzle for today can be found [here](https://adventofcode.com/2018/day/21).

today *again* was more a puzzle part with reading the stupid assembler code.

I was not able to find a pattern in the checks (for my input it was *Reg5*).

Initially I just changed Day 19 so I can look at the values in line 28 (which yield the first part at register 5)
and then added an `error` when the register value repeats. This ran for a *long* time (20minutes?) but gave me star 2 as well.

Of course that's not really useful so I translated the calcuation `step` for reg 5 into Haskell
and let it just run with `0` for **part 1**. For **part2** it just iterates and returns the last value once a reg 5
was seen twice.

If you want to use this for your input you have to look for the two `konstante` in your code - they should be
at *line 7* and *line 11* in your input.

If you are interested you can find my annotated diggings in the source code in
[notes.txt](./notes.txt).

I really hope this was the last one of those - they are just not fun for me :(

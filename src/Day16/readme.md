# Day 16

the puzzle for today can be found [here](https://adventofcode.com/2018/day/16).

Today was the long awaited "write some Assembler" with a twist:
Instead of just giving us the instruction set we are asked to find out how
*opcode-numbers* correspond to the described memonics first.

After we are asked to run a small little program.

---

luckily the most naive idea of just looking for single-matches (only one op-code matches
all examples for one number) and building a correspondence-table from that works just fine.

I guess we'll come back to this machine later (after all it's the time-machine)

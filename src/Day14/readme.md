# Day 14

the puzzle for today can be found [here](https://adventofcode.com/2018/day/14).

the problem today is not really hard but I did not find a good solution for Haskell yet.

Right now I'm using `Data.Sequence` for the steps and it takes around 15s.

The [alternate C# solution](../../csharp/Day14/Program.cs) is not only shorter but also done in an eye-blink ... very sad :(

--- 

## UPDATE

I found a far more performant solution on reddit using an array that is dynamically grown.
I tried to adapt the solution. Mine seems to be a bit slower but I'm under 5s now so I guess it's fine ...

using System;
using System.Collections.Generic;

namespace Day14
{
    class Program
    {
        static string Input = "894501";

        static void Main(string[] args)
        {
            Console.WriteLine($"part 2: {RunPart2()}");
        }

        static bool Found(List<int> list, int offset = 0)
        {
            if (list.Count < Input.Length + offset)
                return false;

            for (var i = 0; i < Input.Length; i++)
            {
                var digit = Input[i] - '0';
                if (list[list.Count - Input.Length - offset + i] != Input[i] - '0')
                    return false;
            }
            return true;
        }

        static int RunPart2()
        {
            var recipes = new List<int> { 3, 7 };
            var elf1 = 0;
            var elf2 = 1;

            while (true)
            {
                if (Found(recipes))
                    return recipes.Count - Input.Length;
                if (Found(recipes, offset: 1))
                    return recipes.Count - Input.Length - 1;

                var sum = recipes[elf1] + recipes[elf2];
                if (sum >= 10)
                    recipes.Add(sum / 10);
                recipes.Add(sum % 10);

                elf1 = (elf1 + recipes[elf1] + 1) % recipes.Count;
                elf2 = (elf2 + recipes[elf2] + 1) % recipes.Count;
            }
        }
    }
}

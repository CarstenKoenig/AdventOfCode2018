using System;
using System.Collections.Generic;
using System.Linq;

namespace Day8
{
    class Program
    {
        static void Main(string[] args)
        {
            var numbers = ReadFile();
            var index = 0;

            var node = Node.ParseNode(numbers, ref index);

            Console.WriteLine($"Part 1: {node.SumMeta()}");
            Console.WriteLine($"Part 2: {node.Value()}");
        }

        static int[] ReadFile()
        {
            return System.IO.File.ReadAllText("input.txt")
                .Split(new[] { ' ', '\n', '\r' }, StringSplitOptions.RemoveEmptyEntries)
                .Select(Int32.Parse)
                .ToArray();
        }
    }

    public class Node
    {
        public int[] Meta;
        public Dictionary<int, Node> Children;

        public int SumMeta()
        {
            return Meta.Sum() + Children.Values.Sum(c => c.SumMeta());
        }

        public int Value()
        {
            if (!Children.Any())
                return Meta.Sum();

            int childValue(int ind)
            {
                if (!Children.TryGetValue(ind, out var child))
                    return 0;
                return child.Value();
            }

            return Meta.Sum(childValue);
        }

        public static Node ParseNode(int[] numbers, ref int index)
        {
            var nrChildren = numbers[index++];
            var nrMeta = numbers[index++];
            var newNode = new Node();
            newNode.Children = new Dictionary<int, Node>();
            for (var i = 0; i < nrChildren; i++)
                newNode.Children[i + 1] = ParseNode(numbers, ref index);
            newNode.Meta = new int[nrMeta];
            Array.Copy(numbers, index, newNode.Meta, 0, nrMeta);
            index += nrMeta;

            return newNode;
        }
    }
}

using System;
using System.Collections.Generic;
using System.Linq;

namespace Day9
{
  class Program
  {
    static void Main(string[] args)
    {
      var nrPlayers = 425;
      var lastMarble = 70848 * 100;

      var players = new Dictionary<int, Int64>(Enumerable.Range(0, nrPlayers).Select(p => KeyValuePair.Create(p, 0L)));
      var curPlayer = 0;
      var circle = new Circle();

      for (var i = 1; i <= lastMarble; i++)
      {
        Int64 score = circle.GameMove(i);
        players[curPlayer] += score;
        curPlayer++;
        if (curPlayer >= players.Count) curPlayer = 0;
      }

      var highScore = players.Max(kvp => kvp.Value);

      Console.WriteLine($"with {nrPlayers} and last marble={lastMarble} hightScore is: {highScore}");
    }
  }

  class Circle
  {
    public Circle()
    {
      Start = new Item(0, null, null);
      Focus = Start;
      Focus.Next = Focus;
      Focus.Prev = Focus;
    }

    public Int64 GameMove(Int64 newValue)
    {
      if (newValue % 23 == 0)
        return ScoreMove(newValue);
      return InsertMove(newValue);
    }

    public Int64 InsertMove(Int64 newValue)
    {
      MoveNext();
      InsertRight(newValue);
      return 0;
    }

    public Int64 ScoreMove(Int64 newValue)
    {
      Move(-7);
      return DeleteRight() + newValue;
    }

    public override string ToString()
    {
      var sb = new System.Text.StringBuilder();
      var item = Start;
      do
      {
        if (item == Focus)
          sb.Append($"({item.Value}) ");
        else
          sb.Append($"{item.Value} ");
        item = item.Next;
      } while (item != Start);
      return sb.ToString();
    }

    public void MoveNext()
    {
      Focus = Focus.Next;
    }

    public void MovePrev()
    {
      Focus = Focus.Prev;
    }

    public Int64 DeleteRight()
    {
      var curFocus = Focus;
      curFocus.Prev.Next = curFocus.Next;
      curFocus.Next.Prev = curFocus.Prev;
      Focus = curFocus.Next;
      return curFocus.Value;
    }

    public void InsertRight(Int64 newValue)
    {
      var newFocus = new Item(newValue, Focus, Focus.Next);
      Focus.Next.Prev = newFocus;
      Focus.Next = newFocus;
      Focus = newFocus;
    }

    public void Move(Int64 steps)
    {
      while (steps > 0)
      {
        MoveNext();
        steps--;
      }
      while (steps < 0)
      {
        MovePrev();
        steps++;
      }

    }
    private Item Start { get; set; }
    private Item Focus { get; set; }
    public Int64 FocusValue => Focus.Value;

    class Item
    {
      public Item(Int64 value, Item prev, Item next)
      {
        Value = value;
        Prev = prev;
        Next = next;
      }
      public Int64 Value { get; internal set; }
      public Item Prev { get; internal set; }
      public Item Next { get; internal set; }
    }
  }
}

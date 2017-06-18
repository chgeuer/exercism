defmodule TwelveDays do
  @prefix1 "On the "

  @numbers [
    "first", "second" , "third", "fourth",
    "fifth", "sixth", "seventh", "eighth",
    "ninth", "tenth", "eleventh", "twelfth"
  ]
  def numbers(count) when count > 0 and count <= 12, do: @numbers |> Enum.drop(count - 1) |> Enum.take(1)

  @prefix2 " day of Christmas my true love gave to me, "

  @goods [
    "a Partridge in a Pear Tree", "two Turtle Doves", "three French Hens", "four Calling Birds", 
    "five Gold Rings", "six Geese-a-Laying", "seven Swans-a-Swimming", "eight Maids-a-Milking", 
    "nine Ladies Dancing", "ten Lords-a-Leaping", "eleven Pipers Piping", "twelve Drummers Drumming"
  ]
  def goods(1), do: @goods |> Enum.take(1)
  def goods(count) when count > 1 and count <= 12 do
    items = @goods |> Enum.drop(1) |> Enum.take(count - 1) |> Enum.reverse |> Enum.join(", ")
    lst = @goods |> Enum.take(1)
    "#{ items }, and #{lst}"
  end

  @doc """
  Given a `number`, return the song's verse for that specific day, including
  all gifts for previous days in the same line.
  """
  @spec verse(number :: integer) :: String.t()
  def verse(number) do
    "#{ @prefix1 }#{ numbers(number) }#{ @prefix2 }#{ goods(number) }."
  end

  @doc """
  Given a `starting_verse` and an `ending_verse`, return the verses for each
  included day, one per line.
  """
  @spec verses(starting_verse :: integer, ending_verse :: integer) :: String.t()
  def verses(starting_verse, ending_verse) do
    x = for n <- starting_verse..ending_verse, do: verse(n)
    x |> Enum.join("\n")
  end

  @doc """
  Sing all 12 verses, in order, one verse per line.
  """
  @spec sing():: String.t()
  def sing, do: verses(1, 12)
end


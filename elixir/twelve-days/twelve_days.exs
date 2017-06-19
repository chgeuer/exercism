defmodule TwelveDays do
  @numbers [
    "first", "second" , "third", "fourth", "fifth", "sixth", 
    "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"
  ]
  @spec goods(number :: integer) :: String.t()
  defp as_written_number(number) when number > 0 and number <= 12, do: @numbers |> Enum.drop(number - 1) |> Enum.take(1)

  @goods [
    "a Partridge in a Pear Tree", "two Turtle Doves", "three French Hens", "four Calling Birds", 
    "five Gold Rings", "six Geese-a-Laying", "seven Swans-a-Swimming", "eight Maids-a-Milking", 
    "nine Ladies Dancing", "ten Lords-a-Leaping", "eleven Pipers Piping", "twelve Drummers Drumming"
  ]
  @spec goods(number :: integer) :: String.t()
  defp goods(1), do: @goods |> Enum.take(1)
  defp goods(number) when number > 1 and number <= 12 do
    items = @goods |> Enum.drop(1) |> Enum.take(number - 1) |> Enum.reverse |> Enum.join(", ")
    lst = @goods |> Enum.take(1)
    "#{ items }, and #{lst}"
  end

  @doc """
  Given a `number`, return the song's verse for that specific day, including
  all gifts for previous days in the same line.
  """
  @spec verse(number :: integer) :: String.t()
  def verse(number) do
    "On the #{ number |> as_written_number } day of Christmas my true love gave to me, #{ number |> goods }."
  end

  @doc """
  Given a `starting_verse` and an `ending_verse`, return the verses for each
  included day, one per line.
  """
  @spec verses(starting_verse :: integer, ending_verse :: integer) :: String.t()
  def verses(starting_verse, ending_verse) do
    (for n <- starting_verse..ending_verse, do: verse(n))
    |> Enum.join("\n")
  end

  @doc """
  Sing all 12 verses, in order, one verse per line.
  """
  @spec sing():: String.t()
  def sing, do: verses(1, 12)
end

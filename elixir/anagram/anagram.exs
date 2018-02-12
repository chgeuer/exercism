defmodule Anagram do
  @doc """
  Returns all candidates that are anagrams of, but not equal to, 'base'.
  """
  @spec match(String.t(), [String.t()]) :: [String.t()]
  def match(base, candidates) do
    target = base |> dict_from_word

    candidates
    |> Enum.map(&{&1, &1 |> dict_from_word})
    |> Enum.filter(&(base |> String.downcase() != &1 |> elem(0) |> String.downcase() && target == &1 |> elem(1)))
    |> Enum.map(&(&1 |> elem(0)))
  end

  def dict_from_word(word) do
    word
    |> String.downcase()
    |> String.codepoints()
    |> aggregate(Map.new())
  end

  def aggregate([], map), do: map

  def aggregate([h | t], map) do
    t |> aggregate(map |> Map.get_and_update(h, &{&1, (&1 || 0) + 1}) |> elem(1))
  end
end

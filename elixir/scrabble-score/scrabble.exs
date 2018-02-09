defmodule Scrabble do
  @doc """
  Calculate the scrabble score for the word.
  """
  @spec score(String.t()) :: non_neg_integer
  def score(word) do
    word
    |> String.upcase()
    |> String.codepoints()
    |> sum(0, the_values())
  end

  defp sum([], aggregate, _values) when is_integer(aggregate), do: aggregate

  defp sum([head | tail], aggregate, values) when is_integer(aggregate) do
    case values[head] do
      nil -> sum(tail, aggregate + 0, values)
      val -> sum(tail, aggregate + val, values)
    end
  end

  defp the_values do
    [
      {"AEIOULNRST", 1},
      {"DG", 2},
      {"BCMP", 3},
      {"FHVWY", 4},
      {"K", 5},
      {"JX", 8},
      {"QZ", 10}
    ]
    |> List.foldl(Map.new(), fn {string, weight}, map ->
      map_for_single_value(string, weight) |> Map.merge(map)
    end)
  end

  defp map_for_single_value(string, weight) do
    string
    |> String.upcase()
    |> String.codepoints()
    |> add(weight, Map.new())
  end

  defp add([], _, map), do: map
  defp add([head | tail], weight, map), do: add(tail, weight, map |> Map.put(head, weight))
end

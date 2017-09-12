defmodule Words do
  @doc """
  Count the number of words in the sentence.

  Words are compared case-insensitively.
  """
  @spec count(String.t) :: map
  def count(sentence) do
    # http://www.regular-expressions.info/unicode.html#prop
    lowered = sentence 
    |> String.downcase
    |> String.replace("_", " ")
    
    words = Regex.split(~r/\b/u, lowered, trim: true)
    |> Enum.filter(&(!Regex.match?(~r/[\p{P}\s]/u, &1)))

    aggregate(words, %{})
  end

  defp aggregate([], map), do: map

  defp aggregate([head | tail], map) do
    { _, map } = map |> Map.get_and_update(head, fn(x) -> {x, (x || 0) + 1} end)
    aggregate(tail, map)
  end
end

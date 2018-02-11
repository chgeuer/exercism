defmodule SumOfMultiples do
  @doc """
  Adds up all numbers from 1 to a given end number that are multiples of the factors provided.
  """
  @spec to(non_neg_integer, [non_neg_integer]) :: non_neg_integer
  def to(limit, factors) when is_integer(limit) and limit > 0 do
    factors
    |> Enum.uniq()
    |> Enum.filter(&(&1 < limit))
    |> Enum.map(&({ &1, div(limit - 1, &1) }))
    |> iterate(Map.new)
    |> Map.keys()
    |> Enum.sum()
  end

  def iterate([], map), do: map
  def iterate([{ val, count } | tail ], map) do
    iterate(tail, series(val, count, map))
  end

  def series(_val, 0, map), do: map
  def series(val, count, map) do
    series(val, count-1, map |> Map.put( count * val, true))
  end
end

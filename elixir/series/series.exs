defmodule StringSeries do
  @doc """
  Given a string `s` and a positive integer `size`, return all substrings
  of that size. If `size` is greater than the length of `s`, or less than 1,
  return an empty list.
  """
  @spec slices(s :: String.t(), size :: integer) :: list(String.t())
  def slices(_s, size) when size <= 0, do: []
  def slices(s, size) do
    chars = s |> String.codepoints
    slices_impl(chars, length(chars), size)
  end

  defp slices_impl(chars, len, size) when len < size, do: []
  defp slices_impl(chars, len, size) when len >= size do
    for p <- 0..(len - size) do
      chars |> Enum.slice(p, size) |> Enum.join
    end
  end
end
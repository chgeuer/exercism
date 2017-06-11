defmodule RotationalCipher do
  @doc """
  Given a plaintext and amount to shift by, return a rotated string.

  Example:
  iex> RotationalCipher.rotate("Attack at dawn", 13)
  "Nggnpx ng qnja"
  """
  @spec rotate(text :: String.t(), shift :: integer) :: String.t()
  def rotate(text, shift) do
    text 
    |> String.to_charlist
    |> Enum.map(&(transform(&1, shift)))
    |> to_string
  end

  defp transform(c, delta) when ?A <= c and c <= ?Z do
    i = rem((c - ?A) + delta, 26)
    if i < 0, do: i + ?A + 26, else: i + ?A
  end
  
  defp transform(c, delta) when ?a <= c and c <= ?z do
    i = rem((c - ?a) + delta, 26)
    if i < 0, do: i + ?a + 26, else: i + ?a
  end

  defp transform(c, _delta), do: c
end


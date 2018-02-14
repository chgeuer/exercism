defmodule Roman do
  @doc """
  Convert the number to a roman number.
  """
  @spec numerals(pos_integer) :: String.t()
  def numerals(number) do
    tnds(number) <> huns(number) <> tens(number) <> ones(number)
  end

  defp ones(n), do: n |> form(1, "I", "V", "X")
  defp tens(n), do: n |> form(10, "X", "L", "C")
  defp huns(n), do: n |> form(100, "C", "D", "M")
  defp tnds(n), do: n |> form(1000, "M", "?", "?")

  defp form(n, place, one, five, ten), do: n |> div(place) |> rem(10) |> place_value(one, five, ten)

  defp place_value(0, _one, _five, _ten), do: ""
  defp place_value(1, one, _five, _ten), do: one
  defp place_value(2, one, _five, _ten), do: one <> one
  defp place_value(3, one, _five, _ten), do: one <> one <> one
  defp place_value(4, one, five, _ten), do: one <> five
  defp place_value(5, _one, five, _ten), do: five
  defp place_value(6, one, five, _ten), do: five <> one
  defp place_value(7, one, five, _ten), do: five <> one <> one
  defp place_value(8, one, five, _ten), do: five <> one <> one <> one
  defp place_value(9, one, _five, ten), do: one <> ten
end

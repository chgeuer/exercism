defmodule Roman do
  @doc """
  Convert the number to a roman number.
  """
  @spec numerals(pos_integer) :: String.t()
  def numerals(number) when 0 <= number and number < 10,
    do: ["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"] |> Enum.at(number)

  def numerals(number) when 10 <= number and number < 100,
    do:
      (["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"] |> Enum.at(div(number, 10))) <>
        numerals(rem(number, 10))

  def numerals(number) when 100 <= number and number < 1000,
    do:
      (["", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"] |> Enum.at(div(number, 100))) <>
        numerals(rem(number, 100))

  def numerals(number) when 1000 <= number, do: "M" <> numerals(number - 1000)
end

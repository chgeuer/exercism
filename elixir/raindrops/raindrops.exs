defmodule Raindrops do
  @doc """
  Returns a string based on raindrop factors.

  - If the number contains 3 as a prime factor, output 'Pling'.
  - If the number contains 5 as a prime factor, output 'Plang'.
  - If the number contains 7 as a prime factor, output 'Plong'.
  - If the number does not contain 3, 5, or 7 as a prime factor,
    just pass the number's digits straight through.
  """
  @spec convert(pos_integer) :: String.t
  def convert(number) do

    plingplangplong = [ 
      "#{if 0 == rem(number, 3), do: "Pling"}", 
      "#{if 0 == rem(number, 5), do: "Plang"}", 
      "#{if 0 == rem(number, 7), do: "Plong"}" 
    ] |> Enum.join
      
    case plingplangplong do
      "" -> number |> to_string
      _ -> plingplangplong
    end
  end
end

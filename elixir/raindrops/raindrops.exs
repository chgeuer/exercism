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
    case %{ :three => 0 == rem(number, 3), :five => 0 == rem(number, 5), :seven => 0 == rem(number, 7) } do
      %{ :three => false, :five => false, :seven => false } -> number |> to_string         
      %{ :three => true, :five => false, :seven => false } -> "Pling"         
      %{ :three => false, :five => true, :seven => false } -> "Plang"         
      %{ :three => false, :five => false, :seven => true } -> "Plong"         
      %{ :three => true, :five => true, :seven => false } -> "PlingPlang"         
      %{ :three => true, :five => false, :seven => true } -> "PlingPlong"         
      %{ :three => false, :five => true, :seven => true } -> "PlangPlong"         
      %{ :three => true, :five => true, :seven => true } -> "PlingPlangPlong"         
    end
  end
end

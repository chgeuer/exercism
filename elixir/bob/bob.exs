defmodule Bob do
  def hey(input) do
    input = input |> String.trim
    cond do
      input |> isEmpty -> "Fine. Be that way!"
      input |> isQuestion -> "Sure."
      input |> isShouting -> "Whoa, chill out!"
      true -> "Whatever."
    end
  end

  def isEmpty(input), do: input |> String.equivalent?("")
  def isQuestion(input), do: input |> String.ends_with?("?")
  def isUppercase(input), do: input |> String.upcase == input
  def isContainsChars(input), do: not (input |> String.match?(~r/^[\s01234567890,.-]*$/))
  def isShouting(input), do: isUppercase(input) && isContainsChars(input) 
end

defmodule Scrabble do
  @doc """
  Calculate the scrabble score for the word.
  """
  @spec score(String.t()) :: non_neg_integer
  def score(word), do: word |> String.upcase |> count(0)

  @mapping %{
    ~c(AEIOULNRST) => 1,
    ~c(DG) => 2,
    ~c(BCMP) => 3,
    ~c(FHVWY) => 4,
    ~c(K) => 5,
    ~c(JX) => 8,
    ~c(QZ) => 10
  }

  for {letters, score} <- @mapping do
    for letter <- letters do
      defp count(<<unquote(letter), rest::binary>>, acc), do: count(rest, acc + unquote(score))
    end
  end

  defp count("", acc), do: acc

  defp count(<<_, rest::binary>>, acc), do: count(rest, acc)
end

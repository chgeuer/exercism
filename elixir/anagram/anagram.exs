defmodule Anagram do
  @doc """
  Returns all candidates that are anagrams of, but not equal to, 'base'.
  """
  @spec match(String.t(), [String.t()]) :: [String.t()]
  def match(base, candidates) do
    comparable = &(&1 |> String.downcase() |> to_charlist() |> Enum.sort())
    base_comparable = comparable.(base)

    candidates
    |> Enum.reject(&(&1 |> String.downcase() == base |> String.downcase()))
    |> Enum.map(&{&1, comparable.(&1)})
    |> Enum.reject(&(base_comparable != &1 |> elem(1)))
    |> Enum.map(&(&1 |> elem(0)))
  end
end

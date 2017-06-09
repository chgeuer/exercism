defmodule NucleotideCount do
  @nucleotides [?A, ?C, ?G, ?T]

  @doc """
  Counts individual nucleotides in a NucleotideCount strand.

  ## Examples

  iex> NucleotideCount.count('AATAA', ?A)
  4

  iex> NucleotideCount.count('AATAA', ?T)
  1
  """
  @spec count([char], char) :: non_neg_integer
  def count(strand, nucleotide), do: histogram(strand) |> Map.get(nucleotide)

  @doc """
  Returns a summary of counts by nucleotide.

  ## Examples

  iex> NucleotideCount.histogram('AATAA')
  %{?A => 4, ?T => 1, ?C => 0, ?G => 0}
  """
  @spec histogram([char]) :: map
  def histogram(strand), do: histogram(strand, %{ ?A => 0, ?T => 0, ?C => 0, ?G => 0 })

  defp histogram('', map), do: map
  defp histogram('C' ++ tail, map), do: histogram(tail, map |> Map.update!(?C, &(&1 + 1)))
  defp histogram('T' ++ tail, map), do: histogram(tail, map |> Map.update!(?T, &(&1 + 1)))
  defp histogram('G' ++ tail, map), do: histogram(tail, map |> Map.update!(?G, &(&1 + 1)))
  defp histogram('A' ++ tail, map), do: histogram(tail, map |> Map.update!(?A, &(&1 + 1)))

end

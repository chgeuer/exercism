defmodule RNATranscription do
  @doc """
  Transcribes a character list representing DNA nucleotides to RNA

  ## Examples

  iex> RNATranscription.to_rna('ACTG')
  'UGAC'
  """
  @spec to_rna([char]) :: [char]
  for {from, to} <- [{'G', 'C'}, {'C', 'G'}, {'T', 'A'}, {'A', 'U'}] do
    def to_rna(unquote(from) ++ tail), do: unquote(to) ++ to_rna(tail)
  end

  def to_rna(''), do: ''
end

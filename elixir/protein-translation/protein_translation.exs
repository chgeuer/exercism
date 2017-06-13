defmodule ProteinTranslation do
  defmacro is_rna(rna), do: quote do: is_binary(unquote(rna)) and rem(byte_size(unquote(rna)), 3) == 0

  @doc """
  Given an RNA string, return a list of proteins specified by codons, in order.
  """
  @spec of_rna(String.t()) :: { atom,  list(String.t()) }
  def of_rna(rna) when not is_rna(rna), do: { :error, "invalid RNA" }
  def of_rna(rna) when is_rna(rna), do: rna |> split_rna([])
  
  def split_rna("", { :error }), do: { :error }
  def split_rna("", result), do: { :ok, result |> Enum.reverse }
  def split_rna(rna, result) do
    <<a::size(8), b::size(8), c::size(8), rest::binary>> = rna

    case <<a, b, c>> |> of_codon do
      { :error, _ } -> { :error, "invalid RNA" }
      { :ok, "STOP" } -> { :ok, result |> Enum.reverse }
      { :ok, codon } -> split_rna(rest, [codon | result])
    end
  end

  @codons %{
    "UGU" => "Cysteine",
    "UGC" => "Cysteine",
    "UUA" => "Leucine",
    "UUG" => "Leucine",
    "AUG" => "Methionine",
    "UUU" => "Phenylalanine",
    "UUC" => "Phenylalanine",
    "UCU" => "Serine",
    "UCC" => "Serine",
    "UCA" => "Serine",
    "UCG" => "Serine",
    "UGG" => "Tryptophan",
    "UAU" => "Tyrosine",
    "UAC" => "Tyrosine",
    "UAA" => "STOP",
    "UAG" => "STOP",
    "UGA" => "STOP",
  }

  def codons(), do: @codons

  @doc """
  Given a codon, return the corresponding protein
  """
  @spec of_codon(String.t()) :: { atom, String.t() }
  def of_codon(codon) do
    case @codons[codon] do
      nil -> { :error, "invalid codon" }
      val -> { :ok, val }
    end
  end
end

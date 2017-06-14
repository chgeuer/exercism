defmodule PigLatin do
  @doc """
  Given a `phrase`, translate it a word at a time to Pig Latin.

  Words beginning with consonants should have the consonant moved to the end of
  the word, followed by "ay".

  Words beginning with vowels (aeiou) should have "ay" added to the end of the
  word.

  Some groups of letters are treated like consonants, including "ch", "qu",
  "squ", "th", "thr", and "sch".

  Some groups are treated like vowels, including "yt" and "xr".
  """
  @spec translate(phrase :: String.t()) :: String.t()
  def translate(phrase) do
    phrase
    |> String.split(" ", trim: true)
    |> Enum.map(&(translate_word(&1)))
    |> Enum.join(" ")
  end

  @spec translate_word(phrase :: String.t()) :: String.t()
  defp translate_word(phrase) do
    cond do
      phrase |> String.match?(~r/^(thr|sch)/i) -> phrase |> rule3
      phrase |> String.match?(~r/^(ch|qu|th)/i) -> phrase |> rule2
      phrase |> String.match?(~r/^(yt|xr)/i) -> phrase |> rule0
      phrase |> String.match?(~r/^([bcdfghjklmnpqrstvwxyz])/i) -> phrase |> rule1
      phrase |> String.match?(~r/^([aeiou])/i) -> phrase |> rule0
      true -> "misc"
    end
  end

  defp rule0(phrase), do: "#{phrase}ay"
  defp rule1(<< c1 :: utf8, rest :: binary >>), do: "#{ rest |> to_string }#{ << c1 :: utf8 >>}ay"
  defp rule2(<< c1 :: utf8, c2 :: utf8, rest :: binary >>), do: "#{ rest |> to_string }#{ << c1 :: utf8, c2 :: utf8 >>}ay"
  defp rule3(<< c1 :: utf8, c2 :: utf8, c3 :: utf8, rest :: binary >>), do: "#{ rest |> to_string }#{ << c1 :: utf8, c2 :: utf8, c3 :: utf8 >>}ay"
end

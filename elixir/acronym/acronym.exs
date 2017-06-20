defmodule Acronym do
  @doc """
  Generate an acronym from a string.
  "This is a string" => "TIAS"
  """
  @spec abbreviate(String.t()) :: String.t()
  def abbreviate(string) do
    # \p{Ll}: a lowercase letter that has an uppercase variant. 
    # \p{Lu}: an uppercase letter that has a lowercase variant
    # \p{P}:  any kind of punctuation character

    Regex.replace(~r/(\p{Ll})(\p{Lu})/, string, "\\1 \\2", global: true)
    |> String.split(~r/[\p{P}\s]/, trim: true)
    |> Enum.map(&(String.slice(&1, 0, 1)))
    |> Enum.join("")
    |> String.upcase
  end
end

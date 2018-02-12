defmodule BracketPush do
  @doc """
  Checks that all the brackets and braces in the string are matched correctly, and nested correctly
  """
  @spec check_brackets(String.t()) :: boolean
  def check_brackets(str) do
    str
    |> String.codepoints()
    |> check([])
  end

  for {opener, closer} <- [{"{", "}"}, {"(", ")"}, {"[", "]"}, {"<", ">"}] do
    defp check([unquote(opener) | tail], stack), do: check(tail, [unquote(opener) | stack])
    defp check([unquote(closer) | tail], [unquote(opener) | stack]), do: check(tail, stack)
    defp check([unquote(closer) | _], _), do: false
  end

  defp check([_head | tail], stack), do: check(tail, stack)
  defp check([], []), do: true
  defp check([], _), do: false
end

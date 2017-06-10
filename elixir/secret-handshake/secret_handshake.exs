defmodule SecretHandshake do
  @doc """
  Determine the actions of a secret handshake based on the binary
  representation of the given `code`.

  If the following bits are set, include the corresponding action in your list
  of commands, in order from lowest to highest.

  1 = wink
  10 = double blink
  100 = close your eyes
  1000 = jump

  10000 = Reverse the order of the operations in the secret handshake
  """
  @spec commands(code :: integer) :: list(String.t())
  def commands(code) do
    <<_::27, reverse::1, jump::1, close_your_eyes::1, double_blink::1, wink::1>> = <<code::32>>

    [ ] 
    |> append(jump, "jump") 
    |> append(close_your_eyes, "close your eyes") 
    |> append(double_blink, "double blink") 
    |> append(wink, "wink") 
    |> reverseIf(reverse)
  end
  
  defp append(list, 0, _str), do: list
  defp append(list, 1, str), do: [ str | list ]

  defp reverseIf(list, 0), do: list
  defp reverseIf(list, 1), do: Enum.reverse(list)
end


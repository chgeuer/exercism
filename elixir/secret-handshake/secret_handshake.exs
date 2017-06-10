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

    append = fn (val, str, list) -> if val == 1, do: [ str | list ], else: list end

    result = [ ]
    result = append.(jump, "jump", result)
    result = append.(close_your_eyes, "close your eyes", result) 
    result = append.(double_blink, "double blink", result) 
    result = append.(wink, "wink", result) 
    if reverse == 0, do: result, else: Enum.reverse(result), else: result
  end
end


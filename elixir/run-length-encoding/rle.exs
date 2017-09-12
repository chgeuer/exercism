defmodule RunLengthEncoder do
  @doc """
  Generates a string where consecutive elements are represented as a data value and count.
  "AABBBCCCC" => "2A3B4C"
  For this example, assume all input are strings, that are all uppercase letters.
  It should also be able to reconstruct the data into its original form.
  "2A3B4C" => "AABBBCCCC"
  """
  @spec encode(String.t) :: String.t
  def encode(string), do: encode_impl(string |> String.graphemes, nil, 0, []) |> to_string

  defp encode_impl([], _lastchar, 0, result), do: result |> Enum.join |> String.reverse 

  defp encode_impl([], lastchar, count, result) when count > 0 do 
    cond do
      count == 1 -> encode_impl([], lastchar, 0, [ lastchar, result ])
      count > 1  -> encode_impl([], lastchar, 0, [ lastchar, "#{count |> to_string}" |> String.reverse, result ])
    end
  end

  defp encode_impl([head | tail], lastchar, count, result) do
    cond do
      lastchar ==  nil -> encode_impl(tail, head, 1,         result)
      lastchar == head -> encode_impl(tail, head, count + 1, result)
      lastchar != head && count == 1 -> encode_impl(tail, head, 1, [ lastchar, result ])
      lastchar != head && count > 1  -> encode_impl(tail, head, 1, [ lastchar, "#{count |> to_string}"|> String.reverse, result ])
    end
  end

  @spec decode(String.t) :: String.t
  def decode(string) do
    string |> String.graphemes |> decode_impl(0, [])
  end

  defp decode_impl([], _multiplier, acc), do: acc |> Enum.reverse |> Enum.join

  defp decode_impl([head | tail], multiplier, acc) do
    cond do
      head in ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"] -> 
        decode_impl(tail, multiplier * 10 + String.to_integer(head), acc)
      true -> 
        decode_impl(tail, 0, [ String.duplicate(head, max(1, multiplier)) | acc])
    end
  end
end

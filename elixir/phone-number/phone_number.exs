defmodule Phone do
  @invalid "0000000000"

  @doc """
  Remove formatting from a phone number.

  Returns "0000000000" if phone number is not valid
  (10 digits or "1" followed by 10 digits)

  ## Examples

  iex> Phone.number("212-555-0100")
  "2125550100"

  iex> Phone.number("+1 (212) 555-0100")
  "2125550100"

  iex> Phone.number("+1 (212) 055-0100")
  "0000000000"

  iex> Phone.number("(212) 555-0100")
  "2125550100"

  iex> Phone.number("867.5309")
  "0000000000"
  """
  @spec number(String.t()) :: String.t()
  def number(str) do
    case str |> scan_numbers() do
      <<"10"::utf8, _::9*8>> -> @invalid
      <<"11"::utf8, _::9*8>> -> @invalid
      <<"1"::utf8, _::3*8, "0"::utf8, _::6*8>> -> @invalid
      <<"1"::utf8, _::3*8, "1"::utf8, _::6*8>> -> @invalid
      <<"1"::utf8, x::10*8>> -> <<x::10*8>>
      <<"0"::utf8, _::9*8>> -> @invalid
      <<"1"::utf8, _::9*8>> -> @invalid
      <<_::3*8, "0"::utf8, _::6*8>> -> @invalid
      <<_::3*8, "1"::utf8, _::6*8>> -> @invalid
      <<x::10*8>> -> <<x::10*8>>
      _ -> @invalid
    end
  end

  #
  # Meta programming
  #
  # for n <- ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"], do: defp scan_numbers(unquote(n) <> tail), do: unquote(n) <> scan_numbers(tail)
  # for n <- ["+", "(", ")", " ", "-", "."], do: defp scan_numbers(unquote(n) <> tail), do: scan_numbers(tail)
  # defp scan_numbers(""), do: ""
  # defp scan_numbers(_), do: @invalid

  #
  # Guard clauses
  #
  @plus ?+
  @space 0x20
  @open ?(
  @close ?)
  @dash ?-
  @dot ?.

  defp scan_numbers(<<n::8>> <> tail) when n in [@plus, @space, @open, @close, @dash, @dot],
    do: scan_numbers(tail)

  defp scan_numbers(<<n::8>> <> tail) when n in [?0, ?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9],
    do: <<n::8>> <> scan_numbers(tail)

  defp scan_numbers(<<>>), do: ""
  defp scan_numbers(_), do: @invalid

  @doc """
  Extract the area code from a phone number

  Returns the first three digits from a phone number,
  ignoring long distance indicator

  ## Examples

  iex> Phone.area_code("212-555-0100")
  "212"

  iex> Phone.area_code("+1 (212) 555-0100")
  "212"

  iex> Phone.area_code("+1 (012) 555-0100")
  "000"

  iex> Phone.area_code("867.5309")
  "000"
  """
  @spec area_code(String.t()) :: String.t()
  def area_code(raw), do: raw |> number() |> String.slice(0, 3)

  @doc """
  Pretty print a phone number

  Wraps the area code in parentheses and separates
  exchange and subscriber number with a dash.

  ## Examples

  iex> Phone.pretty("212-555-0100")
  "(212) 555-0100"

  iex> Phone.pretty("212-155-0100")
  "(000) 000-0000"

  iex> Phone.pretty("+1 (303) 555-1212")
  "(303) 555-1212"

  iex> Phone.pretty("867.5309")
  "(000) 000-0000"
  """
  @spec pretty(String.t()) :: String.t()
  def pretty(raw) do
    case raw |> number() do
      <<area::bytes-size(3), exchange::bytes-size(3), subscriber::bytes-size(4)>> ->
        "(" <> <<area::bytes-size(3)>> <> ") " <> <<exchange::bytes-size(3)>> <> "-" <> <<subscriber::bytes-size(4)>>
    end
  end
end

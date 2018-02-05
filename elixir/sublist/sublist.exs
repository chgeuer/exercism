defmodule Sublist do
  @doc """
  Returns whether the first list is a sublist or a superlist of the second list
  and if not whether it is equal or unequal to the second list.
  """
  def compare(a, b) when not is_list(a) or not is_list(b), do: raise("Must provide 2 lists")

  def compare(a, b) do
    diff = (a |> Enum.count()) - (b |> Enum.count())

    cond do
      diff == 0 -> check_equality(a, b)
      diff > 0 -> if a |> contains(b), do: :superlist, else: :unequal
      diff < 0 -> if b |> contains(a), do: :sublist, else: :unequal
    end
  end

  defp check_equality([], []), do: :equal

  defp check_equality([ahead | atail], [bhead | btail]) do
    case ahead === bhead do
      false -> :unequal
      true -> check_equality(atail, btail)
    end
  end

  defp found_at_current_pos([], []), do: true
  defp found_at_current_pos(a, []) when is_list(a), do: true
  defp found_at_current_pos([], b) when is_list(b), do: false

  defp found_at_current_pos([lhead | _ltail], [shead | _stail]) when not (lhead === shead),
    do: false

  defp found_at_current_pos([head | ltail], [head | stail]),
    do: found_at_current_pos(ltail, stail)

  defp contains([], _), do: false

  defp contains(long = [_ | ltail], short) do
    case found_at_current_pos(long, short) do
      true -> true
      false -> contains(ltail, short)
    end
  end
end

defmodule Strain do

  @spec keep(list :: list(any), fun :: ((any) -> boolean)) :: list(any)
  def keep(list, fun) do
    enum_filter(list, fun)
  end

  @spec keep(list :: list(any), fun :: ((any) -> boolean)) :: list(any)
  defp enum_filter([ head | tail ], fun) do
    case fun.(head) do
      true  -> [ head | enum_filter(tail, fun) ]
      false -> enum_filter(tail, fun)
    end
  end
  defp enum_filter([], _fun), do: []

  @spec discard(list :: list(any), fun :: ((any) -> boolean)) :: list(any)
  def discard(list, fun) do
    enum_filter(list, fn(x) -> !fun.(x) end)
  end
end

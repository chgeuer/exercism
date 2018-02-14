defmodule Prime do
  @doc """
  Generates the nth prime.
  """
  @spec nth(non_neg_integer) :: non_neg_integer
  def nth(1), do: 2
  def nth(count) when count > 1, do: nth(2, 2, count, %{1 => 2})


  # purely functional, not using a process to keep state. 
  # Always recomputes for each call
  defp nth(potentialPrime, n, count, map) when n <= count do
    not_a_prime = map |> Map.values() |> Enum.any?(&(rem(potentialPrime, &1) == 0))

    if not_a_prime do
      nth(potentialPrime + 1, n, count, map)
    else
      nth(potentialPrime + 1, n + 1, count, map |> Map.put(n, potentialPrime))
    end
  end

  defp nth(_, _, count, map), do: map |> Map.get(count)
end

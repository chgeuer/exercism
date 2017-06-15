defmodule SpaceAge do
  @type planet :: :mercury | :venus | :earth | :mars | :jupiter
                | :saturn | :uranus | :neptune

  @spec in_seconds(float) :: float
  defp in_seconds(year) when is_float(year), do: year * 365.25 * 24 * 60 * 60

  @spec orbital_period_in_seconds(planet) :: float
  defp orbital_period_in_seconds(planet) do
    case planet do
      :earth -> 1.0 |> in_seconds
      :mercury -> 0.2408467 |> in_seconds
      :venus -> 0.61519726 |> in_seconds
      :mars -> 1.8808158 |> in_seconds
      :jupiter -> 11.862615 |> in_seconds
      :saturn -> 29.447498 |> in_seconds
      :uranus -> 84.016846 |> in_seconds
      :neptune -> 164.79132 |> in_seconds
      _ -> raise "Unknown planet :#{planet}"
    end
  end
    
  @doc """
  Return the number of years a person that has lived for 'seconds' seconds is
  aged on 'planet'.
  """
  @spec age_on(planet, pos_integer) :: float
  def age_on(planet, seconds), do: seconds / orbital_period_in_seconds(planet)
end

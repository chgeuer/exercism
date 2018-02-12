defmodule LinkedList do
  @opaque t :: tuple()

  @enforce_keys [:value, :length, :next_elem]
  defstruct [:value, :length, :next_elem]

  @doc """
  Construct a new LinkedList
  """
  @spec new() :: t
  def new(), do: {}

  @doc """
  Push an item onto a LinkedList
  """
  @spec push(t, any()) :: t
  def push(list, elem), do: { elem, list }

  @doc """
  Calculate the length of a LinkedList
  """
  @spec length(t) :: non_neg_integer()
  def length(list), do: _length(list, 0)

  def _length({ }, len), do: len
  def _length({ head }, len), do: len + 1
  def _length({ head, tail }, len), do: _length(tail, len + 1)

  @doc """
  Determine if a LinkedList is empty
  """
  @spec empty?(t) :: boolean()
  def empty?({}), do: true
  def empty?(_), do: false

  @doc """
  Get the value of a head of the LinkedList
  """
  @spec peek(t) :: {:ok, any()} | {:error, :empty_list}
  def peek({ }), do: {:error, :empty_list}
  def peek({ head }), do: {:ok, head}
  def peek({ head, tail }), do: {:ok, head}

  @doc """
  Get tail of a LinkedList
  """
  @spec tail(t) :: {:ok, t} | {:error, :empty_list}
  def tail({}), do: {:error, :empty_list}
  def tail({ head }), do: {:error, :empty_list}
  def tail({ head, tail }), do: {:ok, tail}

  @doc """
  Remove the head from a LinkedList
  """
  @spec pop(t) :: {:ok, any(), t} | {:error, :empty_list}
  def pop({}), do: {:error, :empty_list}
  def pop({head}), do: {:ok, head, {}}
  def pop({head, tail}), do: {:ok, head, tail }

  @doc """
  Construct a LinkedList from a stdlib List
  """
  @spec from_list(list()) :: t
  def from_list(list), do: list |> from_list(new()) |> reverse()

  defp from_list([], linked_list), do: linked_list
  defp from_list([head | tail], linked_list), do: tail |> from_list(linked_list |> push(head))

  @doc """
  Construct a stdlib List LinkedList from a LinkedList
  """
  @spec to_list(t) :: list()
  def to_list(list), do: list |> reverse() |> to_list([])

  defp to_list({}, array), do: array
  defp to_list({head, tail}, array), do: to_list(tail, [head | array])

  @doc """
  Reverse a LinkedList
  """
  @spec reverse(t) :: t
  def reverse(list), do: list |> reverse(new())

  defp reverse({}, dest_list), do: dest_list
  defp reverse({head, tail}, dest_list), do: tail |> reverse({ head, dest_list })
end

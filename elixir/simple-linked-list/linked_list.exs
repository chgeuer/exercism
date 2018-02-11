defmodule LinkedList do
  @opaque t :: tuple()

  @enforce_keys [:value, :length, :next_elem]
  defstruct [:value, :length, :next_elem]

  @doc """
  Construct a new LinkedList
  """
  @spec new() :: t
  def new(), do: %LinkedList{next_elem: nil, value: nil, length: 0}

  @doc """
  Push an item onto a LinkedList
  """
  @spec push(t, any()) :: t
  def push(list, elem) do
    %LinkedList{next_elem: list, value: elem, length: list.length + 1}
  end

  @doc """
  Calculate the length of a LinkedList
  """
  @spec length(t) :: non_neg_integer()
  def length(%LinkedList{length: length}), do: length

  @doc """
  Determine if a LinkedList is empty
  """
  @spec empty?(t) :: boolean()
  def empty?(%LinkedList{length: length}), do: length == 0

  @doc """
  Get the value of a head of the LinkedList
  """
  @spec peek(t) :: {:ok, any()} | {:error, :empty_list}
  def peek(%LinkedList{length: 0}), do: {:error, :empty_list}
  def peek(%LinkedList{value: value}), do: {:ok, value}

  @doc """
  Get tail of a LinkedList
  """
  @spec tail(t) :: {:ok, t} | {:error, :empty_list}
  def tail(%LinkedList{length: 0}), do: {:error, :empty_list}

  def tail(%LinkedList{next_elem: next_elem}), do: {:ok, next_elem}

  @doc """
  Remove the head from a LinkedList
  """
  @spec pop(t) :: {:ok, any(), t} | {:error, :empty_list}
  def pop(%LinkedList{length: 0}), do: {:error, :empty_list}

  def pop(%LinkedList{value: value, next_elem: next_elem}), do: {:ok, value, next_elem}

  @doc """
  Construct a LinkedList from a stdlib List
  """
  @spec from_list(list()) :: t
  def from_list(list), do: list |> from_list(LinkedList.new()) |> reverse()
  defp from_list([], linked_list), do: linked_list
  defp from_list([head | tail], linked_list), do: tail |> from_list(linked_list |> push(head))

  @doc """
  Construct a stdlib List LinkedList from a LinkedList
  """
  @spec to_list(t) :: list()
  def to_list(list) do
    list
    |> reverse()
    |> to_list([])
  end

  defp to_list(%LinkedList{length: 0}, array), do: array

  defp to_list(%LinkedList{value: value, next_elem: next_elem}, array),
    do: to_list(next_elem, [value | array])

  @doc """
  Reverse a LinkedList
  """
  @spec reverse(t) :: t
  def reverse(list) do
    list
    |> reverse(LinkedList.new())
  end

  defp reverse(%LinkedList{length: 0}, dest_list), do: dest_list

  defp reverse(
         %LinkedList{length: _, value: src_value, next_elem: src_next_elem},
         dest_list = %LinkedList{length: dest_length}
       ) do
    reverse(src_next_elem, %LinkedList{
      length: dest_length + 1,
      value: src_value,
      next_elem: dest_list
    })
  end
end

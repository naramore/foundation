defprotocol Kanren.Reifiable do
  @moduledoc """
  """
  alias Kanren.Store

  @fallback_to_any true

  @doc """
  """
  @spec reify(t(), Store.t()) :: Store.t()
  def reify(v, s)

  @doc """
  """
  @spec unify(t(), t(), Store.t()) :: Store.t() | nil
  def unify(u, v, s)

  @doc """
  """
  @spec occurs_check(t(), t(), Store.t()) :: Store.t()
  def occurs_check(u, v, s)

  @doc """
  """
  @spec walk(t(), (t() -> t())) :: t()
  def walk(v, f)

  @doc """
  """
  @spec is_tree?(t()) :: boolean()
  def is_tree?(v)
end

defimpl Kanren.Reifiable, for: List do
  def reify([h | t], s) do
    h
    |> Kanren.reify_s(s)
    |> then(&@protocol.reify(t, &1))
  end
  def reify(_, s), do: s

  def unify([uh | ut], [vh | vt], s) do
    case Kanren.unify(uh, vh, s) do
      nil -> nil
      s -> @protocol.unify(ut, vt, s)
    end
  end
  def unify(_u, _v, _s), do: nil

  def occurs_check([], _v, _s), do: false
  def occurs_check([h | t], v, s),
    do: Kanren.occurs_check(v, h, s) or @protocol.occurs_check(t, v, s)

  def walk([]), do: []
  def walk([h | t], f), do: [f.(h) | f.(t)]

  def is_tree?(_), do: false
end

defimpl Kanren.Reifiable, for: Tuple do
  def reify(v, s), do: v |> Tuple.to_list() |> @protocol.reify(s)

  def unify(u, v, s) when is_tuple(v),
    do: @protocol.unify(Tuple.to_list(u), Tuple.to_list(v), s)
  def unify(_u, _v, _s), do: nil

  def occurs_check(u, v, s) when is_tuple(v),
    do: @protocol.occurs_check(Tuple.to_list(u), Tuple.to_list(v), s)
  def occurs_check(_u, _v, _s), do: false

  def walk(v, f),
    do: v |> Tuple.to_list() |> Enum.map(f) |> List.to_tuple()

  def is_tree?(_), do: false
end

defimpl Kanren.Reifiable, for: MapSet do
  def reify(v, s), do: v |> Enum.to_list() |> @protocol.reify(s)

  def unify(u, %MapSet{} = v, s),
    do: @protocol.unify(Enum.to_list(u), Enum.to_list(v), s)
  def unify(_u, _v, _s), do: nil

  def occurs_check(u, %MapSet{} = v, s),
    do: @protocol.occurs_check(Enum.to_list(u), Enum.to_list(v), s)
  def occurs_check(_u, _v, _s), do: false

  def walk(v, f),
    do: v |> Enum.to_list() |> Enum.map(f) |> Enum.into(%MapSet{})

  def is_tree?(_), do: false
end

# TODO: implement reifiable maps
defimpl Kanren.Reifiable, for: Map do
  def reify(_v, s), do: s

  def unify(_u, %{} = _v, _s), do: nil
  def unify(_u, _v, _s), do: nil

  def occurs_check(_u, %{} = _v, _s), do: false
  def occurs_check(_u, _v, _s), do: false

  def walk(v, f), do: v |> Enum.map(f) |> Enum.into(%{})

  def is_tree?(_), do: false
end

defimpl Kanren.Reifiable, for: Any do
  def reify(nil, s), do: s
  def reify(%{__struct__: _} = v, s),
    do: v |> Map.from_struct() |> @protocol.reify(s)

  def unify(%{__struct__: m} = u, %{__struct__: m} = v, s),
    do: @protocol.unify(Map.from_struct(u), Map.from_struct(v), s)
  def unify(u, u, s), do: s
  def unify(_u, _v, _s), do: nil

  def occurs_check(%{__struct__: m} = u, %{__struct__: m} = v, s),
    do: @protocol.occurs_check(Map.from_struct(u), Map.from_struct(v), s)
  def occurs_check(_u, _v, _s), do: false

  def walk(%{__struct__: m} = v, f),
    do: v |> Map.from_struct() |> @protocol.walk(f) |> then(&struct(m, &1))
  def walk(v, f), do: f.(v)

  def is_tree?(_), do: false
end

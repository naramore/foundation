defmodule Digraph do
  @moduledoc """
  `:digraph` but using `Struct`s instead of `:ets`.
  """
  use Boundary, top_level?: true, deps: [], exports: [Edge, Vertex]

  alias Digraph.{Edge, Vertex}

  @behaviour Access

  defstruct vertices: %{},
            edges: %{},
            options: [],
            vertex_id: 0,
            edge_id: 0

  @type t :: %__MODULE__{
          vertices: %{optional(Vertex.id()) => Vertex.t()},
          edges: %{optional(Edge.id()) => Edge.t()},
          options: [opt],
          vertex_id: non_neg_integer,
          edge_id: non_neg_integer
        }

  @type digraph :: :digraph.graph()
  @type opt :: :digraph.d_type()
  @type label :: :digraph.label()

  @impl Access
  def fetch(graph, key) do
    Map.fetch(graph, key)
  end

  @impl Access
  def get_and_update(graph, key, fun) do
    Map.get_and_update(graph, key, fun)
  end

  @impl Access
  def pop(graph, key) do
    Map.pop(graph, key)
  end

  @doc """
  See `:digraph.new/1`
  """
  @spec new([opt]) :: t
  def new(opts \\ []) do
    %__MODULE__{options: opts}
  end

  @doc """
  Create a `t:t/0` from a `t:digraph/0`.
  """
  @spec from_digraph(digraph()) :: t
  def from_digraph(dg) do
    {options, _} = Keyword.split(:digraph.info(dg), [:cyclicity, :protection])

    %__MODULE__{
      vertices:
        dg
        |> :digraph.vertices()
        |> Enum.map(&{elem(&1, 0), Vertex.from_digraph(dg, &1)})
        |> Enum.into(%{}),
      edges:
        dg
        |> :digraph.edges()
        |> Enum.map(&{elem(&1, 0), Edge.from_digraph(dg, &1)})
        |> Enum.into(%{}),
      options: Keyword.values(options)
    }
  end

  @doc """
  Create a `t:digraph/0` from a `t:t/0`.
  """
  @spec to_digraph(t) :: digraph()
  def to_digraph(graph) do
    dg = :digraph.new(graph.options)
    _ = Enum.each(graph.vertices, fn {_, v} -> Vertex.to_digraph(dg, v) end)
    _ = Enum.each(graph.edges, fn {_, e} -> Edge.to_digraph(dg, e) end)

    dg
  end

  @doc """
  See `:digraph.add_vertex/1`
  """
  @spec add_vertex(t) :: {Vertex.id(), t}
  def add_vertex(graph) do
    {id, graph} = next_id(graph, :vertex)
    add_vertex(graph, id)
  end

  @doc """
  See `:digraph.add_vertex/2`
  """
  @spec add_vertex(t, Vertex.id()) :: {Vertex.id(), t}
  def add_vertex(graph, id), do: add_vertex(graph, id, [])

  @doc """
  See `:digraph.add_vertex/3`
  """
  @spec add_vertex(t, Vertex.id(), label) :: {Vertex.id(), t}
  def add_vertex(graph, id, label) do
    {id, put_in(graph, [:vertices, id], Vertex.new(id, label))}
  end

  @doc """
  Similar to `add_vertex/3` but does not require specifying the vertex id.
  """
  @spec add_next_vertex(t, label) :: {Vertex.id(), t}
  def add_next_vertex(graph, label) do
    {id, graph} = next_id(graph, :vertex)
    add_vertex(graph, id, label)
  end

  @doc """
  See `:digraph.add_edge/3`
  """
  @spec add_edge(t, Vertex.id(), Vertex.id()) :: {:ok, {Edge.t(), t}} | {:error, reason :: term}
  def add_edge(graph, v1, v2), do: add_edge(graph, v1, v2, [])

  @doc """
  See `:digraph.add_edge/4`
  """
  @spec add_edge(t, Vertex.id(), Vertex.id(), label) ::
          {:ok, {Edge.t(), t}} | {:error, reason :: term}
  def add_edge(graph, v1, v2, label) do
    {id, graph} = next_id(graph, :edge)
    add_edge(graph, id, v1, v2, label)
  end

  @doc """
  See `:digraph.add_edge/5`
  """
  @spec add_edge(t, Edge.id(), Vertex.id(), Vertex.id(), label) ::
          {:ok, {Edge.t(), t}} | {:error, reason :: term}
  def add_edge(graph, id, v1, v2, label) do
    with {_, true} <- {:v1, v1 in Map.keys(graph.vertices)},
         {_, true} <- {:v2, v2 in Map.keys(graph.vertices)} do
      {:ok, {id, put_in(graph, [:edges, id], Edge.new(id, v1, v2, label))}}
    else
      {:v1, false} -> {:error, {:bad_vertex, v1}}
      {:v2, false} -> {:error, {:bad_vertex, v2}}
    end
  end

  @doc """
  See `:digraph.del_edge/2`
  """
  @spec del_edge(t, Edge.id()) :: t
  def del_edge(graph, edge) do
    Map.update!(graph, :edges, &Map.delete(&1, edge))
  end

  @doc """
  See `:digraph.del_edges/2`
  """
  @spec del_edges(t, [Edge.id()]) :: t
  def del_edges(graph, edges) do
    Enum.reduce(edges, graph, &del_edge(&2, &1))
  end

  @doc """
  See `:digraph.del_vertex/2`
  """
  @spec del_vertex(t, Vertex.id()) :: t
  def del_vertex(graph, vertex) do
    graph
    |> del_edges(Enum.map(edges(graph, vertex), & &1.id))
    |> Map.update!(:vertices, &Map.delete(&1, vertex))
  end

  @doc """
  See `:digraph.del_vertices/2`
  """
  @spec del_vertices(t, [Vertex.id()]) :: t
  def del_vertices(graph, vertices) do
    Enum.reduce(vertices, graph, &del_vertex(&2, &1))
  end

  @doc """
  See `:digraph.vertex/2`
  """
  @spec vertex(t, Vertex.id()) :: Vertex.t() | nil
  def vertex(graph, vertex) do
    Map.get(graph.vertices, vertex)
  end

  @doc """
  See `:digraph.edge/2`
  """
  @spec edge(t, Edge.id()) :: Edge.t() | nil
  def edge(graph, edge) do
    Map.get(graph.edges, edge)
  end

  @doc """
  See `:digraph.vertices/1`
  """
  @spec vertices(t) :: [Vertex.t()]
  def vertices(graph) do
    Map.values(graph.vertices)
  end

  @doc """
  See `:digraph.edges/1`
  """
  @spec edges(t) :: [Edge.t()]
  def edges(graph) do
    Map.values(graph.edges)
  end

  @doc """
  See `:digraph.edges/2`
  """
  @spec edges(t, Vertex.id()) :: [Edge.t()]
  def edges(graph, vertex) do
    graph
    |> edges()
    |> Enum.filter(fn
      %{v1: ^vertex} -> true
      %{v2: ^vertex} -> true
      _ -> false
    end)
  end

  @doc """
  See `:digraph.in_edges/2`
  """
  @spec in_edges(t, Vertex.id()) :: [Edge.t()]
  def in_edges(graph, vertex) do
    graph
    |> edges()
    |> Enum.filter(fn
      %{v2: ^vertex} -> true
      _ -> false
    end)
  end

  @doc """
  See `:digraph.out_edges/2`
  """
  @spec out_edges(t, Vertex.id()) :: [Edge.t()]
  def out_edges(graph, vertex) do
    graph
    |> edges()
    |> Enum.filter(fn
      %{v1: ^vertex} -> true
      _ -> false
    end)
  end

  @doc """
  See `:digraph.in_neighbours/2`
  """
  @spec in_neighbours(t, Vertex.id()) :: [Vertex.t()]
  def in_neighbours(graph, vertex) do
    graph
    |> in_edges(vertex)
    |> Enum.map(&vertex(graph, Map.get(&1, :v1)))
    |> Enum.reject(&is_nil/1)
  end

  @doc """
  See `:digraph.out_neighbours/2`
  """
  @spec out_neighbours(t, Vertex.id()) :: [Vertex.t()]
  def out_neighbours(graph, vertex) do
    graph
    |> out_edges(vertex)
    |> Enum.map(&vertex(graph, Map.get(&1, :v2)))
    |> Enum.reject(&is_nil/1)
  end

  @doc """
  See `:digraph.no_vertices/1`
  """
  @spec no_vertices(t) :: non_neg_integer()
  def no_vertices(graph) do
    Enum.count(graph.vertices)
  end

  @doc """
  See `:digraph.no_edges/1`
  """
  @spec no_edges(t) :: non_neg_integer()
  def no_edges(graph) do
    Enum.count(graph.edges)
  end

  @doc """
  See `:digraph.in_degree/2`
  """
  @spec in_degree(t, Vertex.id()) :: non_neg_integer()
  def in_degree(graph, vertex) do
    graph
    |> in_edges(vertex)
    |> Enum.count()
  end

  @doc """
  See `:digraph.out_degree/2`
  """
  @spec out_degree(t, Vertex.id()) :: non_neg_integer()
  def out_degree(graph, vertex) do
    graph
    |> out_edges(vertex)
    |> Enum.count()
  end

  @spec next_id(t, :vertex | :edge) :: {term, t}
  defp next_id(graph, :vertex) do
    {[:"$v" | graph.vertex_id], Map.update!(graph, :vertex_id, &(&1 + 1))}
  end

  defp next_id(graph, :edge) do
    {[:"$e" | graph.edge_id], Map.update!(graph, :edge_id, &(&1 + 1))}
  end

  defmodule Vertex do
    @moduledoc false

    alias Digraph

    @behaviour Access

    defstruct id: nil,
              label: nil

    @type t :: %__MODULE__{
            id: id,
            label: Digraph.label()
          }

    @type id :: :digraph.vertex()

    @impl Access
    def fetch(vertex, key) do
      Map.fetch(vertex, key)
    end

    @impl Access
    def get_and_update(vertex, key, fun) do
      Map.get_and_update(vertex, key, fun)
    end

    @impl Access
    def pop(vertex, key) do
      Map.pop(vertex, key)
    end

    @doc false
    @spec new(id, Digraph.label()) :: t
    def new(id, label \\ nil) do
      %__MODULE__{id: id, label: label}
    end

    @doc false
    @spec from_digraph(:digraph.graph(), id) :: t | nil
    def from_digraph(dg, vertex) do
      case :digraph.vertex(dg, vertex) do
        {id, label} -> new(id, label)
        _ -> nil
      end
    end

    @doc false
    @spec to_digraph(:digraph.graph(), t) :: :digraph.graph()
    def to_digraph(dg, vertex) do
      _ = :digraph.add_vertex(dg, vertex.id, vertex.label)
      dg
    end
  end

  defmodule Edge do
    @moduledoc false

    alias Digraph
    alias Digraph.Vertex

    @behaviour Access

    defstruct id: nil,
              v1: nil,
              v2: nil,
              label: nil

    @type t :: %__MODULE__{
            id: id,
            v1: Vertex.id(),
            v2: Vertex.id(),
            label: Digraph.label()
          }

    @type id :: :digraph.edge()

    @impl Access
    def fetch(edge, key) do
      Map.fetch(edge, key)
    end

    @impl Access
    def get_and_update(edge, key, fun) do
      Map.get_and_update(edge, key, fun)
    end

    @impl Access
    def pop(edge, key) do
      Map.pop(edge, key)
    end

    @doc false
    @spec new(id, Vertex.id(), Vertex.id(), Digraph.label()) :: t
    def new(id, v1, v2, label \\ nil) do
      %__MODULE__{id: id, v1: v1, v2: v2, label: label}
    end

    @doc false
    @spec from_digraph(:digraph.graph(), id) :: t | nil
    def from_digraph(dg, edge) do
      case :digraph.edge(dg, edge) do
        {id, v1, v2, label} -> new(id, v1, v2, label)
        _ -> nil
      end
    end

    @doc false
    @spec to_digraph(:digraph.graph(), t) :: :digraph.graph()
    def to_digraph(dg, edge) do
      _ = :digraph.add_edge(dg, edge.id, edge.v1, edge.v2, edge.label)
      dg
    end
  end
end

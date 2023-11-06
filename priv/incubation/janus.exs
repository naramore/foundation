defprotocol EQL.AST do
  @moduledoc false

  @spec to_expr(t) :: term
  def to_expr(ast)

  @spec get_key(t) :: EQL.AST.Prop.expr() | nil
  def get_key(ast)
end

defmodule EQL.Expression do
  @moduledoc false

  alias EQL.AST

  @callback to_ast(term) :: AST.t() | nil

  @spec to_ast([module] | module, term) :: AST.t() | nil
  def to_ast([], _), do: nil

  def to_ast([module | t], term) do
    case to_ast(module, term) do
      nil -> to_ast(t, term)
      otherwise -> otherwise
    end
  end

  def to_ast(module, term) do
    module.to_ast(term)
  end
end

defmodule EQL do
  @moduledoc """
  EDN Query Language (`EQL`) is a reimplementation of
  [EQL](https://github.com/edn-query-language/eql) in Elixir.

  Unlike Clojure, Elixir does not possess an [Extensible Data
  Notation (EDN)](https://github.com/edn-format/edn) equivalent
  at the time of this writing.

  ## Query / Transaction

  An `EQL` transaction is represented by a `List`.

  A transaction that only contains reads is commonly called a query,
  but notice that at the syntax level, it has no difference.

  ### Examples

  An empty transaction / query:

    iex> empty_txn = []
    iex> EQL.to_ast(empty_txn)
    %EQL.AST.Query{children: []}

  ## Properties

  Properties are expressed as Elixir `{module, atom}` tuples;
  they express the property been requested.

  ### Examples

    iex> query = [{Album, :name}, {Album, :year}]
    iex> EQL.to_ast(query)
    %EQL.AST.Query{children: [%EQL.AST.Prop{module: Album, key: :name},
                              %EQL.AST.Prop{module: Album, key: :year}]}

  ## Joins

  Joins are used to describe nesting in the request transaction.
  They are represented as Elixir `Map`s, always with a single entry,
  the key is the property to join on, and the value is a sub-query to run.

  ### Examples

  Simple join

    iex> join = [%{{Favorite, :albums} => [{Album, :name}, {Album, :year}]}]
    iex> EQL.to_ast(join)
    %EQL.AST.Query{children: [
      %EQL.AST.Join{
        key: %EQL.AST.Prop{module: Favorite, key: :albums},
        query: %EQL.AST.Query{children: [
          %EQL.AST.Prop{module: Album, key: :name},
          %EQL.AST.Prop{module: Album, key: :year}
        ]}
      }
    ]}

  Nested joins

    iex> join = [%{{Favorite, :albums} => [
    ...>   {Album, :name},
    ...>   {Album, :year},
    ...>   %{{Album, :tracks} => [
    ...>     {Track, :name},
    ...>     {Track, :duration}
    ...>   ]}
    ...> ]}]
    iex> EQL.to_ast(join)
    %EQL.AST.Query{children: [
      %EQL.AST.Join{
        key: %EQL.AST.Prop{module: Favorite, key: :albums},
        query: %EQL.AST.Query{children: [
          %EQL.AST.Prop{module: Album, key: :name},
          %EQL.AST.Prop{module: Album, key: :year},
          %EQL.AST.Join{
            key: %EQL.AST.Prop{module: Album, key: :tracks},
            query: %EQL.AST.Query{children: [
              %EQL.AST.Prop{module: Track, key: :name},
              %EQL.AST.Prop{module: Track, key: :duration}
            ]}
          }
        ]}
      }
    ]}

  ## Idents

  Idents are represented by a `{property, value}` tuple, where the first
  is a property and the second can be anything. They are like (lookup refs
  on Datomic)[https://blog.datomic.com/2014/02/datomic-lookup-refs.html], in
  general, they can provide an address-like thing, and their use and
  semantics might vary from system to system.

  ### Examples

      iex> ident = [{{Customer, :id}, 123}]
      iex> EQL.to_ast(ident)
      %EQL.AST.Query{children: [
        %EQL.AST.Ident{
          key: %EQL.AST.Prop{module: Customer, key: :id},
          value: 123
        }
      ]}

  It’s common to use an ident as a join key to start a query for some entity:

    iex> join = [%{{{Customer, :id}, 123} => [
    ...>   {Customer, :name},
    ...>   {Customer, :email}
    ...> ]}]
    iex> EQL.to_ast(join)
    %EQL.AST.Query{children: [
      %EQL.AST.Join{
        key: %EQL.AST.Ident{
          key: %EQL.AST.Prop{module: Customer, key: :id},
          value: 123
        },
        query: %EQL.AST.Query{children: [
          %EQL.AST.Prop{module: Customer, key: :name},
          %EQL.AST.Prop{module: Customer, key: :email}
        ]}
      }
    ]}

  ## Parameters

  `EQL` properties, joins, and idents have support for parametrization.
  This allows the query to provide an extra dimension of information about the requested data.
  A parameter is expressed by wrapping the thing with an improper list, like so:

    iex> # without params
    iex> EQL.to_ast([{Foo, :bar}])
    %EQL.AST.Query{children: [%EQL.AST.Prop{module: Foo, key: :bar}]}

    iex> # with params
    iex> EQL.to_ast([[{Foo, :bar} | %{with: "params"}]])
    %EQL.AST.Query{children: [
      %EQL.AST.Params{
        expr: %EQL.AST.Prop{module: Foo, key: :bar},
        params: %{with: "params"}
      }
    ]}

  Params must always be maps, the map values can be anything.

  ## Unions

  In `EQL` unions are used to specify polymorphic requirements.
  For example, a messaging app may have a single list, and each
  entry on the chat log can be a message, audio or photo, each
  having its own query requirement.

      # message query
      [{Message, :id}, {Message, :text}, {Chat.Entry, :timestamp}]

      # audio query
      [{Audio, :id}, {Audio, :url}, {Audio, :duration}, {Chat.Entry, :timestamp}]

      # photo query
      [{Photo, :id}, {Photo, :url}, {Photo, :width}, {Photo, :height}, {Chat.Entry, :timestamp}]

      # list query
      [%{{Chat, :entries} => "???"}]

  Now to express this polymorphic requirement as the sub-query of the
  `{Chat, :entries}` list we can use a map as the join value, and each
  entry on this map represents a possible sub-query.

  The way this information is used is up to the parser implementation;
  EQL only defines the syntax.

  ### Examples

    iex> union = [%{{Chat, :entries} => %{
    ...>   {Message, :id} => [{Message, :id}, {Message, :text}, {Chat.Entry, :timestamp}],
    ...>   {Audio, :id} => [{Audio, :id}, {Audio, :url}, {Audio, :duration}, {Chat.Entry, :timestamp}],
    ...>   {Photo, :id} => [{Photo, :id}, {Photo, :url}, {Photo, :width}, {Photo, :height}, {Chat.Entry, :timestamp}]
    ...> }}]
    iex> EQL.to_ast(union)
    %EQL.AST.Query{children: [
      %EQL.AST.Join{
        key: %EQL.AST.Prop{module: Chat, key: :entries},
        query: %EQL.AST.Union{children: [
          %EQL.AST.Union.Entry{
            key: %EQL.AST.Prop{module: Audio, key: :id},
            query: %EQL.AST.Query{children: [
              %EQL.AST.Prop{module: Audio, key: :id},
              %EQL.AST.Prop{module: Audio, key: :url},
              %EQL.AST.Prop{module: Audio, key: :duration},
              %EQL.AST.Prop{module: Chat.Entry, key: :timestamp},
            ]}
          },
          %EQL.AST.Union.Entry{
            key: %EQL.AST.Prop{module: Message, key: :id},
            query: %EQL.AST.Query{children: [
              %EQL.AST.Prop{module: Message, key: :id},
              %EQL.AST.Prop{module: Message, key: :text},
              %EQL.AST.Prop{module: Chat.Entry, key: :timestamp},
            ]}
          },
          %EQL.AST.Union.Entry{
            key: %EQL.AST.Prop{module: Photo, key: :id},
            query: %EQL.AST.Query{children: [
              %EQL.AST.Prop{module: Photo, key: :id},
              %EQL.AST.Prop{module: Photo, key: :url},
              %EQL.AST.Prop{module: Photo, key: :width},
              %EQL.AST.Prop{module: Photo, key: :height},
              %EQL.AST.Prop{module: Chat.Entry, key: :timestamp},
            ]}
          },
        ]}
      }
    ]}

  ## Mutations

  Mutations in `EQL` are used to represent operation calls,
  usually to do something that will cause a side effect.
  Mutations as data allows that operation to behave much like event sourcing,
  and can be transparently applied locally, across a network, onto an event bus, etc.

  ### Examples

  A mutation is represented by a list of two elements; the first is the symbol that names the mutation, and the second is a map with input data.

    iex> mutation = [{Call.Some, :operation, ["input"]}]
    iex> EQL.to_ast(mutation)
    %EQL.AST.Query{children: [
      %EQL.AST.Mutation{
        module: Call.Some,
        fun: :operation,
        args: ["input"]
      }
    ]}
  """
  alias EQL.AST

  @type query :: AST.Query.expr()

  @spec to_ast(term) :: AST.t() | nil
  def to_ast(term) do
    AST.Query.to_ast(term)
  end

  @spec to_expr(AST.t()) :: term
  def to_expr(ast) do
    AST.to_expr(ast)
  end

  @spec get_key(AST.t()) :: AST.Prop.expr() | nil
  def get_key(ast) do
    AST.get_key(ast)
  end

  defdelegate union_to_query(union), to: AST.Union, as: :to_query

  defdelegate query(children), to: AST.Query, as: :new

  defdelegate prop(module, key), to: AST.Prop, as: :new

  defdelegate ident(prop, value), to: AST.Ident, as: :new

  defdelegate join(prop, query), to: AST.Join, as: :new

  defdelegate union(children), to: AST.Union, as: :new

  defdelegate union_entry(key, query), to: AST.Union.Entry, as: :new

  defdelegate params(expr, params), to: AST.Params, as: :new

  defdelegate mutation(module, fun, args), to: AST.Mutation, as: :new
end

defmodule Digraph do
  @moduledoc false
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

  @spec new([opt]) :: t
  def new(opts \\ []) do
    %__MODULE__{options: opts}
  end

  @spec from_digraph(:digraph.graph()) :: t
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

  @spec to_digraph(t) :: :digraph.graph()
  def to_digraph(graph) do
    dg = :digraph.new(graph.options)
    _ = Enum.each(graph.vertices, fn {_, v} -> Vertex.to_digraph(dg, v) end)
    _ = Enum.each(graph.edges, fn {_, e} -> Edge.to_digraph(dg, e) end)

    dg
  end

  @spec add_vertex(t) :: {Vertex.id(), t}
  def add_vertex(graph) do
    {id, graph} = next_id(graph, :vertex)
    add_vertex(graph, id)
  end

  @spec add_vertex(t, Vertex.id()) :: {Vertex.id(), t}
  def add_vertex(graph, id), do: add_vertex(graph, id, [])

  @spec add_vertex(t, Vertex.id(), label) :: {Vertex.id(), t}
  def add_vertex(graph, id, label) do
    {id, put_in(graph, [:vertices, id], Vertex.new(id, label))}
  end

  @spec add_next_vertex(t, label) :: {Vertex.id(), t}
  def add_next_vertex(graph, label) do
    {id, graph} = next_id(graph, :vertex)
    add_vertex(graph, id, label)
  end

  @spec add_edge(t, Vertex.id(), Vertex.id()) :: {:ok, {Edge.t(), t}} | {:error, reason :: term}
  def add_edge(graph, v1, v2), do: add_edge(graph, v1, v2, [])

  @spec add_edge(t, Vertex.id(), Vertex.id(), label) ::
          {:ok, {Edge.t(), t}} | {:error, reason :: term}
  def add_edge(graph, v1, v2, label) do
    {id, graph} = next_id(graph, :edge)
    add_edge(graph, id, v1, v2, label)
  end

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

  @spec del_edge(t, Edge.id()) :: t
  def del_edge(graph, edge) do
    Map.update!(graph, :edges, &Map.delete(&1, edge))
  end

  @spec del_edges(t, [Edge.id()]) :: t
  def del_edges(graph, edges) do
    Enum.reduce(edges, graph, &del_edge(&2, &1))
  end

  @spec del_vertex(t, Vertex.id()) :: t
  def del_vertex(graph, vertex) do
    graph
    |> del_edges(Enum.map(edges(graph, vertex), & &1.id))
    |> Map.update!(:vertices, &Map.delete(&1, vertex))
  end

  @spec del_vertices(t, [Vertex.id()]) :: t
  def del_vertices(graph, vertices) do
    Enum.reduce(vertices, graph, &del_vertex(&2, &1))
  end

  @spec vertex(t, Vertex.id()) :: Vertex.t() | nil
  def vertex(graph, vertex) do
    Map.get(graph.vertices, vertex)
  end

  @spec edge(t, Edge.id()) :: Edge.t() | nil
  def edge(graph, edge) do
    Map.get(graph.edges, edge)
  end

  @spec vertices(t) :: [Vertex.t()]
  def vertices(graph) do
    Map.values(graph.vertices)
  end

  @spec edges(t) :: [Edge.t()]
  def edges(graph) do
    Map.values(graph.edges)
  end

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

  @spec in_edges(t, Vertex.id()) :: [Edge.t()]
  def in_edges(graph, vertex) do
    graph
    |> edges()
    |> Enum.filter(fn
      %{v2: ^vertex} -> true
      _ -> false
    end)
  end

  @spec out_edges(t, Vertex.id()) :: [Edge.t()]
  def out_edges(graph, vertex) do
    graph
    |> edges()
    |> Enum.filter(fn
      %{v1: ^vertex} -> true
      _ -> false
    end)
  end

  @spec in_neighbours(t, Vertex.id()) :: [Vertex.t()]
  def in_neighbours(graph, vertex) do
    graph
    |> in_edges(vertex)
    |> Enum.map(&vertex(graph, Map.get(&1, :v1)))
    |> Enum.reject(&is_nil/1)
  end

  @spec out_neighbours(t, Vertex.id()) :: [Vertex.t()]
  def out_neighbours(graph, vertex) do
    graph
    |> out_edges(vertex)
    |> Enum.map(&vertex(graph, Map.get(&1, :v2)))
    |> Enum.reject(&is_nil/1)
  end

  @spec no_vertices(t) :: non_neg_integer()
  def no_vertices(graph) do
    Enum.count(graph.vertices)
  end

  @spec no_edges(t) :: non_neg_integer()
  def no_edges(graph) do
    Enum.count(graph.edges)
  end

  @spec in_degree(t, Vertex.id()) :: non_neg_integer()
  def in_degree(graph, vertex) do
    graph
    |> in_edges(vertex)
    |> Enum.count()
  end

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

    @spec new(id, Digraph.label()) :: t
    def new(id, label \\ nil) do
      %__MODULE__{id: id, label: label}
    end

    @spec from_digraph(:digraph.graph(), id) :: t | nil
    def from_digraph(dg, vertex) do
      case :digraph.vertex(dg, vertex) do
        {id, label} -> new(id, label)
        _ -> nil
      end
    end

    @spec to_digraph(:digraph.graph(), t) :: :digraph.graph()
    def to_digraph(dg, vertex) do
      _ = :digraph.add_vertex(dg, vertex.id, vertex.label)
      dg
    end
  end

  defmodule Edge do
    @moduledoc false

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

    @spec new(id, Vertex.id(), Vertex.id(), Digraph.label()) :: t
    def new(id, v1, v2, label \\ nil) do
      %__MODULE__{id: id, v1: v1, v2: v2, label: label}
    end

    @spec from_digraph(:digraph.graph(), id) :: t | nil
    def from_digraph(dg, edge) do
      case :digraph.edge(dg, edge) do
        {id, v1, v2, label} -> new(id, v1, v2, label)
        _ -> nil
      end
    end

    @spec to_digraph(:digraph.graph(), t) :: :digraph.graph()
    def to_digraph(dg, edge) do
      _ = :digraph.add_edge(dg, edge.id, edge.v1, edge.v2, edge.label)
      dg
    end
  end
end

defmodule Janus do
  @moduledoc fallse

  @type env :: map
  @type attr :: EQL.AST.Prop.expr()
  @type shape_descriptor(x) :: %{optional(x) => shape_descriptor(x)}
  @type shape_descriptor :: shape_descriptor(attr)
  @type response_form(x) :: %{optional(x) => [response_form(x)] | any}
  @type response_form :: response_form(attr)
end

defmodule Janus.Utils do
  @moduledoc false
  import Kernel, except: [inspect: 1, inspect: 2]
  require Logger

  @spec inspect_opts() :: keyword
  def inspect_opts,
    do: [pretty: true, structs: false, limit: :infinity, printable_limit: :infinity]

  @spec inspect(term, keyword) :: Inspect.t()
  def inspect(term, opts \\ inspect_opts()) do
    Kernel.inspect(term, opts)
  end

  @spec to_shape_descriptor([x]) :: Janus.shape_descriptor(x) when x: any
  def to_shape_descriptor(terms) do
    terms
    |> Enum.zip(Stream.repeatedly(fn -> %{} end))
    |> Enum.into(%{})
  end

  @spec trace(Janus.Graph.edge(), term) :: :ok
  def trace({_, i, o, %{id: id}}, term) do
    Logger.debug(
      "#{Kernel.inspect(short_form(id))}[#{Kernel.inspect(short_form(o))} <- #{
        Kernel.inspect(short_form(i))
      }] :: #{Kernel.inspect(short_form(term))}"
    )
  end

  def trace(edge, term) do
    Logger.warn(
      "unknown edge format: #{Kernel.inspect(edge)} :: #{Kernel.inspect(short_form(term))}"
    )
  end

  def short_form(x) when is_map(x),
    do: Enum.map(x, fn {k, v} -> {short_form(k), short_form(v)} end) |> Enum.into(%{})

  def short_form([]), do: []
  def short_form([h | t]), do: [short_form(h) | short_form(t)]
  def short_form({:and, ids}), do: {:and, short_form(ids)}
  def short_form({:and, ids, id}), do: {:and, short_form(ids), short_form(id)}
  def short_form({_, id}), do: id
  def short_form(id), do: id

  @spec cond_pipe(x, boolean | (x -> boolean), (x -> y)) :: y when x: any, y: any
  def cond_pipe(x, true, fun), do: fun.(x)
  def cond_pipe(x, false, _), do: x

  def cond_pipe(x, pred, fun) do
    if pred.(x) do
      fun.(x)
    else
      x
    end
  end

  @spec deep_merge(map, map) :: map
  def deep_merge(m1, m2) do
    Map.merge(m1, m2, &deep_merge/3)
  end

  @spec deep_merge(term, map, map) :: map
  defp deep_merge(_key, m1, m2), do: deep_merge(m1, m2)
end

defmodule Janus.Resolver do
  @moduledoc false

  # TODO: batching (implemented using a more generic transform middleware / interceptor model?)
  #                (or maybe, default to batching? and have single requests be a corner case?)
  # TODO: add params to resolver? (if a resolver can specified required params,
  #                                then this would potentially change planning)
  # TODO: async (perhaps supports the return of a Task.t or [Task.t], or something similar?)
  # TODO: defresolver macro
  # TODO: resolver helpers
  #         - alias_resolver
  #         - equivalence_resolver
  #         - constantly_resolver
  #         - single_attr_resolver
  #         - single_attr_with_env_resolver
  #         - static_table_resolver
  #         - attribute_map_resolver
  #         - attribute_table_resolver
  # TODO: file resolver?
  # TODO: `:ets` resolver?

  defstruct id: nil,
            input: [],
            output: [],
            resolve: nil

  @type t :: %__MODULE__{
          id: id,
          input: input,
          output: output,
          resolve: resolve_fun
        }

  @type id :: Janus.attr()
  @type input :: [Janus.attr()]
  @type output :: [output_attr, ...] | composed_output
  @type output_attr :: Janus.attr() | composed_output
  @type composed_output :: %{required(Janus.attr()) => output}

  @type input_map :: %{optional(Janus.attr()) => any}
  @type output_map :: %{required(Janus.attr()) => [output_map] | any}
  @type resolve_fun :: (input_map, Janus.env() -> output_map)

  @spec new(id, input, output, resolve_fun, keyword) :: t
  def new(id, input, output, resolve, _opts \\ []) do
    %__MODULE__{
      id: id,
      input: input,
      output: output,
      resolve: resolve
    }
  end
end

defmodule Janus.Graph do
  @moduledoc false

  alias Janus.Resolver
  alias EQL.AST.{Ident, Join, Params, Prop, Query, Union, Union.Entry}

  @behaviour Access

  # TODO: add checks to Janus.Graph.from_resolvers/1 for resolver uniqueness
  # TODO: add resolver function to nodes? (or just keep the resolver index on the graph?)

  defstruct unreachable: MapSet.new([]),
            attr_trail: [],
            dg: nil

  @type t :: %__MODULE__{
          unreachable: MapSet.t(node_id),
          attr_trail: [node_id],
          dg: :digraph.graph()
        }

  @type node_id :: Janus.attr() | [Janus.attr()]
  @type vertex :: :digraph.vertex()
  @type edge :: :digraph.edge()
  @type acc :: term
  @type reachability :: :reachable | :unreachable | :found
  @type walker(x) :: (type :: term, x, t, acc -> {reachability, t, acc})
  @type walker :: walker(edge)
  @type depth :: non_neg_integer
  @type label_tuple(id) :: {id, depth, [Janus.attr()], Janus.attr() | nil, leaf? :: boolean}
  @type label(id) :: %{
          required(:id) => id,
          required(:depth) => depth,
          required(:path) => [Janus.attr()],
          required(:union_key) => Janus.attr() | nil,
          required(:leaf?) => boolean
        }
  @type ast_type ::
          :pre_walk
          | :ident
          | {:pre_walk | :post_walk, :params}
          | {:pre_subquery | :post_subquery, Resolver.id()}
          | {:recursion, depth :: timeout}
          | {:post_walk, reachability}
  @type attr_type :: :pre_walk | :cyclic | {:post_walk, reachability}

  @callback ast_walker(ast_type, EQL.AST.t(), t, acc) ::
              {:cont | :skip, t, acc} | {:error, reason :: term}
  @callback attr_walker(attr_type, edge, t, acc) :: {reachability, t, acc}

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

  @spec new([Resolver.t()]) :: t
  def new(resolvers) do
    %__MODULE__{
      dg: from_resolvers(resolvers)
    }
  end

  @spec new([Resolver.t()], :digraph.graph()) :: t
  def new(resolvers, dg) do
    %__MODULE__{
      dg: from_resolvers(resolvers, dg)
    }
  end

  @spec reset(t) :: t
  def reset(graph) do
    %{graph | attr_trail: [], unreachable: MapSet.new([])}
  end

  @spec ast_walker(module, ast_type, EQL.AST.t(), t, acc) ::
          {:cont | :skip, t, acc} | {:error, reason :: term}
  def ast_walker(module, type, ast, graph, acc) do
    module.ast_walker(type, ast, graph, acc)
  end

  @spec attr_walker(module, attr_type, edge, t, acc) :: {reachability, t, acc}
  def attr_walker(module, type, edge, graph, acc) do
    module.attr_walker(type, edge, graph, acc)
  end

  @spec walk_ast(t, EQL.AST.t(), acc, module) :: {:ok, {t, acc}} | {:error, reason :: term}
  def walk_ast(graph, %Prop{} = prop, acc, module) do
    with {:cont, graph, acc} <- ast_walker(module, :pre_walk, prop, graph, acc),
         {r, g, a} <- walk_attr(graph, EQL.get_key(prop), acc, module),
         {_, g, a} <- ast_walker(module, {:post_walk, r}, prop, g, a) do
      {:ok, {g, a}}
    else
      {:skip, graph, acc} -> {:ok, {graph, acc}}
      {:error, reason} -> {:error, reason}
    end
  end

  def walk_ast(graph, %Ident{} = ident, acc, module) do
    case ast_walker(module, :ident, ident, acc, graph) do
      {:error, reason} -> {:error, reason}
      {_, graph, acc} -> {:ok, {graph, acc}}
    end
  end

  def walk_ast(graph, %Params{expr: expr} = params, acc, module) do
    with {:cont, graph, acc} <- ast_walker(module, {:pre_walk, :params}, params, graph, acc),
         {:ok, {graph, acc}} <- walk_ast(graph, expr, acc, module),
         {_, graph, acc} <- ast_walker(module, {:post_walk, :params}, params, graph, acc) do
      {:ok, {graph, acc}}
    else
      {:skip, graph, acc} -> {:ok, {graph, acc}}
      {:error, reason} -> {:error, reason}
    end
  end

  def walk_ast(graph, %Join{key: key, query: %Query{} = q} = join, acc, module) do
    case walk_ast(graph, key, acc, module) do
      {:error, reason} ->
        {:error, reason}

      {:ok, {graph, acc}} ->
        graph
        |> subqueries(key)
        |> Enum.reduce_while({:ok, {graph, acc}}, fn
          _, {:error, reason} ->
            {:halt, {:error, reason}}

          r, {:ok, {g, a}} ->
            with {:cont, g, a} <- ast_walker(module, {:pre_subquery, r}, join, g, a),
                 {:ok, {g, a}} <- walk_ast(g, q, a, module),
                 {_, g, a} <- ast_walker(module, {:post_subquery, r}, join, g, a) do
              {:cont, {:ok, {g, a}}}
            else
              {:skip, graph, acc} -> {:cont, {:ok, {graph, acc}}}
              {:error, reason} -> {:halt, {:error, reason}}
            end
        end)
    end
  end

  def walk_ast(graph, %Join{key: key} = join, acc, module)
      when is_integer(key) or key == :infinity do
    with {:ok, {graph, acc}} <- walk_ast(graph, key, acc, module),
         {:cont, graph, acc} <- ast_walker(module, {:recursion, key}, join, graph, acc) do
      {:ok, {graph, acc}}
    else
      {:skip, graph, acc} -> {:ok, {graph, acc}}
      {:error, reason} -> {:error, reason}
    end
  end

  def walk_ast(graph, %Union{} = union, acc, module) do
    walk_ast(graph, EQL.union_to_query(union), acc, module)
  end

  def walk_ast(graph, %Query{children: children}, acc, module) do
    Enum.reduce_while(children, {:ok, {graph, acc}}, fn
      _, {:error, reason} -> {:halt, {:error, reason}}
      ast, {:ok, {g, a}} -> {:cont, walk_ast(g, ast, a, module)}
    end)
  end

  def walk_ast(_graph, ast, _acc, _module) do
    {:error, {:invalid_ast, ast}}
  end

  @spec walk_attr(t, node_id, acc, module) :: {reachability, t, acc}
  def walk_attr(graph, node_ids, acc, module) when is_list(node_ids) do
    Enum.reduce_while(node_ids, {:reachable, graph, acc}, fn
      _, {:unreachable, g, a} ->
        {:halt, {:unreachable, g, a}}

      nid, {r, g, a} when r in [:reachable, :found] ->
        fake_edge = {nil, nid, node_ids, %{id: nil}}
        walk_attr_reducer(fake_edge, {:reachable, g, a}, module)
    end)
  end

  def walk_attr(graph, node_id, acc, module) do
    graph.dg
    |> :digraph.in_edges(node_id)
    |> Enum.map(&:digraph.edge(graph.dg, &1))
    |> Enum.filter(&direct_edge?/1)
    |> Enum.reduce_while({:unreachable, graph, acc}, &walk_attr_reducer(&1, &2, module))
  end

  @spec walk_attr_reducer(edge, {reachability, t, acc}, module) ::
          {:cont | :halt, {reachability, t, acc}}
  defp walk_attr_reducer(edge, {r, graph, acc}, module) do
    cond do
      unreachable?(graph, edge) ->
        {:unreachable, graph, acc}

      cyclic?(graph, edge) ->
        {:unreachable, graph, acc}
        |> wrap_walker(edge, module, :cyclic)
        |> update_unreachable(edge, graph, r)

      true ->
        {graph, acc}
        |> continue_walk_attr(edge, module)
        |> update_unreachable(edge, graph, r)
    end
    |> cont_or_halt(edge)
  end

  @spec continue_walk_attr({t, acc}, edge, module) :: {reachability, t, acc}
  defp continue_walk_attr({g, a}, {_, i, o, _} = edge, module) do
    case attr_walker(module, :pre_walk, edge, g, a) do
      {:reachable, graph, acc} ->
        graph
        |> Map.update!(:attr_trail, &[o | &1])
        |> walk_attr(i, acc, module)
        |> wrap_walker(edge, module)

      otherwise ->
        otherwise
    end
  end

  @spec wrap_walker({reachability, t, acc}, edge, module) :: {reachability, t, acc}
  defp wrap_walker(rga, edge, module, type \\ nil)

  defp wrap_walker({reach, graph, acc}, edge, module, nil) do
    attr_walker(module, {:post_walk, reach}, edge, graph, acc)
  end

  defp wrap_walker({_, graph, acc}, edge, module, type) do
    attr_walker(module, type, edge, graph, acc)
  end

  @spec cont_or_halt({reachability, t, acc}, edge) :: {:cont | :halt, {reachability, t, acc}}
  defp cont_or_halt({:unreachable, _, _} = rga, {_, _, o, _}) when is_list(o), do: {:halt, rga}
  defp cont_or_halt(rga, _edge), do: {:cont, rga}

  @spec update_unreachable({reachability, t, acc}, edge, t, reachability) ::
          {reachability, t, acc}
  defp update_unreachable({:unreachable, g, a}, {_, i, o, _}, graph, _) when is_list(o) do
    {:unreachable, mark_unreachable(graph, g, i), a}
  end

  defp update_unreachable({:unreachable, g, a}, {_, i, _, _}, graph, r) do
    {r, mark_unreachable(graph, g, i), a}
  end

  defp update_unreachable({r, g, a}, _edge, graph, _) do
    {r, %{graph | unreachable: g.unreachable}, a}
  end

  @spec mark_unreachable(t, t, Janus.attr()) :: t
  defp mark_unreachable(previous_graph, graph, attr) do
    if attr in graph.attr_trail do
      %{previous_graph | unreachable: graph.unreachable}
    else
      %{previous_graph | unreachable: MapSet.put(graph.unreachable, attr)}
    end
  end

  @spec direct_edge?(edge) :: boolean
  defp direct_edge?({_, _, _, %{depth: 0}}), do: true
  defp direct_edge?(_), do: false

  @spec unreachable?(t, edge) :: boolean
  defp unreachable?(graph, {_, i, _, _}) when is_list(i) do
    i in graph.unreachable or Enum.any?(i, &(&1 in graph.unreachable))
  end

  defp unreachable?(graph, {_, i, _, _}) do
    i in graph.unreachable
  end

  @spec cyclic?(t, edge) :: boolean
  defp cyclic?(graph, {_, i, _, _}), do: i in graph.attr_trail

  @spec from_resolvers([Resolver.t()]) :: :digraph.graph()
  defp from_resolvers(resolvers) do
    from_resolvers(resolvers, :digraph.new([]))
  end

  @spec from_resolvers([Resolver.t()], :digraph.graph()) :: :digraph.graph()
  defp from_resolvers([], dg), do: dg

  defp from_resolvers([res | t], dg) do
    i = extract_input_name(res)
    labels = output_info(res)
    output = Enum.map(labels, &Map.get(&1, :id))
    _ = Enum.each([i | res.input], &:digraph.add_vertex(dg, &1))
    _ = Enum.each(output, &:digraph.add_vertex(dg, &1))
    _ = Enum.each(labels, &:digraph.add_edge(dg, i, &1.id, %{&1 | id: res.id}))

    from_resolvers(t, dg)
  end

  @spec extract_input_name(Resolver.t()) :: id | [id] when id: {module, atom}
  defp extract_input_name(%Resolver{input: []}), do: []
  defp extract_input_name(%Resolver{input: [id]}), do: id
  defp extract_input_name(%Resolver{input: [_ | _] = ids}), do: ids

  @spec output_info(Resolver.t()) :: [label(id)] when id: Resolver.id()
  defp output_info(resolver) do
    case EQL.to_ast(resolver.output) do
      nil ->
        []

      ast ->
        ast
        |> output_info(0, [], nil)
        |> Enum.map(fn {id, depth, path, union_key, leaf?} ->
          %{id: id, depth: depth, parent: path, union_key: union_key, leaf?: leaf?}
        end)
    end
  end

  @spec output_info([EQL.AST.t()] | EQL.AST.t(), depth, path, union_key) ::
          [{id, depth, path, union_key, leaf?}]
        when id: Janus.attr(), path: [id], union_key: id | nil, leaf?: boolean
  defp output_info([], _, _, _), do: []
  defp output_info([h | t], d, p, u), do: output_info(h, d, p, u) ++ output_info(t, d, p, u)
  defp output_info(%Prop{module: m, key: k}, d, p, u), do: [{{m, k}, d, p, u, true}]

  defp output_info(%Join{key: %Prop{module: m, key: k}, query: q}, d, p, u),
    do: [{{m, k}, d, p, u, false} | output_info(q, d + 1, [{m, k} | p], nil)]

  defp output_info(%Union{children: cs}, d, p, _), do: output_info(cs, d, p, nil)

  defp output_info(%Entry{key: %Prop{module: m, key: k}, query: q}, d, p, _),
    do: output_info(q, d, p, {m, k})

  defp output_info(%Query{children: cs}, d, p, _), do: output_info(cs, d, p, nil)

  @spec subqueries(t, Prop.t()) :: [Resolver.id()]
  defp subqueries(graph, prop) do
    graph.dg
    |> :digraph.in_edges(EQL.get_key(prop))
    |> Enum.map(fn
      {_, _, _, %{id: id}} -> id
      _ -> nil
    end)
    |> Enum.reject(&is_nil/1)
    |> Enum.dedup()
  end

  @doc false
  defmacro __using__(_opts) do
    quote do
      @behaviour Janus.Graph

      @spec walk_ast(Janus.Graph.t(), EQL.AST.t(), Janus.Graph.acc()) ::
              {:ok, {Janus.Graph.t(), Janus.Graph.acc()}} | {:error, reason :: term}
      def walk_ast(graph, ast, acc) do
        Janus.Graph.walk_ast(graph, ast, acc, __MODULE__)
      end

      @spec walk_attr(Janus.Graph.t(), Janus.Graph.node_id(), Janus.Graph.acc()) ::
              {Janus.Graph.reachability(), Janus.Graph.t(), Janus.Graph.acc()}
      def walk_attr(graph, node_id, acc) do
        Janus.Graph.walk_attr(graph, node_id, acc, __MODULE__)
      end
    end
  end
end

defmodule Janus.Plan do
  @moduledoc """
  Core logic for the plan / run graph construction.

  There are 4 different node types in the run graph:
  - resolver-nodes: represents a `Janus.Resolver` to be executed
  - or-nodes: represents a set of paths of which at least ONE must be traversed
  - and-nodes: represents a set of paths that ALL must be traversed
  - join-nodes: used with and-nodes as the 'end' of the and-branches

  The primary entrypoint is `follow/5` which takes a 'path' and the current run
  graph, and walks the path either verifying that the correct node next in the
  path exists, otherwise creating it.

  ## Run Graph Construction Pseudo-Code

  - if the next node is `nil` or `[]`:
    - locate existing node / create new node
    - follow
  - if the next node matches the next node in the path:
    - follow the path
  - if the next node is a resolver node:
    - inject an or-node b/t current and next node
    - locate existing resolver / create new resolver
    - follow
  - if the current node is an and-node:
    - look through the and-node branches for the specific logical branch:
      - if found and matches the next node in the path:
        - follow
      - if found (no match):
        - inject an or-node at path root
        - follow
      - otherwise:
        - locate existing node / create new node
        - follow
  - if the current node is an or-node:
    - look through the or-node branches for a match with next node in path:
      - if found:
        - follow
      - otherwise:
        - locate existing node / create new node
        - follow
  - if the next node is an or-node:
    - continue to follow

  ## Notes

  The main reason `Digraph` exists is that since this functionality is intended
  to be run within the resolution of a client request-response cycle on a
  webserver, the 'overhead' of using `:digraph`, which is `:ets`-based could
  potentially have concurrency / ETS table implications at extreme concurrency.
  (not sure how legitimate this concern is, but if it turns out to be unfounded,
  replacing `Digraph` with `:digraph` would only be a moderate refactor of this
  module and `Janus.Planner`)
  """

  alias Digraph.{Edge, Vertex}
  alias Janus.{Graph, Resolver, Utils}
  require Logger

  @type t :: Digraph.t()
  @type path :: [landmark]
  @type resolver_node :: Resolver.id()
  @type and_node :: {:and, all_branches :: [Janus.attr()], this_branch :: Janus.attr()}
  @type join_node :: {:join, all_branches :: [Janus.attr()]}
  @type landmark :: resolver_node | and_node | join_node
  @type scope :: [Janus.attr() | {:and, [Janus.attr()]}]
  @type type :: :resolver | :and | :or | :join
  @type node_label :: %{
          required(:scope) => scope,
          required(:attrs) => [Janus.attr()],
          required(:type) => type,
          optional(:resolver) => Resolver.id(),
          optional(:params) => map,
          optional(:input) => [Janus.attr()],
          optional(:output) => Janus.shape_descriptor()
        }
  @type vertex :: [Vertex.t()] | Vertex.t() | nil
  @type full_path :: {landmark, Graph.node_id(), Janus.attr(), scope}

  @root_start [:root | :start]
  @root_end [:root | :end]

  @spec root_start() :: Vertex.id()
  def root_start, do: @root_start

  @spec root_end() :: Vertex.id()
  def root_end, do: @root_end

  @spec init_graph(t) :: t
  def init_graph(graph) do
    {_, graph} = create_node(graph, :and, [], [], id: @root_start)
    {_, graph} = create_node(graph, :join, [], [], id: @root_end)
    graph
  end

  @spec create_attr_root_node(t, Janus.attr()) ::
          {:ok, {Vertex.id(), t}} | {:error, reason :: term}
  def create_attr_root_node(graph, attr) do
    case lookup(graph, :or, [], attr) do
      nil ->
        {attr_root, graph} = create_node(graph, :or, [], attr, id: attr)

        case connect(graph, @root_start, attr_root, attr) do
          {:ok, {_, g}} -> {:ok, {attr_root, g}}
          {:error, reason} -> {:error, reason}
        end

      id ->
        {:ok, {id, graph}}
    end
  end

  @spec find_attr_resolvers(t, Janus.attr()) :: [Vertex.t()]
  def find_attr_resolvers(graph, attr) do
    graph
    |> Digraph.vertices()
    |> Enum.filter(fn
      %{label: %{type: :resolver, output: out}} -> attr in Map.keys(out)
      _ -> false
    end)
  end

  @spec follow(t, vertex, [full_path], Janus.attr(), keyword) ::
          {:ok, t} | {:error, reason :: term}
  def follow(graph, current, path, attr, opts \\ [])

  def follow(_graph, nil, _path, _attr, _opts) do
    {:error, :invalid_current}
  end

  def follow(graph, %Vertex{} = current, [], _attr, _opts) do
    graph
    |> Digraph.out_neighbours(current.id)
    |> Enum.filter(&match?(%{id: @root_end}, &1))
    |> case do
      [] ->
        case Digraph.add_edge(graph, current.id, @root_end) do
          {:error, reason} -> {:error, reason}
          {:ok, {_, graph}} -> {:ok, graph}
        end

      _ ->
        {:ok, graph}
    end
  end

  def follow(graph, %Vertex{} = current, path, attr, opts) do
    follow_impl(graph, current, next(graph, current, attr), path, attr, opts)
  end

  def follow(graph, id, path, attr, opts) do
    follow(graph, Digraph.vertex(graph, id), path, attr, opts)
  end

  @spec follow_impl(t, Vertex.t(), vertex, [full_path], Janus.attr(), keyword) ::
          {:ok, t} | {:error, reason :: term}
  defp follow_impl(graph, current, [], path, attr, opts) do
    follow_impl(graph, current, nil, path, attr, opts)
  end

  defp follow_impl(graph, current, nil, [{l, i, o, s} | t], attr, opts) do
    graph
    |> locate_and_connect(current.id, l, s, attr, push(opts, i, o))
    |> Rails.bind(fn {v, g} -> follow(g, v, t, attr, opts) end)
  end

  defp follow_impl(graph, current, next, [{l, _, _, _} | t] = path, attr, opts) do
    if match_path_next?(next, l) do
      follow(graph, next, t, attr, opts)
    else
      case {current, next} do
        {_, %{label: %{resolver: _}}} ->
          inject_new_resolver(graph, current, next, path, attr, opts)

        {%{label: %{type: :and}}, branches} ->
          locate_and_branch(graph, current, branches, path, attr, opts)

        {%{label: %{type: :or}}, branches} ->
          locate_or_branch(graph, current, branches, path, attr, opts)

        {_, %{label: %{type: :or}}} ->
          follow(graph, next, path, attr, opts)

        _ ->
          {:error, :unexpected_form}
      end
    end
  end

  @spec inject_new_resolver(t, Vertex.t(), Vertex.t(), [full_path, ...], Janus.attr(), keyword) ::
          {:ok, t} | {:error, reason :: term}
  defp inject_new_resolver(graph, current, next, [{l, i, o, s} | t], attr, opts) do
    graph
    |> inject_or_node(current, next)
    |> Rails.bind(fn {oid, g} ->
      locate_and_connect(g, oid, l, s, attr, push(opts, i, o))
    end)
    |> Rails.bind(fn {v, g} -> follow(g, v, t, attr, opts) end)
  end

  @spec locate_and_branch(t, Vertex.t(), vertex, [full_path, ...], Janus.attr(), keyword) ::
          {:ok, t} | {:error, reason :: term}
  defp locate_and_branch(graph, current, branches, [{l, i, o, s} | t] = p, attr, opts) do
    case Enum.find(branches, &match?({%{label: %{scope: [b | _]}}, [b | _]}, {&1, s})) do
      nil ->
        graph
        |> locate_and_connect(current.id, l, s, attr, push(opts, i, o))
        |> Rails.bind(fn {v, g} -> follow(g, v, t, attr, opts) end)

      branch ->
        if match_path_next?(branch, elem(hd(t), 0)) do
          follow(graph, branch, t, attr, opts)
        else
          graph
          |> inject_or_node(current, branch, s)
          |> Rails.bind(fn {v, g} -> follow(g, v, p, attr, opts) end)
        end
    end
  end

  @spec locate_or_branch(t, Vertex.t(), vertex, [full_path, ...], Janus.attr(), keyword) ::
          {:ok, t} | {:error, reason :: term}
  defp locate_or_branch(graph, current, branches, [{l, i, o, s} | t], attr, opts) do
    case Enum.find(branches, &match_path_next?(&1, l)) do
      nil ->
        graph
        |> locate_and_connect(current.id, l, s, attr, push(opts, i, o))
        |> Rails.bind(fn {v, g} -> follow(g, v, t, attr, opts) end)

      branch ->
        follow(graph, branch, t, attr, opts)
    end
  end

  @spec push(keyword, Graph.node_id(), Janus.attr()) :: keyword
  defp push(opts, input, output) do
    input = if is_list(input), do: input, else: [input]
    Keyword.merge(opts, input: input, output: %{output => %{}})
  end

  @spec next(t, Vertex.t(), Janus.attr()) :: [Vertex.t()] | Vertex.t() | nil
  defp next(graph, %{label: %{type: type}} = current, attr) do
    graph
    |> Digraph.out_neighbours(current.id)
    |> Enum.filter(&attr_match?(&1, attr))
    |> Utils.cond_pipe(type in [:resolver, :join], &List.first/1)
  end

  @spec inject_or_node(t, Vertex.t(), Vertex.t()) ::
          {:ok, {Vertex.id(), t}} | {:error, reason :: term}
  defp inject_or_node(graph, current, next) do
    inject_or_node(graph, current, next, current.label.scope)
  end

  @spec inject_or_node(t, Vertex.t(), Vertex.t(), scope) ::
          {:ok, {Vertex.id(), t}} | {:error, reason :: term}
  defp inject_or_node(graph, %{label: %{attrs: [attr]}} = current, next, scope) do
    inject_node(graph, current, next, &create_node(&1, :or, scope, attr))
  end

  defp inject_or_node(graph, %{label: %{attrs: attrs}} = current, next, scope) do
    inject_node(graph, current, next, &create_node(&1, :or, scope, attrs))
  end

  @spec inject_node(
          t,
          Vertex.t(),
          Vertex.t(),
          (t -> {Vertex.id(), t})
        ) :: {:ok, {Vertex.id(), t}} | {:error, reason :: term}
  defp inject_node(graph, current, next, fun) do
    with %{id: id, label: label} <-
           Enum.find(Digraph.out_edges(graph, current.id), &(&1.v2 == next.id)),
         {injected_id, g} <- fun.(graph),
         {:ok, {_, g}} <- Digraph.add_edge(g, current.id, injected_id, label),
         {:ok, {_, g}} <- Digraph.add_edge(g, injected_id, next.id) do
      {:ok, {injected_id, Digraph.del_edge(g, id)}}
    else
      _ -> {:error, {:edge_not_found, current.id, next.id}}
    end
  end

  @spec locate_and_connect(
          t,
          Vertex.id(),
          landmark,
          scope,
          [Janus.attr()] | Janus.attr(),
          keyword
        ) :: {:ok, {Vertex.id(), t}} | {:error, reason :: term}
  defp locate_and_connect(graph, previous_id, landmark, scope, attr, opts) do
    with {next_id, graph} <- find_or_create(graph, landmark, scope, attr, opts),
         {:ok, {_, graph}} <- connect(graph, previous_id, next_id, attr) do
      {:ok, {next_id, graph}}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @spec connect(t, Vertex.id(), Vertex.id(), [Janus.attr()] | Janus.attr() | nil) ::
          {:ok, {Edge.t(), t}} | {:error, reason :: term}
  def connect(graph, v1, v2, attr \\ nil) do
    graph
    |> Digraph.out_neighbours(v1)
    |> Enum.find(fn v -> v.id == v2 and attr_match?(v, attr) end)
    |> case do
      nil -> Digraph.add_edge(graph, v1, v2)
      id -> {:ok, {id, graph}}
    end
  end

  @spec find_or_create(t, landmark | :or, scope, [Janus.attr()] | Janus.attr(), keyword) ::
          {Vertex.id(), t}
  defp find_or_create(graph, landmark, scope, attr, opts) do
    case lookup(graph, landmark, scope) do
      nil ->
        create_node(graph, landmark, scope, attr, opts)

      id ->
        graph
        |> update_attrs(id, attr)
        |> update_via_opts(id, opts, :params)
        |> update_via_opts(id, opts, :output)
        |> (&{id, &1}).()
    end
  end

  @spec update_attrs(t, Vertex.id(), [Janus.attr()] | Janus.attr()) :: t
  defp update_attrs(graph, id, attr) do
    update_in(graph, [:vertices, id, :label, :attrs], fn
      nil ->
        if is_list(attr), do: attr, else: [attr]

      attrs ->
        if is_list(attr) do
          Enum.dedup(:lists.reverse(attr, attrs))
        else
          Enum.dedup([attr | attrs])
        end
    end)
  end

  @spec update_via_opts(t, Vertex.t() | Vertex.id(), keyword, atom) :: t
  defp update_via_opts(graph, %Vertex{} = vertex, opts, key) do
    if Keyword.has_key?(opts, key) do
      update_in(graph, [:vertices, vertex.id, :label, key], fn
        nil ->
          Keyword.get(opts, key)

        params ->
          Utils.deep_merge(params, Keyword.get(opts, key))
      end)
    else
      graph
    end
  end

  defp update_via_opts(graph, id, opts, key) do
    case Digraph.vertex(graph, id) do
      nil -> graph
      vertex -> update_via_opts(graph, vertex, opts, key)
    end
  end

  @spec lookup(t, landmark | :or, scope, [Janus.attr()] | Janus.attr() | nil) ::
          Vertex.id() | nil
  defp lookup(graph, landmark, scope, attr \\ nil) do
    graph
    |> Digraph.vertices()
    |> Enum.find_value(fn vertex ->
      if vertex_match?(vertex, landmark, scope, attr) do
        vertex.id
      end
    end)
  end

  @spec vertex_match?(Vertex.t(), landmark | :or, scope, [Janus.attr()] | Janus.attr() | nil) ::
          boolean
  defp vertex_match?(v, id, scope, attr) do
    type_match?(v, id) and scope_match?(v, scope) and attr_match?(v, attr)
  end

  @spec type_match?(Vertex.t(), landmark | :or) :: boolean
  defp type_match?(%{label: %{type: :and}}, {:and, _, _}), do: true
  defp type_match?(%{label: %{type: :join}}, {:join, _}), do: true
  defp type_match?(%{label: %{type: :or}}, :or), do: true
  defp type_match?(%{label: %{resolver: id}}, id), do: true
  defp type_match?(_, _), do: false

  @spec scope_match?(Vertex.t(), scope) :: boolean
  defp scope_match?(%{label: %{scope: scope}}, scope), do: true
  defp scope_match?(_, _), do: false

  @spec attr_match?(Vertex.t(), [Janus.attr()] | Janus.attr() | nil) :: boolean
  defp attr_match?(_, nil), do: true
  defp attr_match?(%{label: %{attrs: []}}, []), do: true
  defp attr_match?(%{label: %{attrs: attrs}}, [_ | _] = attr), do: Enum.all?(attr, &(&1 in attrs))
  defp attr_match?(%{label: %{attrs: attrs}}, attr), do: attr in attrs
  defp attr_match?(_, _), do: false

  @spec create_node(
          t,
          landmark | :or | :and | :join,
          scope,
          [Janus.attr()] | Janus.attr(),
          keyword
        ) :: {Vertex.id(), t}
  defp create_node(graph, landmark, scope, attr, opts \\ []) do
    label = create_node_label(landmark, scope, attr, opts)

    if Keyword.has_key?(opts, :id) do
      Digraph.add_vertex(graph, Keyword.get(opts, :id), label)
    else
      Digraph.add_next_vertex(graph, label)
    end
  end

  @spec create_node_label(
          landmark | :or | :and | :join,
          scope,
          [Janus.attr()] | Janus.attr(),
          keyword
        ) :: node_label
  defp create_node_label(landmark, scope, attr, opts) do
    %{
      scope: scope,
      attrs: if(is_list(attr), do: attr, else: [attr]),
      type: get_node_type(landmark),
      resolver: landmark,
      input: Keyword.get(opts, :input),
      output: Keyword.get(opts, :output)
    }
    |> case do
      %{type: :resolver} = label -> label
      label -> Map.drop(label, [:resolver, :input, :output])
    end
  end

  @spec get_node_type(landmark | :or | :and | :join) :: type
  defp get_node_type({:and, _, _}), do: :and
  defp get_node_type({:join, _}), do: :join
  defp get_node_type(type) when type in [:or, :and, :join], do: type
  defp get_node_type(_), do: :resolver

  @spec match_path_next?(Vertex.t(), landmark) :: boolean
  defp match_path_next?(%{label: %{type: :and, scope: [{:and, bs} | _]}}, {:and, bs, _}), do: true
  defp match_path_next?(%{label: %{type: :join, scope: [{:and, bs} | _]}}, {:join, bs}), do: true
  defp match_path_next?(%{label: %{resolver: id}}, id), do: true
  defp match_path_next?(_, _), do: false
end

defmodule Janus.Planner do
  @moduledoc """
  Core logic for resolver path accumulation.

  Responsible for interfacing with `Janus.Graph` via its `ast_walker/4` and
  `attr_walker/4` callbacks and constructing the paths needed by
  `Janus.Plan` to create the run graph to be used by `Janus.Processor`.

  The basic logic of `attr_walker/4` is to accumulate the path/trail if the
  'end' hasn't been found yet, but if the 'end' has been found then add that
  path to the struct and update the `Digraph` `:plan` via
  `Janus.Plan.follow/5`.
  """

  use Janus.Graph
  alias Digraph.Vertex
  alias EQL.AST.{Ident, Join, Params, Prop}
  alias Janus.{Graph, Plan, Utils}
  require Logger

  @behaviour Access

  # TODO: eventually remove paths?

  defstruct available_data: %{},
            resolver_trail: [],
            paths: %{},
            plan: nil,
            current_attr: nil,
            params: nil

  @type t :: %__MODULE__{
          available_data: Janus.shape_descriptor(),
          resolver_trail: [{Plan.landmark(), Graph.node_id(), Janus.attr()}],
          paths: %{optional(Janus.attr()) => [{Plan.landmark(), Graph.node_id(), Janus.attr()}]},
          plan: Digraph.t(),
          current_attr: Janus.attr() | nil,
          params: map | nil
        }

  @type graph :: %{optional(Vertex.t()) => graph}

  @impl Access
  def fetch(planner, key) do
    Map.fetch(planner, key)
  end

  @impl Access
  def get_and_update(planner, key, fun) do
    Map.get_and_update(planner, key, fun)
  end

  @impl Access
  def pop(planner, key) do
    Map.pop(planner, key)
  end

  @impl Graph
  def ast_walker(:pre_walk, %Prop{} = prop, graph, %{plan: plan} = planner) do
    with attr when not is_nil(attr) <- EQL.get_key(prop),
         {attr, [_ | _] = vs} <- {attr, Plan.find_attr_resolvers(plan, attr)},
         {:ok, {_, plan}} <- Plan.create_attr_root_node(plan, attr),
         {:ok, planner} <- backtrack_and_mark(%{planner | plan: plan}, attr, vs) do
      {:skip, graph, planner}
    else
      {:error, reason} ->
        {:error, reason}

      nil ->
        {:error, {:invalid_prop, prop}}

      {attr, []} ->
        if attr in Map.keys(planner.available_data) do
          {:skip, graph, planner}
        else
          {:cont, graph, reset(planner, attr)}
        end
    end
  end

  def ast_walker({:post_walk, :unreachable}, %Prop{} = prop, _graph, _planner) do
    {:error, {:unreachable_prop, prop}}
  end

  def ast_walker({:post_walk, _}, %Prop{}, graph, planner) do
    {:cont, graph, reset(planner)}
  end

  def ast_walker({:pre_walk, :params}, %Params{params: params}, graph, planner) do
    {:cont, graph, %{planner | params: params}}
  end

  def ast_walker({:post_walk, :params}, %Params{}, graph, planner) do
    {:cont, graph, %{planner | params: nil}}
  end

  def ast_walker(:ident, %Ident{}, _graph, _planner) do
    # TODO: add to (subquery?) available_data?
    {:error, :not_implemented}
  end

  def ast_walker({:pre_subquery, _rid}, %Join{}, _graph, _planner) do
    # TODO: implement
    {:error, :not_implemented}
  end

  def ast_walker({:post_subquery, _rid}, %Join{}, _graph, _planner) do
    # TODO: implement
    {:error, :not_implemented}
  end

  def ast_walker({:recursion, _depth}, %Join{}, _graph, _planner) do
    # TODO: implement
    {:error, :not_implemented}
  end

  @impl Graph
  def attr_walker(:cyclic, _edge, graph, planner) do
    {:unreachable, graph, planner}
  end

  def attr_walker(:pre_walk, {_, i, o, %{id: nil}}, graph, planner) when is_list(o) do
    attr_walker(:pre_walk, {nil, i, o, %{id: {:and, o, i}}}, graph, planner)
  end

  def attr_walker(:pre_walk, {_, i, o, %{id: id}}, graph, planner) do
    if available?(planner, i) do
      {:found, graph, add_path(planner, id, i, o)}
    else
      {:reachable, graph, add_trail(planner, id, i, o)}
    end
  end

  def attr_walker({:post_walk, r}, _edge, graph, planner) do
    {r, graph, tail_trail(planner)}
  end

  @spec new([Janus.attr()]) :: t
  def new(attrs) do
    %__MODULE__{
      available_data: Utils.to_shape_descriptor(attrs),
      plan: Plan.init_graph(Digraph.new())
    }
  end

  @spec reset(t, Janus.attr() | nil) :: t
  def reset(plan, attr \\ nil)

  def reset(plan, nil) do
    %{plan | resolver_trail: [], current_attr: nil}
  end

  def reset(plan, attr) do
    %{plan | resolver_trail: [], current_attr: attr}
    |> update_in([:paths, attr], fn
      nil -> []
      x -> x
    end)
  end

  @spec show(Digraph.t(), Vertex.t() | Vertex.id()) :: graph | nil
  def show(graph, vertex_or_id \\ [:root | :start]) do
    case show_impl(graph, vertex_or_id) do
      {v, g} -> %{v => g}
      _ -> nil
    end
  end

  @spec show_impl(Digraph.t(), Vertex.t() | Vertex.id() | nil) :: {Vertex.t(), graph} | nil
  defp show_impl(_graph, nil), do: nil

  defp show_impl(graph, %Vertex{} = vertex) do
    graph
    |> Digraph.out_neighbours(vertex.id)
    |> Enum.map(&show_impl(graph, &1))
    |> Enum.reject(&is_nil/1)
    |> Enum.into(%{})
    |> (&{vertex, &1}).()
  end

  defp show_impl(graph, vertex_id) do
    show_impl(graph, Digraph.vertex(graph, vertex_id))
  end

  @spec available?(t, Graph.node_id()) :: boolean
  defp available?(_planner, []), do: true

  defp available?(planner, input) when is_list(input) do
    Enum.all?(input, &(&1 in Map.keys(planner.available_data)))
  end

  defp available?(planner, input) do
    input in Map.keys(planner.available_data)
  end

  @spec add_trail(t, Plan.landmark(), Graph.node_id(), Janus.attr()) :: t
  defp add_trail(planner, id, input, output) do
    Map.update!(planner, :resolver_trail, &[{id, input, output} | &1])
  end

  @spec tail_trail(t) :: t
  defp tail_trail(planner), do: Map.update!(planner, :resolver_trail, &tl/1)

  @spec add_path(t, Plan.landmark(), Graph.node_id(), Janus.attr(), keyword) :: t
  defp add_path(planner, id, input, output, opts \\ []) do
    planner
    |> update_in(
      [:paths, planner.current_attr],
      &[[{id, input, output} | planner.resolver_trail] | &1]
    )
    |> update_plan(update_opts(planner, opts))
    |> case do
      {:ok, planner} ->
        planner

      {:error, reason} ->
        raise %RuntimeError{message: "#{inspect(reason)}"}
    end
  end

  @spec update_opts(t, keyword) :: keyword
  defp update_opts(%{params: nil}, opts), do: opts
  defp update_opts(%{params: params}, opts), do: Keyword.put(opts, :params, params)

  @spec update_plan(t, keyword) :: {:ok, t} | {:error, reason :: term}
  defp update_plan(%{plan: plan, current_attr: attr} = planner, opts) do
    with [path | _] <- get_in(planner, [:paths, attr]),
         {:ok, {origin_id, plan}} <- Plan.create_attr_root_node(plan, attr),
         path = pre_process_path(path),
         {:ok, plan} <- Plan.follow(plan, origin_id, path, attr, opts) do
      {:ok, %{planner | plan: plan}}
    else
      {:error, reason} -> {:error, reason}
      _ -> {:error, :unknown}
    end
  end

  # NOTE: this 'path' is getting so complicated as is, convert to a struct?
  @spec pre_process_path([{Plan.landmark(), Graph.node_id(), Janus.attr()}]) :: [
          {Plan.landmark(), Graph.node_id(), Janus.attr(), Plan.scope()}
        ]
  defp pre_process_path(path) do
    {top_ands, new_path} =
      path
      |> :lists.reverse()
      |> Enum.reduce({[], []}, fn
        {{:and, bs, b}, _, _}, {t, []} ->
          {[{{:and, bs, b}, [], [], [{:and, bs}]} | t], [{{:join, bs}, bs, [], [b, {:and, bs}]}]}

        {{:and, bs, b}, _, _}, {t, [{_, _, _, s} | _] = p} ->
          {[{{:and, bs, b}, [], [], [{:and, bs} | s]} | t],
           [{{:join, bs}, bs, [], [b, {:and, bs} | s]} | p]}

        {id, i, o}, {t, []} ->
          {t, [{id, i, o, []}]}

        {id, i, o}, {t, [{_, _, _, s} | _] = p} ->
          {t, [{id, i, o, s} | p]}
      end)

    top_ands
    |> :lists.reverse(new_path)
    |> Enum.map(fn
      {{:join, _} = j, i, o, [_ | s]} -> {j, i, o, s}
      x -> x
    end)
  end

  @spec backtrack_and_mark(t, Janus.attr(), [Vertex.t()]) :: {:ok, t} | {:error, reason :: term}
  defp backtrack_and_mark(planner, attr, vertices) do
    case backtrack(planner, attr, vertices) do
      {:error, reason} ->
        {:error, reason}

      {:ok, planner} ->
        Enum.reduce(vertices, {:ok, planner}, &backtrack_and_marker(&1, &2, attr))
    end
  end

  @spec backtrack_and_marker(Vertex.t(), acc, Janus.attr()) :: acc
        when acc: {:ok, t} | {:error, reason :: term}
  defp backtrack_and_marker(vertex, planner_result, attr)
  defp backtrack_and_marker(_, {:error, reason}, _), do: {:error, reason}

  defp backtrack_and_marker(vertex, {:ok, planner}, attr) do
    planner
    |> update_output(vertex.id, attr)
    |> connect_to_root_end(vertex.id)
  end

  @spec update_output(t, Vertex.id(), Janus.attr()) :: t
  defp update_output(planner, vertex_id, attr) do
    update_in(
      planner,
      [:plan, :vertices, vertex_id, :label, :output],
      &Utils.deep_merge(&1, %{attr => %{}})
    )
  end

  @spec connect_to_root_end(t, Vertex.id()) :: {:ok, t} | {:error, reason :: term}
  defp connect_to_root_end(planner, vertex_id) do
    case Plan.connect(planner.plan, vertex_id, Plan.root_end()) do
      {:error, reason} -> {:error, reason}
      {:ok, {_, plan}} -> {:ok, %{planner | plan: plan}}
    end
  end

  @spec backtrack(t, Janus.attr(), [Vertex.t()]) :: {:ok, t} | {:error, reason :: term}
  defp backtrack(planner, attr, vertices) do
    Enum.reduce(vertices, {:ok, planner}, &backtracker(&1, &2, attr))
  end

  @spec backtracker(Vertex.t(), acc, Janus.attr()) :: acc
        when acc: {:ok, t} | {:error, reason :: term}
  defp backtracker(vertex, planner_result, attr)
  defp backtracker(_, {:error, reason}, _), do: {:error, reason}

  defp backtracker(vertex, {:ok, planner}, attr) do
    planner = update_attrs(planner, vertex.id, attr)

    planner.plan
    |> Digraph.in_neighbours(vertex.id)
    |> Enum.filter(&match?(%{id: [:"$v" | _]}, &1))
    |> case do
      [] ->
        case Plan.connect(planner.plan, attr, vertex.id) do
          {:ok, {_, plan}} -> {:ok, %{planner | plan: plan}}
          {:error, reason} -> {:error, reason}
        end

      next ->
        backtrack(planner, attr, next)
    end
  end

  @spec update_attrs(t, Vertex.id(), Janus.attr()) :: t
  defp update_attrs(planner, vertex_id, attr) do
    update_in(planner, [:plan, :vertices, vertex_id, :label, :attrs], fn
      nil ->
        [attr]

      attrs ->
        if attr in attrs do
          attrs
        else
          [attr | attrs]
        end
    end)
  end
end

defmodule Janus.Processor do
  @moduledoc false

  @type entity :: Janus.response_form()

  @callback process_ast(Janus.env(), EQL.AST.Query.t()) ::
              {:ok, entity} | {:error, reason :: term}

  @spec process_expr(module, Janus.env(), EQL.query()) :: {:ok, entity} | {:error, reason :: term}
  def process_expr(module, env, query) do
    case EQL.to_ast(query) do
      nil -> {:error, :invalid_ast}
      query -> module.process_ast(env, query)
    end
  end

  @doc false
  defmacro __using__(_opts) do
    quote do
      alias Janus.Processor
      @behaviour Processor

      @spec process_expr(Janus.env(), EQL.query()) ::
              {:ok, Processor.entity()} | {:error, reason :: term}
      def process_expr(env, query) do
        Processor.process_expr(__MODULE__, env, query)
      end
    end
  end
end

defmodule Janus.Processor.Serial do
  @moduledoc false

  use Janus.Processor

  @impl Processor
  def process_ast(_env, _ast) do
    {:error, :not_implemented}
  end

  # change env from map -> struct?
  #   graph: Janus.Graph.t
  #   resolvers: %{optional(Resolver.id) => Resolver.t}
  #   planner: Janus.Planner.t
  #   entity: Janus.Processor.entity
  #   context: map

  # graph = Janus.Graph.new(resolvers)
  # planner = Janus.Planner.new(available_data)
  # {:ok, {graph, planner}} = Janus.Planner.walk_ast(graph, query_ast, planner)

  # process_root()
  #   Enum.reduce(root.branches, entity, &process_node/2)

  # process_node(node, env)
  #   resolver -> process_resolver_node(node, env)
  #   and -> process_and_node(node, env)
  #   or -> process_or_node(node, env)
  #   join -> process_join_node(node, env)

  # process_resolver_node(node, env)
  #   if any outputs not in entity -> lookup_resolver -> execute -> merge output w/ entity -> process_next_node()
  #   otherwise -> process_next_node()

  # process_and_node(node, env)
  #   Enum.reduce(node.branches, env, &process_node/2)

  # process_or_node(node, env)
  #   recur until one succeeds (USE WEIGHT BEHAVIOUR HERE!)

  # process_join_node(node, env)
  #   root_end? -> noop
  #   once 1 of each attr of in_neighbours has succeeded -> process_next_node()
  #   otherwise -> noop
end

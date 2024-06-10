defprotocol Logic.Bindable do
  @moduledoc """
  """

  @type t(_a) :: t()

  @doc """
  """
  @spec next(t(a)) :: {a, t(a)} | nil when a: any()
  def next(a)

  @doc """
  """
  @spec interleave(t(a), t(a)) :: t(a) when a: any()
  def interleave(a, b)

  @doc """
  """
  @spec bind(t(a), (a -> t(b))) :: t(b) when a: any(), b: any()
  def bind(m, k)
end

defmodule Logic.Bind do
  @moduledoc """
  """
  alias Logic.Bind
  alias Logic.Bindable

  @type t(a) :: Bindable.t(a)
  @type t() :: t(any())

  @doc false
  defmacro __using__(_opts) do
    quote do
      require Logic.Bind
      import Logic.Bind
      alias Logic.Bind
    end
  end

  defdelegate interleave(a, b), to: Bindable
  defdelegate bind(m, k), to: Bindable

  @doc """
  """
  @spec interleave_many([t(a), ...]) :: t(a) when a: any()
  def interleave_many([m]), do: m
  def interleave_many([m | ms]), do: interleave(m, interleave_many(ms))

  @doc """
  """
  @spec bind_many(t(a), [(a -> t(a))]) :: t(a) when a: any()
  def bind_many(m, ks)
  def bind_many(m, []), do: m
  def bind_many(m, [k]), do: bind(m, k)
  def bind_many(m, [k | ks]), do: bind_many(bind(m, k), ks)

  defmacro interleave_many!([m]) do
    quote do: unquote(m)
  end
  defmacro interleave_many!([m | ms]) do
    quote do: Bind.interleave_many([unquote(m), unquote_splicing(ms)])
  end

  defmacro bind_many!(m, []) do
    quote do: unquote(m)
  end
  defmacro bind_many!(m, [k]) do
    quote do: Bind.bind(unquote(m), unquote(k))
  end
  defmacro bind_many!(m, [k | ks]) do
    quote do: Bind.bind_many!(Bind.bind(unquote(m), unquote(k)), unquote(ks))
  end
end

defmodule Logic do
  @moduledoc """
  """
  use Boundary, top_level?: true, deps: [], exports: [Bind]
  alias Logic.LazyTree

  @type t(a) :: LazyTree.t(a)

  @doc false
  defmacro __using__(_opts) do
    quote do
      use Logic.Bind
      require Logic
      import Logic
    end
  end

  defdelegate thunk(f), to: LazyTree
  defdelegate unit(a), to: LazyTree, as: :constant
  defdelegate mzero(), to: LazyTree, as: :empty
  # defdelegate interleave(a, b), to: Bind
  # defdelegate bind(m, k), to: Bind

  # @spec interleave_many([t(a), ...]) :: t(a) when a: any()
  # def interleave_many([m]), do: m
  # def interleave_many([m | ms]), do: interleave(m, thunk(fn -> interleave_many(ms) end))

  # @spec bind_many(t(a), [(a -> t(a))]) :: t(a) when a: any()
  # def bind_many(m, ks)
  # def bind_many(m, []), do: m
  # def bind_many(m, [k]), do: bind(m, k)
  # def bind_many(m, [k | ks]), do: bind_many(bind(m, k), ks)

  # defmacro interleave_many!([m]) do
  #   quote do: unquote(m)
  # end
  # defmacro interleave_many!([m | ms]) do
  #   quote do: Logic.interleave_many([unquote(m), unquote_splicing(ms)])
  # end

  # defmacro bind_many!(m, []) do
  #   quote do: unquote(m)
  # end
  # defmacro bind_many!(m, [k]) do
  #   quote do: Logic.bind(unquote(m), unquote(k))
  # end
  # defmacro bind_many!(m, [k | ks]) do
  #   quote do: Logic.bind_many!(Logic.bind(unquote(m), unquote(k)), unquote(ks))
  # end

  @doc """
  """
  @spec id(a) :: a when a: any()
  def id(a), do: a

  @doc """
  """
  @spec const(a, b) :: a when a: any(), b: any()
  def const(a, _b), do: a

  @doc """
  """
  @spec const(a) :: (b -> a) when a: any(), b: any()
  def const(a), do: &const(a, &1)

  @doc """
  """
  @spec head(a, [a]) :: [a] when a: any()
  def head(h, t), do: [h | t]

  @doc """
  """
  @spec head(a) :: ([a] -> [a]) when a: any()
  def head(h), do: &head(h, &1)

  @doc """
  """
  @spec comp(fun(), fun()) :: fun()
  # @spec comp((b -> c), (a -> b)) :: (a -> c)
  def comp(f, g) when is_function(f, 1) and is_function(g, 1), do: fn x -> x |> g.() |> f.() end
  # @spec comp((b, c -> d), (a -> b)) :: (a, c -> d)
  def comp(f, g) when is_function(f, 2) and is_function(g, 1), do: fn x, y -> x |> g.() |> f.(y) end
  # @spec comp((c -> d), (a, b -> c)) :: (a, b -> d)
  def comp(f, g) when is_function(f, 1) and is_function(g, 2), do: fn x, y -> x |> g.(y) |> f.() end
  # @spec comp((c, d -> e), (a, b -> c)) :: (a, b, d -> e)
  def comp(f, g) when is_function(f, 2) and is_function(g, 2), do: fn x, y, z -> x |> g.(y) |> f.(z) end
  # @spec comp((d -> e), (a, b, c -> d)) :: (a, b, c -> e)
  def comp(f, g) when is_function(f, 1) and is_function(g, 3), do: fn x, y, z -> x |> g.(y, z) |> f.() end
  # @spec comp((b, c, d -> e), (a -> b)) :: (a, c, d -> e)
  def comp(f, g) when is_function(f, 3) and is_function(g, 1), do: fn x, y, z -> x |> g.() |> f.(y, z) end
  # @spec comp((c, d, e -> f), (a, b -> c)) :: (a, b, d, e -> f)
  def comp(f, g) when is_function(f, 3) and is_function(g, 2), do: fn w, x, y, z -> w |> g.(x) |> f.(y, z) end
  # @spec comp((d, e -> f), (a, b, c -> d)) :: (a, b, c, e -> f)
  def comp(f, g) when is_function(f, 2) and is_function(g, 3), do: fn w, x, y, z -> w |> g.(x, y) |> f.(z) end
  # @spec comp((d, e, f -> g), (a, b, c -> d)) :: (a, b, c, e, f -> g)
  def comp(f, g) when is_function(f, 3) and is_function(g, 3), do: fn v, w, x, y, z -> v |> g.(w, x) |> f.(y, z) end

  @doc """
  """
  @spec ap((a -> b), a) :: b when a: any(), b: any()
  def ap(f, a), do: f.(a)

  @doc """
  """
  @spec ap((a -> b)) :: (a -> b) when a: any(), b: any()
  def ap(f), do: &ap(f, &1)

  defmodule LazyTree do
    defstruct next: nil,
              thunk: nil
    @type t(a) :: %__MODULE__{
      next: maybe(a),
      thunk: thunk(t(a)) | nil
    }

    @type maybe(a) :: just(a) | nothing()
    @type nothing() :: nil
    @type just(a) :: {:just, a}
    @type thunk(a) :: (-> a)

    @spec new(maybe(a), thunk(a) | nil) :: t(a) when a: any()
    def new(next, thunk \\ nil) do
      %__MODULE__{
        next: next,
        thunk: thunk
      }
    end

    @spec just(a) :: just(a) when a: any()
    def just(a), do: {:just, a}

    @spec nothing() :: nothing()
    def nothing, do: nil

    @spec empty() :: t(a) when a: any()
    def empty, do: new(nil)

    @spec constant(a) :: t(a) when a: any()
    def constant(a), do: new(just(a))

    @spec thunk(thunk(a) | nil) :: t(a) when a: any()
    def thunk(f)
    def thunk(nil), do: empty()
    def thunk(f), do: new(nil, f)

    # @spec next(t(a)) :: {a, t(a)} | nil when a: any()
    # def next(tree)
    # def next(%__MODULE__{next: nil, thunk: nil}), do: nil
    # def next(%__MODULE__{next: nil, thunk: f}), do: next(f.())
    # def next(%__MODULE__{next: {:just, a}, thunk: f}), do: {a, thunk(f)}

    # @spec interleave(t(a), t(a)) :: t(a) when a: any()
    # def interleave(a, b)
    # def interleave(%__MODULE__{next: nil, thunk: nil}, %__MODULE__{} = b), do: b
    # def interleave(%__MODULE__{next: nil, thunk: f}, %__MODULE__{} = b), do: thunk(fn -> interleave(b, thunk(f)) end)
    # def interleave(%__MODULE__{next: {:just, a}, thunk: f}, %__MODULE__{} = b), do: new(just(a), fn -> interleave(b, thunk(f)) end)

    # @spec bind(t(a), (a -> t(b))) :: t(b) when a: any(), b: any()
    # def bind(m, k)
    # def bind(%__MODULE__{next: nil, thunk: nil}, _k), do: empty()
    # def bind(%__MODULE__{next: nil, thunk: f}, k), do: thunk(fn -> bind(f.(), k) end)
    # def bind(%__MODULE__{next: {:just, a}, thunk: nil}, k), do: k.(a)
    # def bind(%__MODULE__{next: {:just, a}, thunk: f}, k), do: interleave(k.(a), thunk(fn -> bind(f.(), k) end))

    defimpl Logic.Bindable do
      def next(%@for{next: nil, thunk: nil}), do: nil
      def next(%@for{next: nil, thunk: f}), do: @protocol.next(f.())
      def next(%@for{next: {:just, a}, thunk: f}), do: {a, @for.thunk(f)}

      def interleave(%@for{next: nil, thunk: nil}, %@for{} = b), do: b
      def interleave(%@for{next: nil, thunk: f}, %@for{} = b), do: @for.thunk(fn -> @protocol.interleave(b, @for.thunk(f)) end)
      def interleave(%@for{next: {:just, a}, thunk: f}, %@for{} = b), do: @for.new(@for.just(a), fn -> @protocol.interleave(b, @for.thunk(f)) end)

      def bind(%@for{next: nil, thunk: nil}, _k), do: @for.empty()
      def bind(%@for{next: nil, thunk: f}, k), do: @for.thunk(fn -> @protocol.bind(f.(), k) end)
      def bind(%@for{next: {:just, a}, thunk: nil}, k), do: k.(a)
      def bind(%@for{next: {:just, a}, thunk: f}, k), do: @protocol.interleave(k.(a), @for.thunk(fn -> @protocol.bind(f.(), k) end))
    end

    defimpl Enumerable do
      alias Logic.Bindable

      def count(_data), do: {:error, __MODULE__}
      def member?(_data, _term), do: {:error, __MODULE__}
      def slice(_data), do: {:error, __MODULE__}

      def reduce(_data, {:halt, acc}, _fun), do: {:halted, acc}
      def reduce(data, {:suspend, acc}, fun), do: {:suspended, acc, &reduce(data, &1, fun)}
      def reduce(data, {:cont, acc}, fun) do
        case Bindable.next(data) do
          nil -> {:done, acc}
          {next, data} -> reduce(data, fun.(next, acc), fun)
        end
      end
    end

    defimpl Inspect do
      import Inspect.Algebra

      def inspect(tree, opts) do
        thunk = if is_nil(tree.thunk), do: "[]", else: "[...]"
        xs = if tree.next?, do: [tree.next, thunk], else: [thunk]
        container_doc("#LazyTree<", xs, ">", opts, &@protocol.inspect/2, [separator: ",", break: :flex])
      end
    end
  end
end

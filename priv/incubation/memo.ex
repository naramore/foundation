defprotocol Memo.Cache.Protocol do
  @moduledoc """
  """

  @type key() :: term()
  @type val() :: term()
  @type meta() :: term()

  @doc """
  """
  @spec lookup(t(), [{key(), meta()}], keyword()) :: {:ok, [{key(), val(), meta()}]} | {:error, reason :: term()}
  def lookup(cache, keys, opts \\ [])

  @doc """
  """
  @spec insert(t(), [{key(), val(), meta()}], keyword()) :: {:ok, t()} | {:error, reason :: term()}
  def insert(cache, key_values, opts \\ [])

  @doc """
  """
  @spec delete(t(), [key()], keyword()) :: {:ok, t()} | {:error, reason :: term()}
  def delete(cache, keys, opts \\ [])
end

defmodule Memo.Cache do
  @moduledoc """
  """

  @default_module Memo.Cache.PD
  @caching_key {__MODULE__, :cache}

  @type t() :: Memo.Cache.Protocol.t()
  @type key() :: Memo.Cache.Protocol.key()
  @type val() :: Memo.Cache.Protocol.val()
  @type meta() :: Memo.Cache.Protocol.meta()

  @doc """
  """
  @spec lookup([{key(), meta()}], keyword()) :: {:ok, [{key(), val(), meta()}]} | {:error, reason :: term()}
  def lookup(keys, opts \\ []) do
    {module, cache} = get_cache_from_opts(opts)
    module.lookup(cache, keys, opts)
  end

  @doc """
  """
  @spec insert([{key(), val(), meta()}], keyword()) :: :ok | {:error, reason :: term()}
  def insert(keys, opts \\ []) do
    {module, cache} = get_cache_from_opts(opts)
    case module.insert(cache, keys, opts) do
      {:ok, cache} -> put_cache(cache)
      _ -> :ok
    end
  end

  @doc """
  """
  @spec delete([key()], keyword()) :: :ok | {:error, reason :: term()}
  def delete(keys, opts \\ []) do
    {module, cache} = get_cache_from_opts(opts)
    case module.delete(cache, keys, opts) do
      {:ok, cache} -> put_cache(cache)
      _ -> :ok
    end
  end

  @spec get_cache_from_opts(keyword()) :: {module(), t()}
  defp get_cache_from_opts(opts) do
    module = Keyword.get(opts, :module, @default_module)
    {module, get_cache(module)}
  end

  @doc false
  @spec init_cache(module()) :: t()
  def init_cache(module) do
    module.new()
  end

  @doc false
  @spec get_cache(module()) :: t()
  def get_cache(module) do
    case fetch_cache() do
      {:ok, cache} -> cache
      :error ->
        cache = init_cache(module)
        :ok = put_cache(cache, false)
        cache
    end
  end

  @spec fetch_cache() :: {:ok, t()} | :error
  defp fetch_cache() do
    case :erlang.get(@caching_key) do
      :undefined -> :error
      cache -> {:ok, cache}
    end
  end

  @doc false
  @spec put_cache(t(), boolean()) :: :ok
  def put_cache(cache, check_for_change? \\ true) do
    if check_for_change? do
      case fetch_cache() do
        {:ok, ^cache} -> :ok
        _ ->
          _ = :erlang.put(@caching_key, cache)
          :ok
      end
    else
      _ = :erlang.put(@caching_key, cache)
      :ok
    end
  end
end

defmodule Memo.Cache.Map do
  @moduledoc """
  """

  @behaviour Access

  defstruct [cache: %{}]
  @type t() :: %__MODULE__{
    cache: map(),
  }

  @spec new(map()) :: t()
  def new(map \\ %{}), do: %__MODULE__{cache: map}

  @impl Access
  def fetch(%__MODULE__{cache: cache}, key) do
    Map.fetch(cache, key)
  end

  @impl Access
  def get_and_update(%__MODULE__{cache: cache}, key, fun) do
    {val, data} = Map.get_and_update(cache, key, fun)
    {val, new(data)}
  end

  @impl Access
  def pop(%__MODULE__{cache: cache}, key) do
    {val, data} = Map.pop(cache, key)
    {val, new(data)}
  end

  @spec merge(t(), t()) :: t()
  def merge(%__MODULE__{cache: a}, %__MODULE__{cache: b}) do
    new(Map.merge(a, b))
  end

  defimpl Memo.Cache.Protocol do
    def lookup(%@for{cache: cache}, keys, _opts \\ []) do
      keys
      |> Stream.map(&{&1, Map.get(cache, &1)})
      |> Stream.reject(&match?({_, nil}, &1))
      |> Enum.map(fn {k, {v, m}} -> {k, v, m} end)
      |> then(&{:ok, &1})
    end

    def insert(map, key_values, _opts \\ []) do
      key_values
      |> Stream.map(fn {k, v, m} -> {k, {v, m}} end)
      |> Enum.into(%{})
      |> @for.new()
      |> then(&{:ok, @for.merge(map, &1)})
    end

    def delete(%@for{cache: cache}, keys, _opts \\ []) do
      keys
      |> Enum.reduce(cache, &Map.delete(&2, &1))
      |> then(&{:ok, @for.new(&1)})
    end
  end
end

defmodule Memo.Cache.PD do
  @moduledoc """
  """

  defstruct []
  @type t() :: %__MODULE__{}

  @spec new() :: t()
  def new, do: %__MODULE__{}

  defimpl Memo.Cache.Protocol do
    def lookup(_, keys, _opts \\ []) do
      keys
      |> Stream.map(&{&1, :erlang.get(&1)})
      |> Stream.reject(&match?({_, :undefined}, &1))
      |> Enum.map(fn {k, {v, m}} -> {k, v, m} end)
      |> then(&{:ok, &1})
    end

    def insert(pd, key_values, _opts \\ []) do
      :ok = Enum.each(key_values, fn {k, v, m} -> :erlang.put(k, {v, m}) end)
      {:ok, pd}
    end

    def delete(pd, keys, _opts \\ []) do
      :ok = Enum.each(keys, &:erlang.erase/1)
      {:ok, pd}
    end
  end
end

defmodule Memo.Cache.Ets do
end

defmodule Memo.Cache.Dets do
end

defmodule Memo do
  @moduledoc """
  """
  use Boundary, deps: [], exports: [Cache]

  # @doc false
  # defmacro __using__(_opts \\ []) do
  #   quote do
  #     require Memo
  #   end
  # end

  @doc """
  """
  defmacro ize(opts \\ [], do: block) do
    {m, f, args} = extract_key(__CALLER__)
    quote do
      key = {unquote(m), unquote(f), unquote(args)}
      Memo.memoize(key, unquote(opts), fn ->
        unquote(block)
      end)
    end
  end

  @spec extract_key(Macro.Env.t()) :: {module(), atom(), [...]}
  defp extract_key(env) do
    m = env.module
    {f, _} = env.function
    args = Enum.map(
      Macro.Env.vars(env),
      fn {v, c} ->
        Macro.var(v, c)
      end
    )

    {m, f, args}
  end

  @doc """
  """
  @spec memoize(key :: term(), keyword(), (-> a)) :: a when a: term()
  def memoize(key, _opts \\ [], fun) do
    case :erlang.get(key) do
      :undefined ->
        val = fun.()
        _ = :erlang.put(key, val)
        val

      val ->
        val
    end
  end

  @doc """
  """
  @spec clear({module(), atom(), [...]} | {module(), atom()} | module()) :: :ok
  def clear(mod) do
    :erlang.get()
    |> Enum.map(&elem(&1, 0))
    |> filter_keys(mod)
    |> Enum.each(&:erlang.erase/1)
  end

  @spec filter_keys([{module(), atom(), [...]}], {module(), atom(), [...]} | {module(), atom()} | module()) :: [{module(), atom(), [...]}]
  defp filter_keys(keys, {mod, fun, args}), do: Enum.filter(keys, &match?({^mod, ^fun, ^args}, &1))
  defp filter_keys(keys, {mod, fun}), do: Enum.filter(keys, &match?({^mod, ^fun, _}, &1))
  defp filter_keys(keys, mod), do: Enum.filter(keys, &match?({^mod, _, _}, &1))
end

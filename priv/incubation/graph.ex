defmodule Graph.Fnk do
  @moduledoc false

  defstruct inputs: [],
            function: nil

  @type t :: %__MODULE__{
          inputs: [atom()],
          # constraint: _,
          function: (... -> any())
        }

  # @spec new()

  defmacro defnk() do
    quote do
    end
  end

  defmacro fnk() do
    quote do
    end
  end
end

defmodule Graph do
  @moduledoc """
  """

  alias Graph.Fnk

  @default_task_supervisor Graph.TaskSupervisor
  @default_async_opts []

  @type t :: %{required(atom()) => Fnk.t()}
  @type input :: map()

  @doc """
  """
  @spec compile(t(), input(), keyword()) :: {:ok, map()} | {:error, reason :: term()}
  def compile(graph, input, opts \\ []) do
    with {:ok, dg} <- build(graph, input),
         :ok <- validate_digraph(dg, graph, input),
         _ <- :digraph.delete(dg) do
      graph
      |> init(input)
      |> compile_loop(graph, opts)
    else
      {:error, reason} -> {:error, reason}
    end

    # {dg, rs} = Graph.compile(%{
    #   n: %Graph.Fnk{inputs: [:xs]},
    #   m: %Graph.Fnk{inputs: [:xs, :n]},
    #   m2: %Graph.Fnk{inputs: [:xs, :n]},
    #   v: %Graph.Fnk{inputs: [:m, :m2]}
    # }, %{xs: [1,2,3,6]})

    # 1. construct directed graph =>
    # 2. list all nodes with in_edges that are all contained in input
    # 3. if ^^^ is empty -> verify that ALL nodes are done + return input
    # 4. execute these nodes:
    #   - serial -> execute one at a time
    #   - async  -> Task.async |> Task.yield()
    # 5. take result(s) and insert them into the input
    # 6. goto 2
  end

  defp compile_loop(state, graph, opts) do
    # check if any are ready -> if so, execute them
    #                           otherwise -> continue
    # check if any are executing -> if so, yield w/ timeout
    #                               if yielded -> add_result
    #                               otherwise -> check limitations
    #                                            if within limits -> recur
    #                                            otherwise -> error
    # otherwise -> verify_complete
    #              if so, return results
    #              otherwise -> error

    state =
      state
      |> execute_nodes(ready(state), graph, opts)
      |> Enum.reduce(state, fn {n, t}, s ->
        add_executing(s, n, t)
      end)

    case executing(state) do
      [] ->
        case verify_complete(state) do
          {:error, reason} -> {:error, reason}
          :ok -> {:ok, state_to_result(state)}
        end

      nodes ->
        nodes
        # |> Enum.reduce([], fn {k, t}, acc ->
        #   case Task.yield(t, timeout) do
        #     {:ok, v} -> [{k, v} | acc]
        #     nil -> acc
        #   end
        # end)
        # |> case do
        #   [] ->
        #     # TODO: check limitations (so as not to wait forever)
        # end
    end

    case ready(state) do
      [] ->
        case verify_complete(state) do
          {:error, reason} -> {:error, reason}
          :ok -> {:ok, state_to_result(state)}
        end

      nodes ->
        # state =
        #   state
        #   |> execute_nodes(nodes, graph, opts)
        #   |> Enum.reduce(state, fn {n, t}, s ->
        #     add_executing(s, n, t)
        #   end)
        nodes
    end
  end

  defp execute_nodes(state, nodes, graph, opts) do
    task_supervisor = Keyword.get(opts, :task_supervisor, @default_task_supervisor)
    async_opts = Keyword.get(opts, :async_opts, @default_async_opts)

    Enum.map(nodes, fn node ->
      {node,
       Task.Supervisor.async_nolink(
         task_supervisor,
         fn ->
           execute(
             Map.get(graph, node),
             state
           )
         end,
         async_opts
       )}
    end)
  end

  defp execute(fnk, state) do
    apply(
      fnk.function,
      Enum.map(fnk.inputs, &extract_input(state, &1))
    )
  end

  defp extract_input(state, input) do
    {:done, x} = Map.get(state, input)
    x
  end

  @spec build(t(), input()) :: {:ok, :digraph.graph()} | {:error, reason :: term()}
  defp build(graph, input) do
    dg = :digraph.new([:acyclic, :protected])
    :ok = input |> Map.keys() |> Enum.each(&:digraph.add_vertex(dg, &1))
    :ok = Enum.each(graph, fn {k, fnk} -> :digraph.add_vertex(dg, k, fnk) end)

    graph
    |> Enum.map(fn {k, %{inputs: is}} -> Enum.map(is, &{&1, k}) end)
    |> List.flatten()
    |> Enum.map(fn {v1, v2} -> :digraph.add_edge(dg, v1, v2) end)
    |> Enum.reduce(:ok, fn
      {:error, r}, {:error, rs} -> {:error, [r | rs]}
      {:error, r}, :ok -> {:error, [r]}
      _x, :ok -> :ok
    end)
    |> case do
      {:error, reasons} -> {:error, {:invalid_graph, :bad_edges, reasons}}
      :ok -> {:ok, dg}
    end
  end

  @spec validate_digraph(:digraph.graph(), t(), input()) :: :ok | {:error, reason :: term()}
  defp validate_digraph(digraph, graph, input) do
    reachable_vertices =
      input
      |> Map.keys()
      |> :digraph_utils.reachable_neighbours(digraph)

    {all, reachable} = {MapSet.new(reachable_vertices), MapSet.new(Map.keys(graph))}
    unreachable = MapSet.difference(all, reachable)
    extra = MapSet.difference(reachable, all)

    if Enum.empty?(unreachable) and Enum.empty?(extra) do
      :ok
    else
      {:error, {:invalid_graph, :incomplete, unreachable: unreachable, extra: extra}}
    end
  end

  defp init(graph, input) do
    graph
    |> Enum.map(fn {k, fnk} -> {k, {:waiting_on, fnk.inputs}} end)
    |> Enum.into(%{})
    |> then(
      &Enum.reduce(input, &1, fn {k, v}, s ->
        add_result(s, k, v)
      end)
    )
  end

  defp add_executing(state, key, ref) do
    Map.put(state, key, {:executing, ref})
  end

  defp add_result(state, key, result) do
    state
    |> Map.put(key, {:done, result})
    |> update_all_waiting(key)
  end

  defp update_all_waiting(state, dep) do
    state
    |> Enum.map(fn
      {k, {:waiting_on, deps}} ->
        {k, {:waiting_on, List.delete(deps, dep)}}

      x ->
        x
    end)
    |> Enum.into(%{})
  end

  defp waiting_on(state) do
    state
    |> Enum.filter(&match?({_, {:waiting_on, _}}, &1))
    |> Enum.map(fn {k, {:waiting_on, deps}} -> {k, deps} end)
  end

  defp ready(state) do
    state
    |> waiting_on()
    |> Enum.filter(&match?({_, []}, &1))
    |> Enum.map(&elem(&1, 0))
  end

  defp executing(state) do
    state
    |> Enum.filter(&match?({_, {:executing, _}}, &1))
    |> Enum.map(fn {k, {:executing, ref}} -> {k, ref} end)
  end

  # defp done(state) do
  #   state
  #   |> Enum.filter(&match?({_, {:done, _}}, &1))
  #   |> Enum.map(fn {k, {:done, v}} -> {k, v} end)
  # end

  defp verify_complete(state) do
    state
    |> Enum.reduce(:ok, fn
      {_, {:done, _}}, acc -> acc
      x, :ok -> {:error, [x]}
      x, {:error, xs} -> {:error, [x | xs]}
    end)
    |> case do
      {:error, reasons} -> {:error, {:not_complete, reasons}}
      :ok -> :ok
    end
  end

  defp state_to_result(state) do
    state
    |> Enum.map(fn {k, {:done, v}} -> {k, v} end)
    |> Enum.into(%{})
  end
end

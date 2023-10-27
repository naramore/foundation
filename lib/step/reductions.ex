defmodule Step.Reductions do
  @moduledoc """
  Implementation of [diet](https://github.com/pragdave/diet), with adapters
  for the pause monad in `Pend` and `Step`.

  We define a high-level macro, `reductions`. It takes a `do`-block
  containing a list of `a -> b` expressions (just like `cond` or
  `case`).

  Each entry in this list represents a reduction. The left hand side
  specifies a pattern that must be matched , and the right hand side
  gives the transformation to the model (if any) and then returns the
  new trigger.

  ```elixir
  defmodule Step.RLE do
    use Step.Reductions

    reductions do
      {:rle, {[], result}} ->
        {:done, Enum.reverse(result)}

      {:rle, {[{a, n}, a | tail], result}} ->
        {:rle, {[{a, n + 1} | tail], result}}

      {:rle, {[a, a | tail], result}} ->
        {:rle, {[{a, 2} | tail], result}}

      {:rle, {[a | tail], result}} ->
        {:rle, {tail, [a | result]}}

      {:rle, list} ->
        {:rle, {list, []}}
    end
  end
  ```

  In the RLE example, the atom `:rle` is actually not needed at the
  fron of each tuple.

  ## Internals

  Each transform is converted into a function called `step`. This
  function takes the lhs of the transform and the
  model, wrapped in a tuple. So the first transform becomes something like:

  ```elixir
  def step({{:encode, string}, model}) do
    val = {:rle, {String.codepoints(string), []}}
    case val do
      {:update_model, model, result } ->
        {:continue, result, model}
      result ->
        {:continue, result, unquote({:model, [], nil})}
    end
  end
  ```

  So! you cry. What's with the `:update_model`?

  If a transform needs to update the model, it has to signal the fact
  by ending the transform code with

  ```elixir
  update_model(new_model_value) do
    «return value»
  end
  ```

  The `update_model` function then returns a tuple starting
  `:update_model`, which tells the step code to replace the model with
  the new value when it returns.
  """

  @doc false
  @spec bind(Pend.pause(s, a), module()) :: Pend.pause(s, a)
        when s: Step.model(), a: Step.trigger()
  def bind(m, module), do: Step.bind(m, adapt(module))

  # TODO: take `s` -> and put model into `a`
  @doc false
  @spec adapt(module()) :: (a -> Pend.pause(s, a)) when s: Step.model(), a: Step.trigger()
  def adapt(module), do: &adapt(&1, module)

  @doc false
  @spec adapt(a, module()) :: Pend.pause(s, a) when s: Step.model(), a: Step.trigger()
  def adapt(a, module), do: &adapt(a, &1, module)

  @doc false
  @spec adapt(a, s, module()) :: {Pend.result(s, a), s} when s: Step.model(), a: Step.trigger()
  def adapt(a, s, module) do
    case module.reduce({a, s}) do
      {:done, r, m} -> {{:done, r}, m}
      {:continue, r, m} -> {{:suspend, adapt(r, module)}, m}
    end
  end

  @doc """
  Used in the `reductions/2` macro to update the model.

  ## Examples

    ```elixir
    update_model(new_model_value) do
      «return value»
    end
    ```
  """
  @spec update_model(Step.model(), Macro.t()) :: {:update_model, Step.model(), Macro.t()}
  def update_model(model, do: return) do
    {:update_model, model, return}
  end

  # coveralls-ignore-start
  @doc """
  """
  defmacro reductions(opts \\ [], do: body) do
    model_name = opts[:model_name] || :model
    model_name = {model_name, [], nil}

    steps =
      for {:->, _, [[lhs], rhs]} <- body do
        generate_step(lhs, rhs, opts[:debug], model_name)
      end

    quote do
      @doc false
      @spec reduce({Step.trigger(), Step.model()}) ::
              {:done | :continue, result :: term(), Step.model()}
      unquote_splicing(steps)

      def reduce({result, unquote(model_name)}) do
        unquote(
          maybe(
            opts[:debug],
            quote do
              _ = Logger.debug("finished: #{inspect(result)}")
            end
          )
        )

        {:done, result, unquote(model_name)}
      end
    end
  end

  @doc false
  defmacro __using__(opts) do
    result = [
      model_name(opts[:model]),
      model_as(opts[:model], opts[:as])
    ]

    quote generated: true do
      require Logger
      require Step.Reductions
      import Step.Reductions, only: [reductions: 2, reductions: 1, update_model: 2]

      unquote_splicing(result)

      @doc """
      For `Step.bind/2`ing this reduction to a `Pend` suspendable pipeline.
      """
      @spec bind(Pend.pause(s, a)) :: Pend.pause(s, a) when s: Step.model(), a: Step.trigger()
      def bind(m), do: Step.Reductions.bind(m, __MODULE__)

      @doc """
      See `Step.run/2` for more details.
      """
      @spec run(a, s) :: {a, s} when s: Step.model(), a: Step.trigger()
      def run(a, s), do: Step.start(a) |> bind() |> Step.run(s)
    end
  end

  defp model_name(model) do
    quote do
      @doc false
      @spec rm_model() :: module()
      def rm_model do
        unquote(model)
      end
    end
  end

  defp model_as(model, as) when not (is_nil(model) or is_nil(as)) do
    quote do
      alias unquote(model), as: unquote(as)
    end
  end

  defp model_as(model, _as) when not is_nil(model) do
    quote do
      alias unquote(model)
    end
  end

  defp model_as(_model, _as), do: nil

  defp generate_step(trigger, body, debug, model_name) when debug do
    body =
      quote generated: true do
        _ = Logger.debug("handling #{inspect(unquote(trigger))}")

        case unquote(body) do
          {:update_model, unquote(model_name), result} ->
            _ = Logger.debug("   new model: #{inspect(unquote(model_name))}")
            _ = Logger.debug("   => #{inspect(result)}")
            {:continue, result, unquote(model_name)}

          result ->
            _ = Logger.debug("   => #{inspect(result)}")
            {:continue, result, unquote(model_name)}
        end
      end

    generate_full_step(trigger, body, model_name)
  end

  defp generate_step(trigger, body, _debug, model_name) do
    body =
      quote generated: true do
        case unquote(body) do
          {:update_model, unquote(model_name), result} ->
            {:continue, result, unquote(model_name)}

          result ->
            {:continue, result, unquote(model_name)}
        end
      end

    generate_full_step(trigger, body, model_name)
  end

  defp generate_full_step({:when, _, [trigger, when_clause]}, body, model_name) do
    quote do
      def reduce(unquote(add_model_to(trigger, model_name)))
          when unquote(when_clause) do
        unquote(body)
      end
    end
  end

  defp generate_full_step(trigger, body, model_name) do
    quote do
      def reduce(unquote(add_model_to(trigger, model_name))) do
        unquote(body)
      end
    end
  end

  defp add_model_to(trigger, model_name) do
    quote do
      {
        unquote(trigger),
        unquote(model_name)
      }
    end
  end

  defp maybe(flag, code) when flag, do: code
  defp maybe(_, _), do: nil
  # coveralls-ignore-stop
end

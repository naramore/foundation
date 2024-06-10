defmodule Diet.Stepper do

  @moduledoc """
  Executes the reduction machine until it generates a terminating
  transition.

  Before performing any work, you need to initialize a stepper, passing
  it the module containing the reductions and a state that the
  reductions can use:

  ~~~ elixir
  stepper = Stepper.new(RunLengthEncode, nil)
  ~~~

  You then run the machine, passing in any parameters you like.
  Conventionally, you'll pass either an atom representing an event, or
  a tuple with an atom and some additional parameters.

  ~~~ elixir
  { result, updated_stepper } = Stepper.run(stepper, { :encode, "11123342111" }
  ~~~

  """

  alias Diet.History

  defstruct(
    module:       nil,
    model:        nil,
    history:      [],
    run_index:    0,
    step_index:   0)

  def new(module, model_params) do
    model_module = module.rm_model()
    model_data = if model_module do
      model_module.build(model_params)
    else
      model_params
    end

    %__MODULE__{
      module: module,
      model:  model_data,
    }
  end

  def clone(stepper, history) do
    { number, state } = hd(history)
    [ run, step ] =
      number
      |> to_string
      |> String.split(".")
      |> Enum.map(&String.to_integer/1)

    %__MODULE__ {
      module: stepper.module,
      model:  state.new_model,
      history: history,
      run_index: run,
      step_index: step
    }
  end

  @doc """
  Run the reduction machine, passing the initial trigger. The machine
  will then return the result of reducing that. If the result
  represents the conclusion of the machine's work, it will return
  `{ :done, result, model }`. We extract the result and the updated
  model and return them in a tuple to the caller.
  """

  def run(stepper, trigger) do
    { result, stepper } =
    stepper
    |> History.push()
    |> run_step(trigger)
    |> after_step()

    stepper = History.pop(stepper)
    { result, stepper }
  end

  defp run_step(stepper, trigger) do
    step_result = stepper.module.step({ trigger, stepper.model })
    { _continue_or_done, result, model } = step_result
    stepper     = History.record(stepper, trigger, result, model)
    { stepper, step_result }
  end

  defp after_step({ stepper, { :continue, result, model }}) do
    run_step( %{ stepper | model: model }, result)
    |> after_step()
  end

  defp after_step({ stepper, { :done, result, model }}) do
    { result, %{ stepper | model: model } }
  end

end

defmodule Diet.Transformations do

  @moduledoc """
  We define a high-level macro, `transforms`. It takes a `do`-block
  containing a list of `a -> b` expressions (just like `cond` or
  `case`).

  Each entry in this list represents a reduction. The left hand side
  specifies a pattern that must be matched , and the right hand side
  gives the transformation to the model (if any) and then returns the
  new trigger.

  ~~~ elixir
  defmodule RLE do
    use Diet.Transformations

    transforms do

      { :rle, {[], result} } ->
              {:done, result |> Enum.reverse}

      { :rle, {[ {a, n}, a | tail], result }} ->
              {:rle, {[{a, n+1} | tail], result}}

      { :rle, {[ a, a | tail ], result } } ->
              {:rle, {[{a, 2} | tail], result }}

      { :rle, {[a | tail ], result } } ->
              {:rle, {tail, [a | result]  }}

      { :rle, list } ->
              {:rle, {list, []}}
    end

  end
  ~~~

  In the RLE example, the atom `:rle` is actually not needed at the
  fron of each tuple.
  ## Internals

  Each transform is converted into a function called `step`. This
  function takes the lhs of the transform and the
  model, wrapped in a tuple. So the first transform becomes something like:

  ~~~ elixir
  def step({{ :encode, string }, model}) do
    val = { :rle, { String.codepoints(string), [] } }
    case val do
      { :update_model, model, result } ->
            { :continue, result, model }
      result ->
            { :continue, result, unquote({:model, [], nil}) }
    end
  end
  ~~~

  So! you cry. What's with the `:update_model`?

  If a transform needs to update the model, it has to signal the fact
  by ending the transform code with

  ~~~ elixir
  update_model(new_model_value) do
    «return value»
  end
  ~~~

  The `update_model` function then returns a tuple starting
  `:update_model`, which tells the step code to replace the model with
  the new value when it returns.
  """

  defmacro reductions(opts \\ [], do: body) do
    model_name = opts[:model_name] || :model
    model_name = { model_name, [], nil }

    steps = for {:->, _, [ [lhs], rhs ]} <- body do
      generate_step(lhs, rhs, opts[:debug], model_name)
    end

    quote do
      unquote_splicing(steps)

      def step({result, unquote(model_name)}) do
        unquote(maybe(opts[:debug],
          quote do
            Logger.debug("finished: #{inspect(result)}")
          end))
        { :done, result, unquote(model_name) }
      end
    end

  end

  def generate_step(trigger, body, debug, model_name) when debug do
    body = quote do
        Logger.debug("handling #{inspect(unquote(trigger))}")

        case unquote(body) do
          { :update_model, model, result } ->
            Logger.debug("   new model: #{inspect unquote(model_name)}")
            Logger.debug("   => #{inspect result}")
            { :continue, result, unquote(model_name) }
          result ->
            Logger.debug("   => #{inspect result}")
            { :continue, result, unquote(model_name) }
        end
    end

    generate_full_step(trigger, body, model_name)
  end

  def generate_step(trigger, body, _debug, model_name)  do
    body = quote do
      case unquote(body) do
        { :update_model, unquote(model_name), result } ->
          { :continue, result, unquote(model_name) }
        result ->
          { :continue, result, unquote(model_name) }
      end
    end

    generate_full_step(trigger, body, model_name)
  end


  defp generate_full_step({:when, _, [ trigger, when_clause ]}, body, model_name) do
    quote do
      def step(unquote(add_model_to(trigger, model_name)))
          when unquote(when_clause) do
        unquote(body)
      end
    end
  end

  defp generate_full_step(trigger, body, model_name) do
    quote do
      def step(unquote(add_model_to(trigger, model_name))) do
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

  def update_model(model, do: return) do
    { :update_model, model, return }
  end

  defmacro __using__(opts) do
    result = [
      standard_header(),
      model_name(opts[:model]),
      model_as(opts[:model], opts[:as]),
    ]

    quote do
      unquote_splicing(result)
    end
  end

  defp model_name(model) do  # may be nil. that's ok.
    quote do
      def rm_model() do
        unquote(model)
      end
    end
  end


  defp model_as(model, as) when not (is_nil(model) or is_nil(as)) do
    quote do
      alias unquote(model), as: unquote(as)
    end
  end

  defp model_as(_, _), do: nil


  defp standard_header() do
    quote do
      require Logger
      require Diet.Transformations
      import  Diet.Transformations
    end
  end


  defp maybe(flag, code) when flag, do: code
  defp maybe(_, _), do: nil


end

defmodule Diet.History do

  def push(stepper) do
    %{ stepper | run_index: stepper.run_index + 1, step_index: 0 }

  end

  def pop(stepper) do
    stepper
  end

  def record(stepper, trigger, result, new_model) do
    entry = %{
      trigger:   trigger,
      old_model: stepper.model,
      new_model: new_model,
      result:    result
    }

    with step_no = stepper.step_index + 1,
         step = { :"#{stepper.run_index}.#{step_no}", entry },
    do: %{ stepper | history: [ step | stepper.history ], step_index: step_no}
  end

end

defmodule Diet.Debug do

  def on(stepper) do
    name = stepper.module |> to_string |> String.trim_leading("Elixir.")
    IO.puts "\nDiet debugger looking at #{name}\n"
    IO.puts "h for help\n"

    stepper
    |> build_state(name)
    |> interact
  end

  defp build_state(stepper, name) do
    %{
      name:    name,
      steppers: %{ 0 => stepper },
      current:  0,
    }
  end

  defp interact(state) do
    res = IO.gets "#{state.name}[#{state.current}]> "
    handle(res, state)
  end

  defp handle({:error, reason}, _), do: IO.inspect(reason)
  defp handle(:eof, _), do: IO.puts("done")

  defp handle(other, state) do
    [ cmd | args ] =
    other
    |> String.trim
    |> String.downcase
    |> String.split

    command(cmd, Enum.join(args, " "), state)
  end

  ###################
  # c [ <step id> ] #
  ###################

  defp command("c", "", state) do
    command("c", [ hd(stepper(state).history).number ], state)
  end

  defp command("c", number, state) do
    new_history = stepper(state).history |> rollback_to(String.to_atom(number))
    new_stepper = Diet.Stepper.clone(stepper(state), new_history)
    current_max = (state.steppers |> Map.keys |> Enum.max) + 1

    put_in(state.steppers[current_max], new_stepper)
    |> Map.put(:current, current_max)
    |> interact
  end

  ####################################
  # clones   — show list of clones   #
  ####################################

  defp command("clones", _, state) do
    IO.puts "\n  #\tin state"
    IO.puts "  --\t--------\n"

    state.steppers
    |> Map.keys
    |> Enum.sort
    |> Enum.each(&show_clone(state, &1))
    interact(state)
  end

  #################################
  # switch n  — switch to clone n #
  #################################

  defp command("switch", clone, state) do
    clone = String.to_integer(clone)
    state = %{ state | current: clone }
    history = stepper(state).history
    step = most_recent_step(history)
    show_step(history, step)
    interact(state)
  end

  #######################
  # t   — trace history #
  #######################

  defp command("t", _, state) do
    stepper(state).history
    |> Enum.reverse
    |> dump_history

    interact(state)
  end

  ####################
  # h    — give help #
  ####################

  defp command("?", x, state), do: command("h", x, state)
  defp command("h", _, state) do
    IO.puts """

    t         – show execution trace
    n.n       – show details of step n.n
    r «trigger args»
              – run the stepper using the given trigger

    c [n.n]   – clone the current execution at the given step (or the current step
                if n.n is omitted
    clones    — list all clones
    switch n  — switch to clone #n

    q         – quit
    """
    interact(state)
  end

  ##############################
  # r <args>   — run a stepper #
  ##############################

  defp command("r", args, state) do
    args = if String.starts_with?(args, ":") && String.contains?(args, " ") do
      String.replace(args, " ", ", ", global: false)
    else
      args
    end

    args = if String.contains?(args, ",") && !String.starts_with?(args, "{") do
      "{ #{args} }"
    else
      args
    end

    try do
      {arg_val, _} = Code.eval_string(args)
      { result, stepper } = Diet.Stepper.run(stepper(state), arg_val)
      IO.puts "\nResult: #{inspect result, pretty: true}"
      put_in(state.steppers[state.current], stepper)
      |> interact
    rescue
      e in [ CompileError, SyntaxError ] ->
        IO.puts "error: #{e.description}\n"
        interact(state)
    end
  end

  ##############
  # q   — quit #
  ##############

  defp command("q", _, _state) do
    IO.puts "done\n"
  end

  defp command("", _, state) do
    interact(state)
  end


  ###########################
  # r.s   — display <r>.<s> #
  ###########################

  defp command(other, args, state) do
    case Regex.run(~r{^\d+\.\d+$}, other) do

      [step_id] ->
        show_step(stepper(state).history, step_id)

      _ ->
        IO.puts "Unknown command #{inspect other} #{inspect args}"
    end

    interact(state)
  end

  defp most_recent_step(history) do
    hd(history) |> elem(0)
  end

  defp show_step(history, step_id) when is_binary(step_id) do
    show_step(history,  String.to_atom(step_id))
  end

  defp show_step(history, step_id) when is_atom(step_id) do
    step = history[step_id]

    IO.puts "\tTrigger:  #{inspect step.trigger}"
    IO.puts "\tModel:    #{inspect step.old_model}"
    IO.puts "\tResult:   #{inspect step.result}"

    if step.old_model == step.new_model do
      IO.puts "\tNew model:  «unchanged»"
    else
      ExUnit.Diff.script(inspect(step.old_model), inspect(step.new_model))
      |> List.flatten
      |> Enum.map(&convert_fragment/1)
      |> (&IO.puts("\tChanges:  #{&1}")).()
    end
  end

  defp convert_fragment({:eq,  str}), do: IO.ANSI.format([:green, str])
  defp convert_fragment({:del, str}), do: IO.ANSI.format([:magenta, :inverse, str, :normal])
  defp convert_fragment({:ins, str}), do: IO.ANSI.format([:bright, :white, str, :normal])


  defp dump_history(history) do
    history
    |> Enum.each(&format_one_step(&1))
    IO.puts ""
  end

  defp format_one_step({ number, step}) do
    header   = "#{number}" |> String.pad_leading(6)
    trigger  = step.trigger |> inspect |> String.pad_leading(27)
    modified = if step.old_model == step.new_model, do: "•", else: "►"
    IO.puts "#{header} #{modified} #{trigger} → #{inspect step.result}"
  end

  ## Clone support

  defp rollback_to([], _number), do: []
  defp rollback_to(history = [{ number, _ } | _rest], number), do: history
  defp rollback_to([ _ | rest], number), do: rollback_to(rest, number)

  def show_clone(state, number) do
    flag = if state.current == number, do: "*", else: " "
    history = state.steppers[number].history
    step = most_recent_step(history)
    IO.puts "#{flag} #{number}\tStep #:   #{step}"
    show_step(history, step)
    IO.puts ""
  end


  defp stepper(state), do: state.steppers[state.current]
end

defmodule Diet do
  use Boundary, deps: [], exports: []

  def debug(stepper) do
    Diet.Debug.on(stepper)
  end
end

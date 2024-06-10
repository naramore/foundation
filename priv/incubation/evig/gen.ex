# TODO: property-based testing
# TODO: mutation testing
# NOTE: Sibyl + Snabkaffe (less necessary b/c `:eqc_temporal`)


# generators  -> primatives
#             -> containers
#             -> functions
#             -> analytics
#             -> symbolic
# format
# exunit_properties
# temporal    -> temporal
#             -> trace (i.e. `tp` & `tp_span`)
#             -> sibyl
# stateful    -> statem
#             -> fsm
#             -> component (+ mocking)
#             -> cluster
#             -> dynamic_cluster
# mutation    -> mix_task
#             -> mutators
#             -> test_runner


# type (e.g. integer, list, term)
# + strategy (i.e. random / targeted)
# + action (i.e. generate / shrink)

# generate(%Integer{min, max, step}, %Random{seed, size}) :: {:ok, %Generated{type, strategy, value}} | {:error, _}
# shrink(%Generated{}) :: {:ok, %Shrunk{}} | {:error, _}


# - [x] alt_gens
#   proper_sa:get_shrinker(sa_target(), sa_data()) -> proper_types:type().
#   proper_gen:alt_gens(proper_types:type()) -> [imm_instance()].
#     proper_types:unwrap(proper_types:type()) -> [proper_types:type(),...].
#       proper_shrink:alternate_shrinker(proper_gen:imm_instance(), proper_types:type(), state()) -> {[proper_gen:imm_instance()],state()}.
#       proper_shrink:unwrap_shrinker(proper_gen:imm_instance(), proper_types:type(), state()) -> {[proper_gen:imm_instance()],state()}.
#         proper_shrink:get_shrinkers(proper_types:type()) -> [shrinker()].
#           (when kind == wrapper)
#             proper_shrink:shrink(proper_gen:imm_instance(), proper_types:type(), state()) -> {[proper_gen:imm_instance()],state()}.
#               proper:shrink(imm_testcase(), imm_testcase(), stripped_test(), fail_reason(), non_neg_integer(), non_neg_integer(), proper_shrink:state(), opts()) -> shrinking_result().
# - [x] combine
# - [x] constraints
# - [x] env
# - [x] generator
# - [x] get_indices
# - [x] get_length
# - [x] is_instance
# - [x] is_user_nf
# - [x] internal_type
# - [x] internal_types
# - [x] join
# - [x] kind
# - [x] noshrink
# - [x] parameters
# - [x] parts_type
# - [x] remove
# - [x] retrieve
# - [ ] reverse_gen
# - [x] shrink_to_parts
# - [x] shrinkers
# - [x] size_transform
# - [x] split
# - [x] subenv
# - [x] update
# - [x] user_nf

# type
#   basic
#     integer
#       env           -> {low, high}
#       generator     -> {:typed, (t(x), size -> x)}
#       is_instance   -> {:typed, (t(x), x -> bool)}
#       shrinkers     -> [(x, t(x), state -> {x, state})]
#     float
#       env           -> {low, high}
#       generator     -> {:typed, (t(x), size -> x)}
#       is_instance   -> {:typed, (t(x), x -> bool)}
#       shrinkers     -> [(x, t(x), state -> {x, state})]
#     union
#       env           -> choices
#       generator     -> {:typed, (t(x) -> x)}
#       is_instance   -> {:typed, (t(x), x -> bool)}
#       shrinkers     -> [first_choice, recursive]
#     exactly
#       env           -> term
#       generator     -> {:typed, (t(x) -> x)}
#       is_instance   -> {:typed, (t(x), x -> bool)}
#     function
#       env           -> {arity, ret_type}
#       generator     -> {:typed, (t(x) -> x)}
#       is_instance   -> {:typed, (t(x), x -> bool)}
#   wrapper
#     lazy
#       generator       -> (-> x)
#     sized
#       generator       -> (size -> x)
#     shrinkwith
#       generator       -> (-> x)
#       alt_gens        -> (-> [x])
#     native_type
#       generator       -> (-> x)
#     atom
#       generator       -> (-> x)
#       reverse_gen     -> _
#       size_transform  -> (size -> size)
#       is_instance     -> (x -> bool)
#     binary
#       env             -> length
#       generator       -> {:typed, (t(x), size -> x)}
#       reverse_gen     -> _
#       is_instance     -> {:typed, (t(x), x -> bool)}
#     bitstring
#       env             -> length
#       generator       -> {:typed, (t(x), size -> x)}
#       reverse_gen     -> _
#       is_instance     -> {:typed, (t(x), x -> bool)}
#     loose_tuple
#       env             -> elem_type
#       generator       -> {:typed, (t(x), size -> x)}
#       reverse_gen     -> _
#       is_instance     -> {:typed, (t(x), x -> bool)}
#   constructed
#     bind
#       parts_type      -> type
#       combine         -> (_ -> _)
#       shrink_to_parts -> bool
#   container
#     list
#       generator       -> {:typed, (t(x), size -> x)}
#       is_instance     -> {:typed, (t(x), x -> bool)}
#       internal_type   -> y
#       get_length      -> (x -> integer)
#       split           -> (integer, x -> {x, x})
#       join            -> (x, x -> x)
#       get_indices     -> (t(x), x -> [i])
#       remove          -> (i, x -> x)
#       retrieve        -> (i, x -> y)
#       update          -> (i, y, x -> x)
#     shrink_list
#       env
#       generator
#       is_instance
#       get_length
#       split
#       join
#       get_indices
#       remove
#     vector
#       env
#       generator
#       is_instance
#       internal_type
#       get_indices
#       retrieve
#       update
#     tuple
#       env
#       generator
#       is_instance
#       internal_types
#       get_indices
#       retrieve
#       update
#     fixed_list
#       env
#       generator
#       is_instance
#       internal_types
#       get_indices
#       retrieve
#       update




# Evig
#   @spec generate(generator(a), Keyword.t()) :: {:ok, Generated.t(a)} | {:error, GeneratorError.t()}
#   @spec shrink(Generated.t(a), Keyword.t()) :: {:ok, Shrunk.t(a)} | {:error, ShrinkingError.t()}
# Evig.Generator
#   @spec generate(t(a), Strategy.t(), Keyword.t()) :: {:ok, a, shrinker(a)} | {:error, GeneratorError.t()}
# Evig.Generators.Integer
#                .Float
#                .Binary
#                ...
# Evig.Generated
#   defstruct [:generated, :shrinker, :generator_context]
# Evig.Shrunk
#   defstruct [:original_failure, :shrunk_failure, :nodes_visited, :successful_runs]
# Evig.Strategy
#   @type search_steps :: pos_integer()
#   @type strategy_data :: term()
#   @type target_state :: term()
#   @type next_fun :: (... -> type | instance)
#   @type fitness :: number()
#   @callback init_strategy(search_steps()) :: strategy_data()
#   @callback init_target(type, next_fun()) :: target_state()
#   @callback next(target_state(), strategy_data()) :: {instance, target_state(), strategy_data()}
#   @callback get_shrinker(target_state(), strategy_data()) :: type
#   @callback update_fitness(fitness(), target_state(), strategy_data()) :: {target_state(), strategy_data()}
#   @callback reset(target_state(), strategy_data()) :: {target_state(), strategy_data()}
# Evig.Strategies.PseudoRandom
#                .SimulatedAnnealing


###############################################################################
# Helpers

defmodule Evig.ExUnitProperties do
  @moduledoc false
end

defmodule Evig.Gen.Tools do
  @moduledoc false
end

###############################################################################
# Generators

# NOTE: Goals / Preferences
#   - Enumerable Protocol (if reasonably possible)
#   - Generators as Datastructures (as opposed to function closures as in StreamData)
#     (i.e. generators should be symbolic, so as to make presentation easier)
#     (but wait...bind makes this idea problematic...)
#     (maybe add a ``)
#   - needs to work for "random" generation and targetted / simulated annealing

# acc(a) :: {:cont | :halt | :suspend, a}
# reducer(a) :: (term, acc -> acc)
# result(a) :: {:done, a} | {:halted, a} | {:suspended, a, (acc(a) -> result(a))}
# Enumerable.reduce(t, acc, reducer) :: result

# LazyTree.t(a) :: %LazyTree{root: a, children: Stream.t(LazyTree.t(a))}
#                             ^         ^ "shrinking function"
#                             ^ "generated value"

# generator(a) :: (:rand.state(), non_neg_integer() -> LazyTree.t(a))
# StreamData.t(a) :: %StreamData{generator: generator(a)}
#   => generate(seed, size) :: LazyTree.t(a)
#   => shrink() => &Enumerable.reduce(children, &1, fn x, acc -> {:suspend, [x | acc]} end)

# integer => generate(ctx) :: {:ok, a} | {:error, _}
#         => shrink(a, ctx) :: result(a)
#         int_ctx :: %{max, min, step}
#         rng_ctx :: %{seed, size}
#         tar_ctx :: %{?}

# StreamData.ExUnitProperties.property do check all ... do ...
#   - given a generator t(a)
#   - and a function (a -> {:ok, _} | {:error, _})
#   - and options Keyword.t()
#   - runs the test
#     - check runs and time -> {:ok, %{}}
#     - {seed1, seed2} = split_seed(seed)
#     - %{root: root, children: children} = generate(seed1, size)
#     - execute function w/ root -> f.(root)
#       - {:ok, _} -> recur w/ seed2, size + 1, runs + 1, updated_time
#       - {:error, reason} ->
#         - cont = &Enumerable.reduce(children, &1, fn x, acc -> {:suspend, [x | acc]} end)
#         - shrink_failure(cont, parent_cont, smallest, fun, nodes_visited, config)
#           - %{max_shrinking_steps: nodes_visited} = config -> stop
#           - cont.({:cont, []})
#             - {state, _acc} when state in [:halted, :done] and not is_nil(parent_cont)
#               - shrink_failure(parent_cont, nil, _, _, _, _)
#             - {state, _acc} when state in [:halted, :done]
#               - stop
#             - {:suspended, [child], cont}
#               - fun.(child.root)
#                 - {:ok, _}
#                   - shrink_failure(cont, _, _, _, nodes_visited + 1, _)
#                 - {:error, reason}
#                   - c = &Enumerable.reduce(child.children, &1, fn x, acc -> {:suspend, [x | acc]} end)
#                   - shrink_failure(c, cont, reason, _, nodes_visited + 1, _)
#         - {:error, shrinking_result}
#   - returns :ok if successful and raises if not


# => checks(ctx, opts) :: :cont | {:cont, ctx} | {:cont, ctx, opts} |
#                         :done | {:done, _} |
#                         {:error, _}
#           (pre_gen, post_gen, etc.)
#           (i.e. lifecycle hooks)

# => context_init(opts) :: {:ok, ctx} | {:error, _}
# =>        _next(ctx, opts) :: {:ok, ctx, ctx} | {:error, _}

# => generate(gen, ctx) :: {:ok, generated, shrinker} | {:error, _}
# => shrink(cont, parent_cont, smallest, fun, nodes_visited, opts) :: {:ok, %{}} | {:error, %{}}

# NOTE: does the above support TPBT?
#                      symbolic generation?
#                      dynamic generation?

# :proper.quickcheck(outer_test, user_opts) :: result
#   {test, opts} = :proper.peel_test(outer_test, :proper.parse_opts(user_opts))
#   :proper.test({:test, test} = raw_test, opts)
#     :proper.global_state_init(opts)
#     finalizers = :proper.setup_test(raw_test, opts)
#     result = :proper.inner_test(raw_test, opts)
#       test = cook_test(raw_test, opts)
#       imm_result = :proper.parallel_perform(test, opts)
#         :proper.perform(start, to_pass, test, opts)
#           ...
#       :proper.report_imm_result(imm_result, opts)
#       {short_result, long_result} = :proper.get_result(imm_result, test, opts)
#         :proper.shrink(...)
#           ...
#         {:ok, _} ->
#           :proper.clean_testcase(_)
#           :proper.save_counterexample(_)
#         {:error, _} -> report_error(_, _)
#     :ok = :proper.finalize_test(finalizers)
#     :proper.global_state_erase()


defmodule Evig.Gen do
  @moduledoc false
end

defmodule Evig.Fun do
  @moduledoc false
end

defmodule Evig.Symbolic do
  @moduledoc false
end

defmodule Evig.Temporal do
  @moduledoc false
end

###############################################################################
# Side-Effects

defmodule Evig.GroupCommands do
  @moduledoc false
end

defmodule Evig.Statem do
  @moduledoc false
end

defmodule Evig.Fsm do
  @moduledoc false
end

defmodule Evig.Mocking do
  @moduledoc false
end

defmodule Evig.Component do
  @moduledoc false
end

defmodule Evig.Cluster do
  @moduledoc false
end

defmodule Evig.DynamicCluster do
  @moduledoc false
end

###############################################################################

defmodule Evig do
  @moduledoc false
  use Boundary, deps: [], exports: []
end

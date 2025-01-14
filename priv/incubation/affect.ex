defmodule Affect do
  @moduledoc false
  use Boundary, top_level?: true, deps: [], exports: []

  # @type t() :: %{
  #   is: id(),
  #   with:
  # }

  @type id :: atom()
  @type one_or_more(a) :: a | [a]

  # affect-time (incremental, e.g. base env -> 1 env -> ... -> N env)
  #   af, af_env, affects, [parent.affects], af_end, af_one
  #   :joined_env, join, joins, [parent.joins], wrap_joins
  #   merge(_, _)
  # effect-time
  #   ef, ef_env, effects, [parent.effects], ef_end, ef_one
  #   op | op_env + args
  #   fin, finally, [parent.finally], fin_end

  #               -> is, as, op, op_env, args, was
  # join          -> join
  # affectors     -> af, af_env, af_end, af_one
  # effectors     -> ef, ef_env, ef_end, ef_one
  # finally       -> fin, fin_end
  # accumulators  -> joins|affects|effects|finally
  #               -> {child|parent|joined}_env
  # reduced [x]   -> comp_{joins|affects|effects|finally}

  # base
  # void, with, children


  # env
  # (env -> env)
  # (env, env -> env)
  # (... -> env)
  #   (env, args -> env)
  #   (args -> env)

  # mocker <- +s <- +sv
  # mocker.affects
  #   +s.affects, mocker.affects
  #     +sv.affects, +s.affects, mocker.affects

end

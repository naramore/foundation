defmodule Monty do
  @moduledoc """
  """
  use Boundary, deps: [], exports: [Domain]
end

defmodule Monty.Domain do
  @moduledoc """
  """
  use Boundary, deps: [Monty.Glue], exports: :all
end

defmodule Monty.PrimaryAdapters do
  @moduledoc """
  """
  use Boundary, deps: [Monty.{SecondaryAdapters, Domain, Glue}], exports: []
end

defmodule Monty.SecondaryAdapters do
  @moduledoc """
  """
  use Boundary, deps: [Monty.{Domain, Glue}], exports: []
end

defmodule Monty.Glue do
  @moduledoc """
  """
  use Boundary, deps: [], exports: []
end


# ExperimentServer (receives requests for stopping and continuing the experiment)
# EnvironmentServer (receives requests for sensory data, and taking actions within the environment)
#   requests  -> {:act, pid, data}
#             -> {:sense, pid, data}
# MotorSystemServer (receives goals from LMs and raw sensory data from SMs)
#   requests  -> {:goal, pid, goal}
#             -> {:percept, pid, percept}
# SensorModuleServer (receives raw sensory data from environment)
#   requests  -> {:percept, pid, percept}
# LearningModuleServer (receives)
#   requests  -> {:learn, pid, cmp}
#             -> {:bias, pid, cmp}
#             -> {:goal, pid, goal}
#             -> {:vote, pid, vote}



# ExperimentSupervisor (Supervisor)
#   ExperimentServer (GenServer)
#   EnvironmentServer (GenServer)
#     receives  <- {:act, _}
#               <- {:sense, _}
#     send      -> Environment.percept()
#               -> ...
#   AgentSupervisor (Supervisor)
#     AgentServer (GenServer)
#       receives  <- {:sync, _}
#       sends     -> Agent.sync()
#     MotorSystemsSupervisor (DynamicSupervisor)
#       MotorSystemServer (GenServer)
#         receives  <- {:goal, from, goal_state}
#                   <- {:percept, from, env, percept}
#         sends     -> Environment.act(env, %{})
#                   -> Agent.sync(agent, act_output)
#     SensorModulesSupervisor (DynamicSupervisor)
#       SensorModuleServer (GenServer)
#         receives  <- {:percept, from, env, percept}
#                   <- {:sync, _}
#         sends     -> LearningModule.learn(lm, cmp)
#                   -> MotorSystem.goal(ms, goal)
#                   -> Environment.sense(env, %{})
#     LearningModulesSupervisor (DynamicSupervisor)
#       LearningModuleServer (GenServer)
#         receives  <- {:learn, _}
#                   <- {:bias, _}
#                   <- {:goal, _}
#                   <- {:vote, _}
#         sends     -> LearningModule.learn(lm, cmp)
#                   -> MotorSystem.goal(ms, goal)
#                   -> LearningModule.vote(cmp)
#                   -> LearningModule.bias(cmp)
#                   -> LearningModule.goal(goal)

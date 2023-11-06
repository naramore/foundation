defmodule Stepper do
  @moduledoc false

  defstruct state: nil,
            result: nil,
            plugins: %{}
  @type t(s, a) :: %__MODULE__{
    state: s,
    result: a,
    plugins: plugins()
  }
  @type t :: t(any, any)

  @type plugins :: %{
    optional(atom()) => map()
  }
end

defmodule DigraphDocTest do
  use ExUnit.Case, async: true
  doctest Digraph, import: true
end

defmodule DigraphTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import Digraph


end

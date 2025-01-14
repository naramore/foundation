defmodule MemoDocTest do
  use ExUnit.Case, async: true
  doctest Memo, import: true
end

defmodule MemoTest do
  use ExUnit.Case, async: true
  use ExUnitProperties
  import ExUnit.CaptureLog

  describe "verification testing of" do
    @describetag property: :verification_testing
  end

  describe "post-conditions of" do
    @describetag property: :post_conditions
  end

  describe "metamorphic testing of" do
    @describetag property: :metamorphic_testing
  end

  describe "inductive testing of" do
    @describetag property: :inductive_testing
  end

  describe "model-based testing of" do
    @describetag property: :model_based_testing
  end
end

defmodule Mix.Tasks.Incremental do
  @shortdoc "Runs dialyxir w/ incremental."

  @moduledoc """
  This task runs dialyxir but w/o all the PLT management and allowing
  arbitrary

  ## Command line options

  > None at the moment...
  """
  use Boundary, top_level?: true, deps: [], exports: []
  use Mix.Task

  @erlang_apps [:erts, :stdlib, :kernel, :crypto]
  @elixir_apps [:elixir, :logger, :mix, :iex, :eex, :ex_unit]

  defmodule App do
    @shortdoc "Runs incremental dialyxir for app."

    @moduledoc """
    TODO
    """

    use Mix.Task

    def run(args) do
      Mix.Tasks.Incremental.dialyxir_incremental(args, Mix.Tasks.Incremental.app_opts())
    end
  end

  defmodule Elixir do
    @shortdoc "Runs incremental dialyxir for core Elixir."

    @moduledoc """
    TODO
    """

    use Mix.Task

    def run(args) do
      Mix.Tasks.Incremental.dialyxir_incremental(args, Mix.Tasks.Incremental.elixir_core_opts())
    end
  end

  defmodule Erlang do
    @shortdoc "Runs incremental dialyxir for core Erlang."

    @moduledoc """
    TODO
    """

    use Mix.Task

    def run(args) do
      Mix.Tasks.Incremental.dialyxir_incremental(args, Mix.Tasks.Incremental.erlang_core_opts())
    end
  end

  def run(args) do
    dialyxir_incremental(args)
  end

  # NOTE: copied from here: https://github.com/jeremyjh/dialyxir/blob/master/lib/mix/tasks/dialyzer.ex#L261
  #       but w/o the argument coercing (in order to support arbitrary dialyzer opts)
  def dialyxir_incremental(_args, opts \\ app_opts()) do
    {status, exit_status, [time | result]} = Dialyxir.Dialyzer.dialyze(opts)
    Dialyxir.Output.info(time)

    quiet_with_result? = opts[:quiet_with_result]

    report =
      cond do
        status == :ok && quiet_with_result? ->
          fn text ->
            Mix.shell(Mix.Shell.IO)
            Dialyxir.Output.info(text)
            Mix.shell(Mix.Shell.Quiet)
          end

        status == :ok ->
          &Dialyxir.Output.info/1

        true ->
          &Dialyxir.Output.error/1
      end

    Enum.each(result, report)

    unless exit_status == 0 || opts[:ignore_exit_status] do
      Dialyxir.Output.error("Halting VM with exit status #{exit_status}")
      System.halt(exit_status)
    end
  end

  # NOTE: see https://www.erlang.org/blog/otp-26-highlights/#incremental-mode-for-dialyzer
  #       for more details on incremental dialyzer
  def app_opts() do
    [
      analysis_type: :incremental,
      warning_apps: [Mix.Project.config()[:app]],
      apps: all_apps()
    ]
  end

  def elixir_core_opts() do
    [
      analysis_type: :incremental,
      warning_apps: [:elixir],
      apps: elixir_apps() ++ erlang_apps()
    ]
  end

  def erlang_core_opts() do
    [
      analysis_type: :incremental,
      apps: erlang_apps()
    ]
  end

  defp erlang_apps(), do: @erlang_apps
  defp elixir_apps(), do: @elixir_apps

  defp all_apps() do
    Application.loaded_applications()
    |> Stream.map(&elem(&1, 0))
    |> Stream.map(&{&1, :code.lib_dir(&1)})
    |> Stream.reject(&match?({_, {:error, _}}, &1))
    |> Enum.map(&elem(&1, 0))
  end
end

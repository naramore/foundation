defmodule Foundation.MixProject do
  use Mix.Project

  def project do
    [
      app: :foundation,
      version: "0.0.1",
      elixir: "~> 1.15",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        credo: :test,
        dialyzer: :test,
        coveralls: :test,
        muzak: :test
      ],
      dialyzer: [
        plt_core_path: "priv/dialyzer",
        plt_local_path: "priv/dialyzer",
        ignore_warnings: ".dialyzer_ignore.exs",
        list_unused_filters: true
      ]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(env) when env in [:dev, :test], do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:doctor, "~> 0.21", only: [:dev, :test]},
      {:excoveralls, "~> 0.18", only: :test},
      {:stream_data, "~> 0.6", only: [:dev, :test]},
      {:muzak, "~> 1.1", only: :test}
    ]
  end
end

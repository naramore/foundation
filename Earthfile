VERSION 0.7

ARG --global ERLANG_VERSION=26.1.2
ARG --global ELIXIR_VERSION=1.15.7
ARG --global ALPINE_VERSION=3.18.4

all:
  WAIT
    BUILD +fast
  END
  WAIT
    BUILD +slow
  END

fast:
  BUILD +test-coverage
  BUILD +not-dialyzer-checks

slow:
  WAIT
    BUILD +dialyzer
  END
  WAIT
    BUILD +mutation-tests
  END

test:
  BUILD +test-coverage
  BUILD +mutation-tests

checks:
  BUILD +not-dialyzer-checks
  BUILD +dialyzer

not-dialyzer-checks:
  BUILD +check-format
  BUILD +check-unused-deps
  BUILD +check-compile-warnings
  BUILD +lint-docs
  BUILD +lint
  BUILD +audit-deps

dialyzer:
  # TODO: incremental dialyzer https://github.com/jeremyjh/dialyxir/issues/498
  FROM +test-base-compiled
  RUN mkdir -p ./priv/dialyzer
  COPY \
    +dialyzer-plt/dialyxir_erlang-${ERLANG_VERSION}_elixir-${ELIXIR_VERSION}.plt \
    +dialyzer-plt/dialyxir_erlang-${ERLANG_VERSION}.plt \
    +dialyzer-plt/dialyxir_erlang-${ERLANG_VERSION}_elixir-${ELIXIR_VERSION}_deps-${MIX_ENV}.plt.hash \
    +dialyzer-plt/dialyxir_erlang-${ERLANG_VERSION}_elixir-${ELIXIR_VERSION}_deps-${MIX_ENV}.plt \
    ./priv/dialyzer
  COPY .dialyzer_ignore.exs .
  SAVE ARTIFACT priv/dialyzer/* / AS LOCAL priv/dialyzer/
  RUN mix dialyzer --no-check

dialyzer-plt:
  FROM +test-base-compiled
  RUN mkdir -p ./priv/dialyzer
  COPY --if-exists \
    priv/dialyzer/dialyxir_erlang-${ERLANG_VERSION}_elixir-${ELIXIR_VERSION}_deps-${MIX_ENV}.plt.hash \
    priv/dialyzer/dialyxir_erlang-${ERLANG_VERSION}_elixir-${ELIXIR_VERSION}_deps-${MIX_ENV}.plt \
    ./priv/dialyzer
  COPY \
    +dialyzer-core-plt/dialyxir_erlang-${ERLANG_VERSION}_elixir-${ELIXIR_VERSION}.plt \
    +dialyzer-core-plt/dialyxir_erlang-${ERLANG_VERSION}.plt \
    ./priv/dialyzer
  COPY .dialyzer_ignore.exs .
  RUN mix dialyzer --plt
  SAVE ARTIFACT priv/dialyzer/* /

dialyzer-core-plt:
  FROM +test-base-compiled
  RUN mix new plt_cache
  WORKDIR ./plt_cache
  COPY ./priv/core_plt_cache.exs ./mix.exs
  RUN mkdir -p ./priv/dialyzer
  COPY --if-exists priv/dialyzer ./priv/dialyzer
  IF [ ! -f ./priv/dialyzer/dialyxir_erlang-${ERLANG_VERSION}_elixir-${ELIXIR_VERSION}.plt ]
    RUN mix do deps.get, compile, dialyzer --plt
    RUN find ./priv/dialyzer -type f -name '*_deps*' -exec rm -- '{}' +
  END
  SAVE ARTIFACT priv/dialyzer/* /

test-coverage:
  FROM +test-base-compiled
  COPY --if-exists ./coveralls.json .
  RUN mix test --trace --cover --slowest=10

mutation-tests:
  FROM +test-base-compiled
  COPY --if-exists ./.muzak.exs .
  RUN mix muzak

audit-deps:
  FROM +test-base-compiled
  RUN apk --update --upgrade add --no-cache git; \
      mix escript.install --force hex mix_audit
  RUN --no-cache ${MIX_HOME}/escripts/mix_audit

lint-docs:
  FROM +test-base-compiled
  COPY --if-exists .doctor.exs .
  RUN --no-cache mix doctor

lint:
  FROM +test-base-compiled
  COPY --if-exists ./.credo.exs .
  RUN --no-cache mix credo suggest -a --strict

check-format:
  FROM +test-base-with-deps
  COPY --dir lib test ./
  COPY mix.exs mix.lock .formatter.exs .
  RUN --no-cache mix format --check-formatted

check-unused-deps:
  FROM +test-base
  COPY --if-exists --dir config ./
  COPY mix.exs mix.lock .
  RUN cp mix.lock mix.lock.orig && \
      mix deps.get && \
      mix deps.unlock --check-unused && \
      diff -u mix.lock.orig mix.lock && \
      rm mix.lock.orig

test-base-compiled:
  FROM +check-compile-warnings

check-compile-warnings:
  FROM +test-base-compiled-deps
  COPY --if-exists --dir lib ./
  IF [ "${MIX_ENV}" == "test" ]
    COPY --if-exists --dir test ./
  END
  COPY --if-exists config/runtime.exs config/
  RUN mix compile --warnings-as-errors

test-base-compiled-deps:
  FROM +test-base-with-deps
  RUN mkdir config
  COPY --if-exists config/config.exs config/${MIX_ENV}.exs config/
  RUN mix deps.compile

test-base-with-deps:
  FROM +test-base
  COPY mix.exs mix.lock .
  RUN mix deps.get

test-base:
  FROM +elixir-base
  RUN apk add --no-progress --update git build-base
  ENV ELIXIR_ASSERT_TIMEOUT=10000
  ENV MIX_ENV=test
  WORKDIR /work
  RUN mix do local.hex --force, local.rebar --force
  ENV MIX_HOME=/root/.mix

elixir-base:
  FROM hexpm/elixir:${ELIXIR_VERSION}-erlang-${ERLANG_VERSION}-alpine-${ALPINE_VERSION}

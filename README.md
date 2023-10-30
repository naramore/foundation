# Foundation

A collection of data-flow composition techniques that I've read about over the
years in one place and implemented in Elixir.

I've been thinking (off and on) about how to compose these abstractions for
years (in a way that wouldn't be completely obtuse), and I'm pretty
satisfied with the result finally.

## Table of Contents

- [Rails](#rails)
- [Arrow](#arrow)
- [Fix](#fix)
- [Pend and Step](#pend-and-step)
- [Step](#step)

## Rails

[Railway Oriented Programming](https://fsharpforfunandprofit.com/posts/recipe-part2/)

> I would HIGHLY recommend reading the above article or watching this
> [presentation](https://fsharpforfunandprofit.com/rop/) on it instead of
> reading my incomplete and inferior summary of it.

Railway oriented programming is using pipes (i.e. `|>/2`) and two-track
functions (i.e. functions that take an `{:ok, _} | {:error, _}` tuple as input
and return the same as output).

As the pattern of returning `{:ok, _} | {:error, _}` is already very idiomatic
for Elixir, this shouldn't be that hard, right?

So the majority of this module is around providing adapters for functions that
either don't take the correct input or don't return the correct output.

For example, `Rails.bind/1` takes a function that returns
`{:ok, _} | {:error, _}`, but doesn't take it as input, and returns a function
that DOES take an `{:ok, _} | {:error, _}` as input.

```elixir
f = fn x -> {:ok, x + 1} end
bf = Rails.bind(f)

bf.({:ok, 42})
# => {:ok, 43}

bf.({:error, :foo})
# => {:error, :foo}
```

Whereas the `Rails.map/1` does a similar thing for a normal function (i.e. one
that neither takes nor returns `{:ok, _} | {:error, _}`).

```elixir
f = fn x -> x + 3 end
mf = Rails.map(f)

mf.({:ok, 42})
# => {:ok, 45}

mf.({:error, :foo})
# => {:error, :foo}
```

See the documentation in `Rails` for more details on all the adapters provided.

## Arrow

[Arrow TypeClass](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Arrow.html)

For those of you familiar with Haskell typeclasses, you already understand
this abstraction.

> NOTE: this is not a very strict implementation of the arrow typeclass, it's
> primarily just the implementation for functions in order to reduce
> dependencies and make it lighter.

For those of you Haskell virgins, I would HIGHLY recommend reading
[this article](https://en.wikibooks.org/wiki/Haskell/Understanding_arrows) on
understanding arrows.

The primary abstraction that arrows provide for us is that of "splitting" a
computation (via `Arrow.product/2`).

## Fix

[Saga Pattern](https://www.cs.cornell.edu/andru/cs711/2002fa/reading/sagas.pdf)

As stated very succintly in [sage](https://github.com/Nebo15/sage)'s docs:

> It’s like `Ecto.Multi` but across business logic and third-party APIs.
>
> -- [@jayjun](https://github.com/jayjun)

Each stage is considered to have side-effects and consists of the action itself,
and it's compensating action. It's important to note that the purpose of the
compensation is to revert the side-effects of it's action.

It's a way of composing side-effectful functions with their
compensation (i.e. a function that will "undo" the original effects).

To visualize it, let's imagine we have a 4-step transaction. Successful
execution flow would look like:

```
[T1] -> [T2] -> [T3] -> [T4]
```

and if we get a failure on 3-d step, Sage would cleanup side effects by
running compensation functions:

```
[T1] -> [T2] -> [T3 has an error]
                ↓
[C1] <- [C2] <- [C3]
```

## Pend and Step

Interruptible Computation as seen in this [stackoverflow](https://stackoverflow.com/questions/10236953/the-pause-monad).

> NOTE: my only wishlist item this doesn't fullfil is a purely data
> representation of a suspended computation (e.g. for storing remotely and
> optionally editting later), but the simplicity of this abstraction is
> definately superior to the complexity of past implementations.

### Step

Combination of [Pend](#pend) and [diet](https://github.com/pragdave/diet).

After watching [Dave Thomas' Transforming Programming presentation](https://youtu.be/A76hM3MpEKo?si=anQ_DmqvemPOS1H3&t=1668s),
I found the idea of his stepper compelling. This led me to attempting many partial impelementations that supported extra features (e.g. parallelism, and many of the
above features).

After researching dataflow composition for years, I came across this idea of 
"interruptible" computation. This seemed to be a huge part of what his stepper
is doing.

#### TODO

- [x] create bind that auto yields normal function
- [x] replicate reductions macros as seen in diet
- [x] adapt reductions to work with `Pend`
- [ ] create stepper struct for tracking model, last result, and context
- [ ] integrate stepper struct w/ pause monad state
- [ ] capture the return at each yield
- [ ] support plugins
  - [ ] recording history / debug
  - [ ] stage id / cursor tracking

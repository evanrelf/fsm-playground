# fsm-playground

WORK IN PROGRESS, JUST HAVING FUN.

## Tour

- [Workflow.Core](./src/Workflow/Core.hs): The core state machine API. States,
  inputs, transitions, and outputs/effects. The essence of it fits on a
  flashcard:

  ```haskell
  class Workflow w where
    transitionRaw :: w f i o -> i -> f o

  data State w a

  getState :: State w a -> a

  initialize :: (Workflow w, Functor f) => w f () o -> f (State w o)

  transition :: (Workflow w, Functor f) => w f i o -> State w i -> f (State w o)
  ```

  This is all you need to define and run state machines:

  ```haskell
  -- States
  data Locked = Locked
  data Unlocked = Unlocked

  -- Transitions
  data Turnstile f i o where
    Init :: Turnstile Identity () Locked
    Push :: Turnstile Identity Unlocked Locked
    Coin :: Turnstile Identity Locked Unlocked

  -- Logic
  instance Workflow Turnstile where
    transitionRaw = \case
      Init -> \() -> pure Locked
      Push -> \Unlocked -> pure Locked
      Coin -> \Locked -> pure Unlocked

  _exampleTurnstile :: Identity (State Turnstile Locked)
  _exampleTurnstile =
        initialize Init
    >>= transition Coin
    >>= transition Push
  ```

  Stop here if you don't care about introspection stuff.

- [Workflow.Abstract](./src/Workflow/Abstract.hs): A more abstract state machine
  API. Doesn't use real values for states or inputs, and doesn't perform
  effects.

  Provides meta information about states and transitions for modeling purposes
  (e.g. running simulations, writing property tests, generating pretty diagrams,
  etc.).

  In the future, you'll be able to cash in any `Workflow` for a free
  `AbstractWorkflow` instance using Template Haskell.

- [Workflow.Abstract.TH](./src/Workflow/Abstract/TH.hs): Template Haskell to
  generate `AbstractWorkflow` instance boilerplate (work in progress).

- [Workflow.Info](./src/Workflow/Info.hs): Plain data describing a state
  machine. This info is available for all abstract workflows, but you can also
  generate and manipulate this however you like, independent of all the other
  code here.

  Also provides `ToGraph` instances, so you can do all sorts of cool graph
  things with the `algebraic-graphs` library, including generating GraphViz DOT
  files with the `dot` function.

- [Workflow](./src/Workflow.hs): The main module you'd export. Mostly re-exports
  all the previous modules, but also defines an umbrella `Workflow` concept for
  state machines with both concrete and abstract representations.

- [Workflow.Example.*](./src/Workflow/Example/): Examples of different kinds of
  state machines and their associated code. Not all implement `Workflow` and
  `AbstractWorkflow`.

- [Main](./src/Main.hs): Tiny executable that prints the GraphViz DOT
  representation of the `Xyz` example state machine / workflow for demonstration
  purposes.

- I'm generating Haddocks manually and uploading them
  [here](https://s3.evanrelf.com/fsm-playground/). Last updated 2024-06-30 at
  5 PM PT.

## Developing

1. `$ nix develop`
2. `$ ghcid`
3. `$ cabal build && cabal exec fsm-playground | dot -Tpng -o xyz.png && open xyz.png`

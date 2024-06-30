# fsm-playground

WORK IN PROGRESS, JUST HAVING FUN.

## Tour

- [Workflow.Concrete](./src/Workflow/Concrete.hs): The core state machine API.
  States, inputs, transitions, and outputs/effects.

  This is all you need to define and run real state machines. Stop here if you
  don't care about introspection stuff.

- [Workflow.Abstract](./src/Workflow/Abstract.hs): A more abstract state machine
  API. Doesn't use real values for states or inputs, and doesn't perform
  effects.

  Provides meta information about states and transitions for modeling purposes
  (e.g. running simulations, writing property tests, generating pretty diagrams,
  etc.).

  In the future, you'll be able to cash in any `ConcreteWorkflow` for a free
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
  state machines and their associated code. Not all implement `AbstractWorkflow`
  and `ConcreteWorkflow`.

- [Main](./src/Main.hs): Tiny executable that prints the GraphViz DOT
  representation of the `Xyz` example state machine / workflow for demonstration
  purposes.

- I'm generating Haddocks manually and uploading them
  [here](https://s3.evanrelf.com/fsm-playground/). Last updated 2024-06-30 at
  5 PM PT.

## Developing

1. `$ nix develop`
2. `$ ghcid Main.hs`
3. `$ cabal build && cabal exec fsm-playground | dot -Tpng -o xyz.png && open xyz.png`

# fsm-playground

WORK IN PROGRESS, JUST HAVING FUN.

---

- [src/ConcreteWorkflow.hs](./src/ConcreteWorkflow.hs): The core state machine
  API.

  This is all you need to define and run real state machines. Stop here if you
  don't care about introspection stuff.

- [src/AbstractWorkflow.hs](./src/AbstractWorkflow.hs): A more abstract state
  machine API.

  Provides meta information about states and transitions for
  modeling purposes (e.g. running simulations, writing property tests,
  generating pretty diagrams, etc.).

  In the future, you'll be able to cash in any `ConcreteWorkflow` for a free
  `AbstractWorkflow` instance using Template Haskell.

- [src/Info.hs](./src/Info.hs): Plain data describing a state machine. This info
  is available for all abstract workflows, but you can also generate and
  manipulate this however you like, independent of all the other code here.

- [src/Graph.hs](./src/Graph.hs): Converts the state machine info described
  above into a graph data structure. You can do all sorts of cool graph things
  with the `algebraic-graphs` library, including generating GraphViz DOT files
  with the `dot` function.

- [src/TH.hs](./src/TH.hs): Template Haskell to generate `AbstractWorkflow`
  instance boilerplate (work in progress).

- [src/Workflow.hs](./src/Workflow.hs): The main module you'd export. Mostly
  re-exports all the previous modules, but also defines an umbrella `Workflow`
  concept for state machines with both concrete and abstract representations.

- [src/Example.hs](./src/Example.hs): Lots of examples (pretty trivial ones so
  far) of state machines and their associated code.

- [src/Main.hs](./src/Main.hs): Tiny executable that prints the GraphViz DOT
  representation of the `Xyz` example state machine / workflow for demonstration
  purposes.

---

1. `$ nix-shell`
2. `$ cd src/`
3. `$ ghcid Main.hs`
4. `$ runghc Main.hs | dot -Tpng -o xyz.png && open xyz.png`

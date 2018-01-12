# free-agent

[![Build Status](https://secure.travis-ci.org/fizruk/free-agent.png?branch=master)](http://travis-ci.org/fizruk/free-agent)

This package provides a general framework for multi agent systems.
Agent-environment interface is implemented using free monads.

## About this project

NOTE: the project has been stale for quite a while since I didn't have much time to invest into it.

`free-agent` is a proof-of-concept for developing multi-agent systems in Haskell using so called "free monads".
I would consider this to be a relatively low-level (logically speaking) platform for designing MAS,
since it does not implement high-level logic stuff (like BDI-model).

I apologize for the terrible lack of documentation, this was one of my early projects :(
I might come back at this project later at least to update it with some proper docs.

The key idea of the project is to completely isolate agent program from the environment and general I/O
by introducing an explicit agent interface (as a datatype).
Having agent's program as an (almost) first-class value, we can:
- execute agent in completely different environments without changing any agent's code;
- reliably reproduce agent's actions (since all side effects are controlled by the environment); this is crucial for
  - reproducible test scenarios and
  - reproducing logged MAS actions;
- automatically add debug-printing or distributed logging (see below);
- shuffle, re-order or parallelise actions (for efficiency or to model unstable environments);
- substitute agent's sensors/actors with different ones
  - to reuse generic agent algorithms (e.g. see [`Control.Agent.Free.Algorithms.ABT`](http://fizruk.github.io/free-agent/docs/Control-Agent-Free-Algorithms-ABT.html));
  - to build fractal (holonic) MAS (I have no example of this, unfortunately, but you can refer to [Holonic Multiagent Systems: A Foundation for the Organisation of Multiagent Systems for the idea of holonic MAS](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.14.1097&rep=rep1&type=pdf)).

In this project I also experimented with making a part of agent's internal logic accessible to the environment.
Basically, I allow a single context (monad transformer) to be sliced in between agent's sensors and actions and visible to an executing environment.
This tweak allows for the implementation of [Behaviosites](https://drive.google.com/file/d/0B1vPVqheIpiPV1lIRVd5OU5iWVk/view?usp=sharing) (but again, I don't have a working example).

Another interesting idea I have looked at is generic log & replay for MAS. Some work on that is available at https://github.com/fizruk/replay-free. The idea is to get a generic mechanism to record possibly branching (parallel/distributed) execution in terms of agent's sensors and actions. And then to replay those sensors and actions (possibly in a different environment). This generic mechanism could significantly improve debugging distributed behaviour, such as any possible deadlocks, races and such.
The usage of replay-free boils down to two functions: [`record` and `replay`](https://github.com/fizruk/replay-free/blob/master/src/Control/Monad/Trans/Free/Replay.hs). There's even [an example with concurrent program](https://github.com/fizruk/replay-free/blob/master/examples/AskForkHalt.hs) being logged and replayed.

Haskell has been a great language for this project:
- Haskell's purity allows us to enforce agent's interface, disallowing any extra effects;
- Haskell's do-notation allows us to write agent programs cleanly and imperatively (Haskell is the best imperative language!);
- Haskell's purity combined with expressive type system allows to write powerful and reusable code (e.g. reusable MAS algorithms);
- free monads and Template Haskell allow to easily create agent's low-level eDSL for sensors/actors.

Some obvious drawbacks of the project are:
- lack of documentation;
- lack of powerful examples/demos with more sophisticated agents and envrionments;
- incomplete feature set (need more prebuilt environments, interfaces, logging mechanisms, etc.).

## Installation

You can simply clone this repository and run `cabal-install`:

```
$ git clone https://github.com/fizruk/free-agent.git
$ cd free-agent
$ cabal install
```

## Documentation

Haddock documentation is available at http://fizruk.github.io/free-agent/docs/

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

Contributions and bug reports are welcome!

Please feel free to contact me via GitHub or on the #haskell IRC channel on irc.freenode.net.

_Nickolay Kudasov_

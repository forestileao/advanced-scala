# advanced-scala

### This is not a project repository!

This repository is only a collection of Advanced Scala exercises and lecture notes. Each file is a small, self-explanatory unit that you can read or run on its own.

## Stack

- Scala 3.2.2 (`build.sbt`)
- sbt 1.8.2 (`project/build.properties`)

## Structure

Everything lives under `src/main/scala`.

- `lectures/part1`
  - `AdvancedPatternMatching.scala`: custom `unapply` and `unapplySeq` extractors, infix patterns, sequence decomposition, and custom `unapply` return types. Includes small exercises with `even` and `singleDigit`.
  - `DarkSugars.scala`: Scala syntax sugar: single-argument methods with blocks, single abstract method lambdas, and the special associativity of `::` and right-associative methods ending in `:`.
  - `Recap.scala`: a recap of the basics: expressions, recursion, OOP, generics, case classes, exceptions, functions, collections, and pattern matching.
- `lectures/part2`
  - `FunctionalCollections.scala`: an empty `App` stub (in progress).
  - `PartialFunctions.scala`: partial functions, with `isDefinedAt`, `lift`, `orElse`, and use in higher-order functions. Exercises: a hand-written `PartialFunction` and a chatbot that reads lines from stdin.
- `exercises`
  - `MySet.scala`: a custom `MySet[A]` built on the function type `A => Boolean`, with `EmptySet`, `NonEmptySet`, `AllInclusiveSet`, and `ProperyBasedSet` implementations. In progress: several methods are still unimplemented (`???`).
- `playground`
  - `ScalaPlayground.scala`: a Scala `App` for quick experiments.
  - `JavaPlayground.java`: a Java `main` class for quick experiments.

## Usage

Compile everything:

    sbt compile

Run a single lecture. Each lecture is an `object` that extends `App`, so you can run it by its fully qualified name:

    sbt "runMain lectures.part1.Recap"

`lectures.part2.PartialFunctions` waits for lines on stdin, so give it input when you run it.

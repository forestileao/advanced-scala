# Advanced Scala Exercises

## Overview
Small learning repository for advanced Scala topics and exercises. Not a production app—just self‑contained examples and practice code.

## Stack
- Scala: 3.2.2 (from build.sbt)
- sbt: 1.8.2 (from project/build.properties)

## Project Structure
Main sources live under src/main/scala/:
- src/main/scala/exercises/MySet.scala — immutable set exercise with a property‑based complement; several methods intentionally left as ??? for practice; includes a small playground.
- src/main/scala/playground/JavaPlayground.java — trivial Java entry printing a greeting.
- src/main/scala/playground/ScalaPlayground.scala — trivial Scala entry printing a greeting.
- src/main/scala/lectures/part1/AdvancedPatternMatching.scala — custom extractors, infix patterns, unapplySeq, and wrapper types.
- src/main/scala/lectures/part1/DarkSugars.scala — syntax sugars: SAM conversions, right‑associative ops, multi‑word names, update, setters, etc.
- src/main/scala/lectures/part1/Recap.scala — brief recap of Scala syntax: functions, OOP, exceptions, generics.
- src/main/scala/lectures/part2/FunctionalCollections.scala — stub for later functional collections material.
- src/main/scala/lectures/part2/PartialFunctions.scala — partial‑function utilities and a tiny stdin chatbot (reads lines from stdin).

## Usage
From the repository root:
- Compile: sbt compile
- Run an example (sbt may prompt to choose a main if multiple are found):
  - sbt run
  - or run a specific main, e.g.:
    - sbt "runMain lectures.part1.Recap"
    - sbt "runMain lectures.part2.PartialFunctions" (reads from stdin; press Ctrl+D to end)

## Status
- Some exercise methods are intentionally unimplemented (marked ???) in MySet.scala and related types (e.g., AllInclusiveSet).
- FunctionalCollections.scala is a placeholder stub.
- PartialFunctions.scala reads from standard input when executed.

## License
No explicit license file is present in this repository.


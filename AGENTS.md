# AGENTS.md

## Purpose
Exercises for advanced Scala, one lecture topic per file (see README.md).
This is a learning repository, not a product. Each file is self-contained.

## Layout
- src/main/scala/lectures/part1: Recap, AdvancedPatternMatching, DarkSugars
- src/main/scala/lectures/part2: FunctionalCollections, PartialFunctions
- src/main/scala/exercises: exercises, e.g. MySet.scala
- src/main/scala/playground: scratch code (JavaPlayground.java, ScalaPlayground.scala)
- build.sbt, project/build.properties: build definition
- .idea, .bsp: IDE and build server files. Do not edit them.

## Toolchain
- Scala 3.2.2 (build.sbt)
- sbt 1.8.2 (project/build.properties)
- A single root project named scala-advanced. No library dependencies.

## Commands
- Compile everything: sbt compile
- Run one file: sbt "runMain <fully qualified object>"
  Example: sbt "runMain lectures.part1.Recap"
- There is no src/test directory, so no test suite exists.

## Code style (.editorconfig)
- Two-space indent, no tabs
- LF line endings
- UTF-8
- Trim trailing whitespace
- End every file with a newline
- Use Scala 3 syntax where the code already does, e.g. `if ... then`.

## Conventions
- Put new lecture code in the matching lectures/partN folder.
- Keep each file self-contained and runnable.
- Do not add tooling the repository lacks.

## Known state
Some methods in src/main/scala/exercises/MySet.scala are still `???`.
They are unfinished. Do not call them in a way that throws.

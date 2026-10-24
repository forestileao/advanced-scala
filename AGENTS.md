# AGENTS.md

## Purpose
This is a scratch repo of advanced Scala exercises, not an application. Each file is a self-contained lesson.

## Layout
- src/main/scala/lectures/part1: lessons (Recap, AdvancedPatternMatching, DarkSugars)
- src/main/scala/lectures/part2: lessons (FunctionalCollections, PartialFunctions)
- src/main/scala/exercises: exercises, e.g. MySet.scala
- src/main/scala/playground: experiments, including a Java file (JavaPlayground.java)

## Toolchain
- Scala 3.2.2 (build.sbt)
- sbt 1.8.2 (project/build.properties)
- No dependencies and no test sources.

## Commands
- sbt compile
- sbt "runMain lectures.part1.Recap" runs a lesson. Lessons use `extends App`.

## Conventions
From .editorconfig: two-space indent, LF line endings, UTF-8, trim trailing whitespace, final newline.
Code uses Scala 3 syntax such as `if ... then`.

## Rules
- Put new lessons in the matching lectures package.
- Keep each file self-contained and runnable.
- Do not edit .idea, .bsp, or metals files.

## Known state
Some methods in exercises/MySet.scala are still `???`. They are unfinished by design. Do not call them in a way that throws.

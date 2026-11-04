# AGENTS.md

## 1) Purpose
Guidance for AI coding agents working in this repository. Covers structure, conventions, how to build/run locally, common tasks, safety, and review expectations.

## 2) Repo Structure
-  — sbt project definition (Scala 3.2.2, sbt 1.8.x).
-  — sbt meta build (, metals settings).
-  — source code.
  -  — practice code and katas.
  - ,  — lecture/demo code.
  -  — ad‑hoc runnable snippets.
-  — high‑level notes.
-  — formatting defaults (2‑space indent, LF, UTF‑8).

## 3) Conventions
- Prefer small, pure functions and immutable data for exercises.
- Match file names to public types (e.g.,  for ).
- Keep package paths aligned with folders under .
- Do not introduce new build tools; use sbt only.
- Follow : 2 spaces, UTF‑8, trailing newline, trim whitespace.
- Keep runnable demos in ; keep exercises under .
- Avoid unnecessary dependencies; prefer standard library where possible.

## 4) Build & Run
Prereqs: JDK 11+ (JDK 17 recommended), sbt 1.8.x.
- Compile: 
- REPL: 
- Run:  (when a / entry exists)
- Clean: 
- Tests: none are configured; do not add new frameworks without request.

## 5) Tasks
- Add a new exercise: create a file in  and keep it self‑contained.
- Add lecture examples: mirror existing layout under  or .
- Try‑outs: place scratch code in  and avoid committing large binaries.

## 6) Safety
- Do not commit secrets or credentials.
- Respect  (e.g., , IDE files, metals/bloop artifacts).
- Avoid changing sbt/Scala versions unless explicitly requested.

## 7) Review
- Keep PRs small and focused on a single task.
- Ensure  succeeds locally.
- Describe what changed and how to validate. Include any known limitations.

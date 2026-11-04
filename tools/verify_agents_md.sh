#!/usr/bin/env bash
set -euo pipefail

repos=(
  "/home/node/repo"
  "/home/node/context/forestileao/bd_games"
)
required=(
  "^# AGENTS.md$"
  "^## 1) Purpose$"
  "^## 2) Repo Structure$"
  "^## 3) Conventions$"
  "^## 4) Build & Run$"
  "^## 5) Tasks$"
  "^## 6) Safety$"
  "^## 7) Review$"
)

pass=true
for r in ""; do
  file="/AGENTS.md"
  if [[ ! -f "" ]]; then
    echo "[FAIL] Missing: "
    pass=false
    continue
  fi
  missing=0
  for rx in ""; do
    if ! grep -Eq "" ""; then
      echo "[FAIL]  missing heading: "
      missing=1
    fi
  done
  if [[  -eq 0 ]]; then
    echo "[OK]  contains all required sections"
  else
    pass=false
  fi
done

 && exit 0 || exit 1

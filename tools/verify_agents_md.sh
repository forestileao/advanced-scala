#!/usr/bin/env bash
set -euo pipefail

check_repo() {
  r=""
  f="/AGENTS.md"
  if [ ! -f "" ]; then
    echo "[FAIL] Missing: "
    return 1
  fi
  ok=1
  grep -Eq "^# AGENTS.md$" "" || { echo "[FAIL]  missing heading: ^# AGENTS.md$"; ok=0; }
  grep -Eq "^## 1) Purpose$" "" || { echo "[FAIL]  missing heading: ^## 1) Purpose$"; ok=0; }
  grep -Eq "^## 2) Repo Structure$" "" || { echo "[FAIL]  missing heading: ^## 2) Repo Structure$"; ok=0; }
  grep -Eq "^## 3) Conventions$" "" || { echo "[FAIL]  missing heading: ^## 3) Conventions$"; ok=0; }
  grep -Eq "^## 4) Build & Run$" "" || { echo "[FAIL]  missing heading: ^## 4) Build & Run$"; ok=0; }
  grep -Eq "^## 5) Tasks$" "" || { echo "[FAIL]  missing heading: ^## 5) Tasks$"; ok=0; }
  grep -Eq "^## 6) Safety$" "" || { echo "[FAIL]  missing heading: ^## 6) Safety$"; ok=0; }
  grep -Eq "^## 7) Review$" "" || { echo "[FAIL]  missing heading: ^## 7) Review$"; ok=0; }
  if [  -eq 1 ]; then
    echo "[OK]  contains all required sections"
    return 0
  else
    return 1
  fi
}

pass=0
check_repo "/home/node/repo" || pass=1
check_repo "/home/node/context/forestileao/bd_games" || pass=1
exit 

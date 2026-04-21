#!/usr/bin/env bash
set -euo pipefail

END_YEAR=2024

EXPECTED=$(cat <<EOF
(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-${END_YEAR} OCamlPro *)
(* Written by the Owi programmers *)
EOF
)

if [ $# -eq 0 ]; then
  echo "No files to check."
  exit 0
fi

missing=()
for file in "$@"; do
  case "$file" in
    *.ml|*.mli) ;;
    *) continue ;;
  esac
  [ -f "$file" ] || continue
  actual=$(head -n 3 "$file")
  if [ "$actual" != "$EXPECTED" ]; then
    missing+=("$file")
  fi
done

if [ ${#missing[@]} -gt 0 ]; then
  echo "The following new files have a missing or incorrect license header:"
  printf '  %s\n' "${missing[@]}"
  echo
  echo "Expected header:"
  echo "${EXPECTED}" | sed 's/^/  /'
  exit 1
fi

echo "All new .ml and .mli files have a correct license header."

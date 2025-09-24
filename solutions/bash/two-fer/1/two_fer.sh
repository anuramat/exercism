#!/usr/bin/env bash

main() {
  local arg="$1"
  [ -z "$arg" ] && arg="you"
  echo "One for $arg, one for me."
}

main "$@"

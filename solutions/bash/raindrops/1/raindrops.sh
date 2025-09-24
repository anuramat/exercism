#!/usr/bin/env bash

main() {
  local arg=$1
  local result=""
  ((arg % 3 == 0)) && result="$result"Pling
  ((arg % 5 == 0)) && result="$result"Plang
  ((arg % 7 == 0)) && result="$result"Plong
  [ -z "$result" ] && result="$arg"
  echo "$result"
}

main "$1"

#!/bin/bash

echo "module Daffy.Lenses exposing (..)"

while read S; do
  # Ignore whitespace

  if [[ -z "$S" ]]; then
    continue
  fi

  # Ignore lines beginning with '#'

  if [[ "${S:0:1}" == '#' ]]; then
    continue
  fi

  # For each line 'foo', generate an 'overFoo' and a 'setFoo'

  echo ""
  echo "over${S^} : (a -> a) -> { r | $S : a } -> { r | $S : a }"
  echo "over${S^} f x ="
  echo "    { x | $S = f x.$S }"

  echo ""
  echo "set${S^} : a -> { r | $S : a } -> { r | $S : a }"
  echo "set${S^} x s ="
  echo "    { s | $S = x }"
done

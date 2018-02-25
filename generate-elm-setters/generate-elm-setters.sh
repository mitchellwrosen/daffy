#!/bin/bash

echo "module Daffy.Setters exposing (..)"
echo ""
echo "over : ((a -> b) -> s -> t) -> (a -> b) -> s -> t"
echo "over = identity"
echo ""
echo "set : ((a -> b) -> s -> t) -> b -> s -> t"
echo "set l x = over l (always x)"

while read S; do
  # Ignore whitespace

  if [[ -z "$S" ]]; then
    continue
  fi

  # Ignore lines beginning with '#'

  if [[ "${S:0:1}" == '#' ]]; then
    continue
  fi

  # For each line 'foo', generate a setter 'fooS'.

  echo ""
  echo "${S}S : (a -> b) -> { r | $S : a } -> { r | $S : b }"
  echo "${S}S f x = { x | $S = f x.$S }"
done

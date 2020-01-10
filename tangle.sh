#!/usr/bin/env bash
# Change to where this file lives
cd "$(dirname "$0")"

# Use Emacs to tangle the files.
tangle() {
    emacs --batch -l org --eval "(org-babel-tangle-file \"$1\")"
}


tangle_files=("README" "source-code" "custom" "lexical" "expressions"
              "statements-and-declarations" "functions-and-classes"
              "scripts-and-modules" "js-syntax")

# Tangle the files! 

## printf '%s\0' "${tangle_files[@]}" | xargs -0 -n1 tangle ;; # does not work

for f in ${tangle_files[@]}; do
 tangle "${f}.org"
done

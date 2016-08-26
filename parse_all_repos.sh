#!/bin/bash
# Run our parser on all the footprint files we found so far GitHub
set -eu
set -o pipefail
test_dir=dist/build
temp_dir=$test_dir/parse-tmp
test_executable=$test_dir/parse
root_dir=$(pwd)

mkdir -p $test_dir

if [ ! -d "$temp_dir" ]; then
  git clone --depth=1 "https://github.com/kasbah/kicad_footprints" "$temp_dir"
  cd "$temp_dir" && ./init && ./update
else
  cd "$temp_dir" && ./update
fi
cd "$root_dir"

echo "Compiling."
if [[ -v TRAVIS ]]; then
  ghc tests/Parse.hs -tmpdir "$temp_dir" -o "$test_executable"
else
  cabal exec -- ghc tests/Parse.hs -tmpdir "$temp_dir" -o "$test_executable"
fi

echo "Running parse on all files."
find "$temp_dir/" -name "*.kicad_mod" -print0 | xargs -0 -P 2 "$test_executable" > /dev/null

if [ $? -eq 0 ]
then echo "- PARSE SUCCEEDED -";
else echo "- PARSE FAILED -" && exit 2;
fi

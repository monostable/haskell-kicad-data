#!/bin/bash
# Run our parser on all the footprint files we found so far GitHub
set -eu
set -o pipefail
test_dir=dist/build
temp_dir=$test_dir/parse-tmp
kicad_mod_dir=$temp_dir/kicad-data
test_executable=$test_dir/parse
root_dir=$(pwd)

mkdir -p $test_dir
mkdir -p $temp_dir

if [ ! -d "$kicad_mod_dir" ]; then
  git clone --depth=1 "https://github.com/kasbah/kicad_footprints" "$kicad_mod_dir"
  cd "$kicad_mod_dir" && ./init && ./update
else
  cd "$kicad_mod_dir" && ./update
fi
cd "$root_dir"

echo "Compiling."
if [[ -v TRAVIS ]]; then
  ghc tests/Parse.hs -tmpdir "$temp_dir" -o "$test_executable"
else
  cabal exec -- ghc tests/Parse.hs -tmpdir "$temp_dir" -o "$test_executable"
fi

mod_files=$(find "$temp_dir/" -name "*.kicad_mod")
echo "Running parse on $(echo "${mod_files}" | wc -l) kicad_mod files"
find "$temp_dir/" -name "*.kicad_mod" -print0 | xargs -0 -P 2 "$test_executable" > /dev/null

if [ $? -eq 0 ]; then
  echo "- PARSE SUCCEEDED -"
else
  echo "- PARSE FAILED -"
  exit 2
fi

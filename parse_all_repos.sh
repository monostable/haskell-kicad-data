#!/bin/bash
# Run our parser on all the footprint files we found so far GitHub
set -eu
set -o pipefail
set -o verbose
test_dir=dist/build
temp_dir=$test_dir/parse-tmp
kicad_mod_dir=$temp_dir/kicad-data
kicad_mod_output_dir=$temp_dir/mod-output
test_executable=$test_dir/parse
root_dir=$(pwd)

mkdir -p $test_dir
mkdir -p $temp_dir

if [ ! -d "$kicad_mod_dir" ]; then
  git clone "https://github.com/kasbah/kicad_footprints" "$kicad_mod_dir"
  cd "$kicad_mod_dir"
  ./init
  ./update
else
  cd "$kicad_mod_dir"
  ./update
fi
cd "$root_dir"

echo "Compiling."
if [[ -v TRAVIS ]]; then
  ghc tests/Parse.hs -tmpdir "$temp_dir" -o "$test_executable"
else
  cabal exec -- ghc tests/Parse.hs -tmpdir "$temp_dir" -o "$test_executable"
fi

mod_files=$(find "$kicad_mod_dir/" -name "*.kicad_mod")
echo "Running parse on $(echo "${mod_files}" | wc -l) kicad_mod files"
IFS=$'\n'
for f in $mod_files; do
  out=$(echo "$f" | sed "s!$kicad_mod_dir!$kicad_mod_output_dir!")
  mkdir -p "$out"
  $test_executable "$f" > "$out/file.kicad_mod" &
done

wait

if [ $? -eq 0 ]; then
  echo "- PARSE SUCCEEDED -"
else
  echo "- PARSE FAILED -"
  exit 2
fi

python kicad_footprint_load.py "$kicad_mod_output_dir"

if [ $? -eq 0 ]; then
  echo "- LOAD SUCCEEDED -"
else
  echo "- LOAD FAILED -"
  exit 3
fi

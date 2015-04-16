#!/bin/bash
TEST_DIR=dist/build
TEMP_DIR=$TEST_DIR/parse-tmp
TEST_EXE=$TEST_DIR/parse

mkdir -p $TEMP_DIR $TEST_DIR

echo "Cloning repos if needed."
for repo in $(cat tests/kicad_mod_repo_list)
do if [ ! -d "$TEMP_DIR/$repo" ]
    then git clone --depth=1 "https://github.com/KiCad/$repo" "$TEMP_DIR/$repo" || exit 1;
   fi
done;

echo "Compiling."
cabal exec -- ghc tests/Parse.hs -tmpdir "$TEMP_DIR" -o "$TEST_EXE"

echo "Running parse on all files."
find "$TEMP_DIR/" -name "*.kicad_mod" -print0 | xargs -0 "$TEST_EXE" > /dev/null

if [ $? -eq 0 ]
then echo "- PARSE SUCEEDED -";
else echo "- PARSE FAILED -" && exit 2;
fi

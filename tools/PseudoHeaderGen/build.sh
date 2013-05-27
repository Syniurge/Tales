#!/bin/bash

# Rather hackish way to build TalesPseudoHeaderGen, but I couldn't find a proper way with CMake
# Must be run with env PATH_TO_LLVM_SOURCE="" PATH_TO_LLVM_BUILD="" OUTPUT_DIR="" ./build.sh

if [ -z "$PATH_TO_LLVM_SOURCE" ]
then
	echo "Must be run with 'env PATH_TO_LLVM_SOURCE=... PATH_TO_LLVM_BUILD=... OUTPUT_DIR=... build.sh'"
	exit 1
fi

EXTRA_DIR="$PATH_TO_LLVM_SOURCE/tools/clang/tools/extra"

# Every time the SVN reverts Clang source code to the repo version, we have to re-add the line to CMakeLists.txt
if ! grep -q "TalesPseudoHeaderGen" "$EXTRA_DIR/CMakeLists.txt"
then
	echo 'add_subdirectory(TalesPseudoHeaderGen)' >> "$EXTRA_DIR/CMakeLists.txt"
fi

if [ ! -d "$EXTRA_DIR/TalesPseudoHeaderGen" ]
then
	mkdir "$EXTRA_DIR/TalesPseudoHeaderGen"
fi

PSEUDOHEADERGEN_SOURCE_DIR="$( cd -P "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

[ ! -e "$EXTRA_DIR/TalesPseudoHeaderGen/CMakeLists.txt" ] && ln -s "$PSEUDOHEADERGEN_SOURCE_DIR/CMakeLists.txt.in" "$EXTRA_DIR/TalesPseudoHeaderGen/CMakeLists.txt"
[ ! -e "$EXTRA_DIR/TalesPseudoHeaderGen/PseudoHeaderGen.cpp" ] && ln -s "$PSEUDOHEADERGEN_SOURCE_DIR/PseudoHeaderGen.cpp" "$EXTRA_DIR/TalesPseudoHeaderGen/"
[ ! -e "$EXTRA_DIR/TalesPseudoHeaderGen/DeclPrinter.hpp" ] && ln -s "$PSEUDOHEADERGEN_SOURCE_DIR/DeclPrinter.hpp" "$EXTRA_DIR/TalesPseudoHeaderGen/"
[ ! -e "$EXTRA_DIR/TalesPseudoHeaderGen/TypePrinter.hpp" ] && ln -s "$PSEUDOHEADERGEN_SOURCE_DIR/TypePrinter.hpp" "$EXTRA_DIR/TalesPseudoHeaderGen/"

cd "$PATH_TO_LLVM_BUILD"
cmake "$PATH_TO_LLVM_SOURCE"

cd "$PATH_TO_LLVM_BUILD/tools/clang/tools/extra"
make

ln -sf  "$PATH_TO_LLVM_BUILD/bin/TalesPseudoHeaderGen" "$OUTPUT_DIR/"
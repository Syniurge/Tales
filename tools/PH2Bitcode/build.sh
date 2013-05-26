#!/bin/bash

# Must be run with env PATH_TO_LLVM_SOURCE="" PATH_TO_LLVM_BUILD="" CURRENT_DIR="" OUTPUT_DIR="" ./build.sh
# Setting CLEAN to 1 reverts the modifications in the LLVM source tree instead of building

if [ -z "$PATH_TO_LLVM_SOURCE" ]
then
	echo "Must be run with 'env PATH_TO_LLVM_SOURCE=... PATH_TO_LLVM_BUILD=... CURRENT_DIR=... OUTPUT_DIR=... build.sh'"
	exit 1
fi

MODFILES="
tools/clang/include/clang/Driver/CC1Options.td
tools/clang/lib/CodeGen/CodeGenModule.cpp
tools/clang/include/clang/Frontend/CodeGenOptions.h
tools/clang/lib/CodeGen/CodeGenTypes.cpp
tools/clang/lib/Frontend/CompilerInvocation.cpp
lib/IR/Module.cpp
include/llvm/IR/Module.h
lib/IR/TypeFinder.cpp
"

if [ "$CLEAN" == "" ]
then
	for f in $MODFILES
	do
		[ -e "$PATH_TO_LLVM_SOURCE/$f.bak" ] || mv "$PATH_TO_LLVM_SOURCE/$f" "$PATH_TO_LLVM_SOURCE/$f.bak"
		[ -L "$PATH_TO_LLVM_SOURCE/$f" ] || ln -s "$CURRENT_DIR/$(basename "$f")" "$PATH_TO_LLVM_SOURCE/$f"
	done

	TALESDRIVERDIR="$PATH_TO_LLVM_SOURCE/tools/clang/tools/TalesDriver"
	[ -e "$TALESDRIVERDIR" ] || mkdir "$TALESDRIVERDIR"
	[ -e "$TALESDRIVERDIR/CMakeLists.txt" ] || ln -s "$CURRENT_DIR/TalesDriver/CMakeLists.txt.in" "$TALESDRIVERDIR/CMakeLists.txt"
	[ -e "$TALESDRIVERDIR/driver.cpp" ] || ln -s "$CURRENT_DIR/TalesDriver/driver.cpp" "$TALESDRIVERDIR/"
	[ -e "$TALESDRIVERDIR/cc1_main.cpp" ] || ln -s ../driver/cc1_main.cpp "$TALESDRIVERDIR/"
	[ -e "$TALESDRIVERDIR/cc1as_main.cpp" ] || ln -s ../driver/cc1as_main.cpp "$TALESDRIVERDIR/"

	if ! grep -q "TalesDriver" "$PATH_TO_LLVM_SOURCE/tools/clang/tools/CMakeLists.txt"
	then
		echo 'add_subdirectory(TalesDriver)' >> "$PATH_TO_LLVM_SOURCE/tools/clang/tools/CMakeLists.txt"
	fi
else
	for f in $MODFILES
	do
		if [ -f "$PATH_TO_LLVM_SOURCE/$f.bak" ]
		then
			rm "$PATH_TO_LLVM_SOURCE/$f"
			mv "$PATH_TO_LLVM_SOURCE/$f.bak" "$PATH_TO_LLVM_SOURCE/$f"
		fi
	done

	rm -rf "$PATH_TO_LLVM_SOURCE/tools/clang/tools/TalesDriver"
	grep -v "TalesDriver" "$PATH_TO_LLVM_SOURCE/tools/clang/tools/CMakeLists.txt" > "$PATH_TO_LLVM_SOURCE/tools/clang/tools/CMakeLists.txt.bak"
	mv "$PATH_TO_LLVM_SOURCE/tools/clang/tools/CMakeLists.txt.bak" "$PATH_TO_LLVM_SOURCE/tools/clang/tools/CMakeLists.txt"

	exit 0
fi

cd "$PATH_TO_LLVM_BUILD"
cmake "$PATH_TO_LLVM_SOURCE" || exit $?
make || exit $?

ln -sf  "$PATH_TO_LLVM_BUILD/bin/TalesClang" "$OUTPUT_DIR/"
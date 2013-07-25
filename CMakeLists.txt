cmake_minimum_required(VERSION 2.8)

set(PATH_TO_LLVM_SOURCE "" CACHE PATH
	"Path to LLVM (with Clang) source code tree.")
set(PATH_TO_LLVM_BUILD "" CACHE PATH
	"Path to the directory where LLVM and Clang were built with CMake.")

# option(BUILD_CLANG_TOOLS "Build Clang-based tools (requires PATH_TO_LLVM_SOURCE and PATH_TO_LLVM_BUILD to be set)." OFF)
option(RESTORE_LLVM_SOURCE "Restore the original LLVM source tree once Tales is built." OFF)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# TODO: Building could be further simplified with an external_project configured with
# BuildType=Release in case the LLVM paths aren't set, but the user should choose
# this option willingly (with an option()).
# Further TODO: Maybe Tales' core could be built with a system copy, the only issue
# being TalesRuntime, which could come pre-built in the source code.

# Prefer the specified LLVM copy to the system copy (saves about 5GB if a system install isn't needed)
if (EXISTS "${PATH_TO_LLVM_SOURCE}/lib/IRReader" AND
		EXISTS "${PATH_TO_LLVM_BUILD}/lib/IRReader/CMakeFiles")
	message("Using the specified LLVM source tree instead of the system copy.")
	list(APPEND CMAKE_PREFIX_PATH "${PATH_TO_LLVM_BUILD}")
	list(APPEND CMAKE_MODULE_PATH "${PATH_TO_LLVM_BUILD}/share/llvm/cmake"
		"${PATH_TO_LLVM_SOURCE}/cmake/modules")

	find_package(LLVM REQUIRED)  # FIXME: sure it's selecting the source tree
		# instead of the system install?

	# NOTE: LLVM should provide a better way to reuse a source folder
	set(LLVM_INSTALL_PREFIX "${PATH_TO_LLVM_BUILD}")
	set(LLVM_INCLUDE_DIRS "${PATH_TO_LLVM_SOURCE}/include" "${PATH_TO_LLVM_BUILD}/include")
	set(LLVM_LIBRARY_DIRS "${LLVM_INSTALL_PREFIX}/lib")
else()
	message(FATAL_ERROR "A LLVM+Clang source code tree and a LLVM build directory are required to build Tales. The source tree will be patched, and LLVM built.\nLLVM doesn't need to be built before building Tales, but you should configure the source tree for example with:\n	cmake ../llvm -D CMAKE_BUILD_TYPE=Release\nA Release build is recommended since the size of files skyrockets in Debug and RelWithDebInfo modes.")
endif()

project(Tales)

add_subdirectory(utils/bisonc++)
add_subdirectory(src)


# We need a modified Clang in all cases for TalesRuntime.c
set(BUILD_CLANG_TOOLS ON)

if (BUILD_CLANG_TOOLS)
	if (NOT EXISTS "${PATH_TO_LLVM_SOURCE}/lib/IRReader")
		message(FATAL_ERROR "PATH_TO_LLVM_SOURCE needs to be a valid LLVM source tree.")
	endif()

	if (NOT EXISTS "${PATH_TO_LLVM_BUILD}/lib/IRReader/CMakeFiles")
		message(FATAL_ERROR "PATH_TO_LLVM_SOURCE needs to be a valid LLVM Cmake build directory (not an Autoconf build).")
	endif()

	add_subdirectory(tools/PseudoHeaderGen)
	add_subdirectory(tools/PH2Bitcode)
endif()

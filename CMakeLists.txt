cmake_minimum_required(VERSION 2.8)

set(PATH_TO_LLVM_SOURCE "" CACHE PATH "Path to LLVM (with Clang) source code tree.")
set(PATH_TO_LLVM_BUILD "" CACHE PATH "Path to the directory where LLVM and Clang were built with CMake.")

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# Prefer the specified LLVM copy to the system copy (can also saves from installing 5GB of files)
if (EXISTS "${PATH_TO_LLVM_SOURCE}/lib/IRReader" AND EXISTS "${PATH_TO_LLVM_BUILD}/lib/IRReader/CMakeFiles")
	message("Using the specified LLVM source tree instead of the system copy.")
	list(APPEND CMAKE_PREFIX_PATH "${PATH_TO_LLVM_BUILD}")
	list(APPEND CMAKE_MODULE_PATH "${PATH_TO_LLVM_BUILD}/share/llvm/cmake" "${PATH_TO_LLVM_SOURCE}/cmake/modules")

	find_package(LLVM REQUIRED)

	# NOTE: LLVM should provide a better way to reuse a source folder
	set(LLVM_INSTALL_PREFIX "${PATH_TO_LLVM_BUILD}")
	set(LLVM_INCLUDE_DIRS "${PATH_TO_LLVM_SOURCE}/include" "${PATH_TO_LLVM_BUILD}/include")
	set(LLVM_LIBRARY_DIRS "${LLVM_INSTALL_PREFIX}/lib")
else()
	list(APPEND CMAKE_MODULE_PATH "/usr/share/llvm/cmake" "/usr/local/share/llvm/cmake")

	find_package(LLVM REQUIRED)
endif()

project(Tales)

add_subdirectory(src)

# option(BUILD_CLANG_TOOLS "Build Clang-based tools (requires PATH_TO_LLVM_SOURCE and PATH_TO_LLVM_BUILD to be set)." OFF)
set(BUILD_CLANG_TOOLS ON) # We need a modified Clang in all cases for TalesRuntime.c

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
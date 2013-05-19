cmake_minimum_required(VERSION 2.8)

set(PATH_TO_LLVM_SOURCE "" CACHE PATH "Path to LLVM source code.")
set(PATH_TO_LLVM_BUILD "" CACHE PATH "Path to the directory where LLVM was built.")

project(Tales)

add_subdirectory(src)

option(BUILD_CLANG_TOOLS "Build Clang-based tools? Requires PATH_TO_LLVM_SOURCE and PATH_TO_LLVM_BUILD to be set." OFF)
if (BUILD_CLANG_TOOLS)
	add_subdirectory(tools/PseudoHeaderGen)
endif()

# install(TARGETS tales RUNTIME DESTINATION bin)
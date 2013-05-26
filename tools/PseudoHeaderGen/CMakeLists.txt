# NOTE: PseudoHeaderGen also needs Clang's built-in headers.
# That's why the tool needs to be linked rather than copied in order to work in the build folder, so that the path to the headers remains valid.
# You can always use -isystem /path/to/llvmbinarybuild/lib/clang/3.x/include

# TODO: check if PATH_TO_LLVM_SOURCE is empty/correct
add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/TalesPseudoHeaderGen"
	COMMAND env PATH_TO_LLVM_SOURCE="${PATH_TO_LLVM_SOURCE}" PATH_TO_LLVM_BUILD="${PATH_TO_LLVM_BUILD}" OUTPUT_DIR="${CMAKE_CURRENT_BINARY_DIR}" ${CMAKE_CURRENT_SOURCE_DIR}/build.sh)

add_custom_target(TalesPseudoHeaderGen DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/TalesPseudoHeaderGen")
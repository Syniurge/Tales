# NOTE: PseudoHeaderGen also needs Clang's built-in headers.
# That's why the tool needs to be linked rather than copied in order to work in the build folder, so that the path to the headers remains valid.
# You can always use -isystem /path/to/llvmbinarybuild/lib/clang/3.x/include

list(APPEND PHG_SOURCE_FILES
	DeclPrinter.hpp
	PseudoHeaderGen.cpp
	TypePrinter.hpp
	CMakeLists.txt.in
)

set(CLANG_TOOLS_EXTRA_DIR "${PATH_TO_LLVM_SOURCE}/tools/clang/tools/extra")

add_custom_command(OUTPUT "${CLANG_TOOLS_EXTRA_DIR}/TalesPseudoHeaderGen"
	COMMAND ${CMAKE_COMMAND} -E make_directory "${CLANG_TOOLS_EXTRA_DIR}/TalesPseudoHeaderGen")

foreach(PHG_SOURCE_FILE IN LISTS PHG_SOURCE_FILES)
	set(PHG_LLVM_FILE "${CLANG_TOOLS_EXTRA_DIR}/TalesPseudoHeaderGen/${PHG_SOURCE_FILE}")
	if (PHG_SOURCE_FILE STREQUAL "CMakeLists.txt.in")
		set(PHG_LLVM_FILE "${CLANG_TOOLS_EXTRA_DIR}/TalesPseudoHeaderGen/CMakeLists.txt")
	endif()

	if(UNIX)
		add_custom_command(OUTPUT ${PHG_LLVM_FILE}
			COMMAND ${CMAKE_COMMAND} -E create_symlink
				"${CMAKE_CURRENT_SOURCE_DIR}/${PHG_SOURCE_FILE}"
				"${PHG_LLVM_FILE}"
			DEPENDS "${CLANG_TOOLS_EXTRA_DIR}/TalesPseudoHeaderGen")
	else()
		add_custom_command(OUTPUT ${PHG_LLVM_FILE}
			COMMAND ${CMAKE_COMMAND} -E copy_if_different
				"${CMAKE_CURRENT_SOURCE_DIR}/${PHG_SOURCE_FILE}"
				"${PHG_LLVM_FILE}"
			DEPENDS ${PHG_SOURCE_FILE}
				"${CLANG_TOOLS_EXTRA_DIR}/TalesPseudoHeaderGen")
	endif()

	list(APPEND PHG_LLVM_FILES "${PHG_LLVM_FILE}")
endforeach(PHG_SOURCE_FILE)

add_custom_command(OUTPUT "${CLANG_TOOLS_EXTRA_DIR}/CMakeLists.txt.orig"
	COMMAND ${CMAKE_COMMAND} -E rename
		"${CLANG_TOOLS_EXTRA_DIR}/CMakeLists.txt"
		"${CLANG_TOOLS_EXTRA_DIR}/CMakeLists.txt.orig")

add_custom_command(OUTPUT "${CLANG_TOOLS_EXTRA_DIR}/CMakeLists.txt"
	COMMAND ${CMAKE_COMMAND} -E copy
		"${CLANG_TOOLS_EXTRA_DIR}/CMakeLists.txt.orig"
		"${CLANG_TOOLS_EXTRA_DIR}/CMakeLists.txt"
	COMMAND ${CMAKE_COMMAND} -E echo "add_subdirectory\\(TalesPseudoHeaderGen\\)"
		>> "${CLANG_TOOLS_EXTRA_DIR}/CMakeLists.txt"
	DEPENDS "${CLANG_TOOLS_EXTRA_DIR}/CMakeLists.txt.orig")


add_custom_command(OUTPUT "${PATH_TO_LLVM_BUILD}/TalesPHGRegen.touch"
	COMMAND ${CMAKE_COMMAND} "${PATH_TO_LLVM_SOURCE}"
	COMMAND ${CMAKE_COMMAND} -E touch "TalesPHGRegen.touch"
	WORKING_DIRECTORY "${PATH_TO_LLVM_BUILD}"
	DEPENDS ${PHG_LLVM_FILES}
		"${CLANG_TOOLS_EXTRA_DIR}/CMakeLists.txt"
		"${CLANG_TOOLS_EXTRA_DIR}/CMakeLists.txt.orig")

add_custom_command(OUTPUT "${PATH_TO_LLVM_BUILD}/bin/TalesPseudoHeaderGen"
	COMMAND ${CMAKE_BUILD_TOOL} TalesPseudoHeaderGen
	WORKING_DIRECTORY "${PATH_TO_LLVM_BUILD}"
	DEPENDS "${PATH_TO_LLVM_BUILD}/TalesPHGRegen.touch")

if(UNIX)
	add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/TalesPseudoHeaderGen"
		COMMAND ${CMAKE_COMMAND} -E create_symlink
			"${PATH_TO_LLVM_BUILD}/bin/TalesPseudoHeaderGen"
			"${CMAKE_CURRENT_BINARY_DIR}/TalesPseudoHeaderGen"
		DEPENDS "${PATH_TO_LLVM_BUILD}/bin/TalesPseudoHeaderGen")
else()
	add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/TalesPseudoHeaderGen"
		COMMAND ${CMAKE_COMMAND} -E copy
			"${PATH_TO_LLVM_BUILD}/bin/TalesPseudoHeaderGen"
			"${CMAKE_CURRENT_BINARY_DIR}/TalesPseudoHeaderGen"
		DEPENDS "${PATH_TO_LLVM_BUILD}/bin/TalesPseudoHeaderGen")
endif()

add_custom_target(TalesPseudoHeaderGen DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/TalesPseudoHeaderGen")

# Restore the original source tree once built
if (RESTORE_LLVM_SOURCE)
	foreach(PHG_LLVM_FILE IN LISTS PHG_LLVM_FILES)
		add_custom_command(TARGET TalesPseudoHeaderGen POST_BUILD
			COMMAND ${CMAKE_COMMAND} -E remove "${PHG_LLVM_FILE}")
	endforeach(PHG_LLVM_FILE)

	add_custom_command(TARGET TalesPseudoHeaderGen POST_BUILD
		COMMAND ${CMAKE_COMMAND} -E remove_directory
			"${CLANG_TOOLS_EXTRA_DIR}/TalesPseudoHeaderGen"
		COMMAND ${CMAKE_COMMAND} -E remove
			"${CLANG_TOOLS_EXTRA_DIR}/CMakeLists.txt"
		COMMAND ${CMAKE_COMMAND} -E rename
			"${CLANG_TOOLS_EXTRA_DIR}/CMakeLists.txt.orig"
			"${CLANG_TOOLS_EXTRA_DIR}/CMakeLists.txt"
	)
endif()
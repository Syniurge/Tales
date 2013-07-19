# The new bash script-free build system should work on any system supported by CMake (diff.exe and patch.exe are required on Windows though), but watch out for the NOTE at the end of the file.
# I won't be testing Tales on systems with CMAKE_BUILD_TOOL different from Make and Ninja for a long time, so some help would be needed to figure out the arguments to be passed to other build tools if there are any.

include(FindPackageHandleStandardArgs)

find_program(DIFF_EXECUTABLE diff)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(DIFF REQUIRED_VARS DIFF_EXECUTABLE)
find_program(PATCH_EXECUTABLE patch)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(PATCH REQUIRED_VARS PATCH_EXECUTABLE)

list(APPEND PH2BC_MODFILES
	tools/clang/include/clang/AST/ASTConsumer.h
	tools/clang/include/clang/Driver/CC1Options.td
	tools/clang/lib/CodeGen/CodeGenAction.cpp
	tools/clang/lib/CodeGen/CodeGenModule.cpp
	tools/clang/lib/CodeGen/CodeGenModule.h
	tools/clang/include/clang/Frontend/CodeGenOptions.h
	tools/clang/lib/CodeGen/CodeGenTypes.cpp
	tools/clang/lib/Frontend/CompilerInstance.cpp
	tools/clang/lib/Frontend/CompilerInvocation.cpp
	tools/clang/include/clang/AST/Decl.h
	tools/clang/include/clang/Basic/DiagnosticSemaKinds.td
	tools/clang/include/clang/Frontend/FrontendOptions.h
	lib/IR/Module.cpp
	include/llvm/IR/Module.h
	tools/clang/lib/CodeGen/ModuleBuilder.cpp
	tools/clang/lib/Parse/ParseAST.cpp
	tools/clang/lib/Parse/Parser.cpp
	tools/clang/include/clang/Sema/Sema.h
	tools/clang/lib/Sema/Sema.cpp
	tools/clang/lib/Sema/SemaDecl.cpp
	tools/clang/lib/Sema/SemaDeclCXX.cpp
	tools/clang/lib/Sema/SemaTemplate.cpp
	lib/IR/TypeFinder.cpp

	tools/clang/tools/driver/CMakeLists.txt
	tools/clang/tools/driver/driver.cpp
)

file(MAKE_DIRECTORY
	"${CMAKE_CURRENT_BINARY_DIR}/cmake"
	"${CMAKE_CURRENT_BINARY_DIR}/patches"
)

foreach(MODFILE_LLVM IN LISTS PH2BC_MODFILES)
	get_filename_component(MODFILE_BASENAME ${MODFILE_LLVM} NAME)
	set(MODFILE "${MODFILE_BASENAME}")
	set(MODFILE_ORIG "${MODFILE_BASENAME}.orig")

	# Special cases for files in TalesDriver/
	if (MODFILE_BASENAME STREQUAL "CMakeLists.txt")
		# CMakeLists.txt is renamed to CMakeLists.txt.in to avoid confusion and errors
		set(MODFILE "TalesDriver/${MODFILE_BASENAME}.in")
		set(MODFILE_ORIG "TalesDriver/${MODFILE_BASENAME}.orig")

		# The Makefiles only need to be regenerated when a CMakeLists.txt file is altered
		list(APPEND LLVM_CMAKE_GEN_DEPS
			"${PATH_TO_LLVM_SOURCE}/${MODFILE_LLVM}"
			"${PATH_TO_LLVM_SOURCE}/${MODFILE_LLVM}.orig"
		)
	elseif(MODFILE_BASENAME STREQUAL "driver.cpp")
		set(MODFILE "TalesDriver/${MODFILE_BASENAME}")
		set(MODFILE_ORIG "TalesDriver/${MODFILE_BASENAME}.orig")
	endif()

	configure_file(
		"${CMAKE_CURRENT_SOURCE_DIR}/patchgen.cmake.in"
		"${CMAKE_CURRENT_BINARY_DIR}/cmake/${MODFILE_BASENAME}.patchgen.cmake"
		@ONLY
	)

	add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/patches/${MODFILE_BASENAME}.patch"
		COMMAND ${CMAKE_COMMAND} -P "${CMAKE_CURRENT_BINARY_DIR}/cmake/${MODFILE_BASENAME}.patchgen.cmake"
		DEPENDS ${MODFILE}
	)

	# Make sure we are backing up the file to be patched only once
	add_custom_command(OUTPUT "${PATH_TO_LLVM_SOURCE}/${MODFILE_LLVM}.orig"
		COMMAND ${CMAKE_COMMAND} -E rename
			"${PATH_TO_LLVM_SOURCE}/${MODFILE_LLVM}"
			"${PATH_TO_LLVM_SOURCE}/${MODFILE_LLVM}.orig"
	)

	add_custom_command(OUTPUT "${PATH_TO_LLVM_SOURCE}/${MODFILE_LLVM}"
		COMMAND ${PATCH_EXECUTABLE} --no-backup-if-mismatch
			-o "${PATH_TO_LLVM_SOURCE}/${MODFILE_LLVM}"
			"${PATH_TO_LLVM_SOURCE}/${MODFILE_LLVM}.orig"
			"${CMAKE_CURRENT_BINARY_DIR}/patches/${MODFILE_BASENAME}.patch"
		DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/patches/${MODFILE_BASENAME}.patch"
			"${PATH_TO_LLVM_SOURCE}/${MODFILE_LLVM}.orig"
	)

	list(APPEND MODPATCHED "${PATH_TO_LLVM_SOURCE}/${MODFILE_LLVM}")
endforeach(MODFILE_LLVM)

if (LLVM_CMAKE_GEN_DEPS)
	add_custom_command(OUTPUT "${PATH_TO_LLVM_BUILD}/TalesClangRegen.touch"
		COMMAND ${CMAKE_COMMAND} "${PATH_TO_LLVM_SOURCE}"
		COMMAND ${CMAKE_COMMAND} -E touch "TalesClangRegen.touch"
		WORKING_DIRECTORY "${PATH_TO_LLVM_BUILD}"
		DEPENDS ${LLVM_CMAKE_GEN_DEPS})
else()
	add_custom_command(OUTPUT "${PATH_TO_LLVM_BUILD}/TalesClangRegen.touch"
		COMMAND ${CMAKE_COMMAND} -E touch "TalesClangRegen.touch"
		WORKING_DIRECTORY "${PATH_TO_LLVM_BUILD}")
endif()

# NOTE: I've skimmed through ExternalProject.cmake and I'm not sure how it makes
# Visual Studio build CMake-based projects
get_filename_component(BUILD_TOOL_WE "${CMAKE_BUILD_TOOL}" NAME_WE)
if (NOT ("${BUILD_TOOL_WE}" STREQUAL "make" OR "${BUILD_TOOL_WE}" STREQUAL "ninja"))
	message(WARNING "${BUILD_TOOL_WE} build command (for LLVM) may not be supported by Tales yet. If building fails you need to figure out the right arguments to pass in tools/PH2Bitcode/CMakeLists.txt, and then report back to me to include your changes for other users.")
endif()

add_custom_command(OUTPUT "${PATH_TO_LLVM_BUILD}/bin/TalesClang"
	COMMAND ${CMAKE_BUILD_TOOL} TalesClang
	WORKING_DIRECTORY "${PATH_TO_LLVM_BUILD}"
	DEPENDS ${MODPATCHED} "${PATH_TO_LLVM_BUILD}/TalesCMakeRegen.touch")

if(UNIX)
	add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/TalesClang"
		COMMAND ${CMAKE_COMMAND} -E create_symlink
			"${PATH_TO_LLVM_BUILD}/bin/TalesClang"
			"${CMAKE_CURRENT_BINARY_DIR}/TalesClang"
		DEPENDS "${PATH_TO_LLVM_BUILD}/bin/TalesClang")
else()
	add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/TalesClang"
		COMMAND ${CMAKE_COMMAND} -E copy
			"${PATH_TO_LLVM_BUILD}/bin/TalesClang"
			"${CMAKE_CURRENT_BINARY_DIR}/TalesClang"
		DEPENDS "${PATH_TO_LLVM_BUILD}/bin/TalesClang")
endif()

add_custom_target(TalesClang DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/TalesClang")

# Restore the original source tree once built
if (RESTORE_LLVM_SOURCE)
	foreach(MODFILE_LLVM IN LISTS PH2BC_MODFILES)
		add_custom_command(TARGET TalesClang POST_BUILD
			COMMAND ${CMAKE_COMMAND} -E remove
				"${PATH_TO_LLVM_SOURCE}/${MODFILE_LLVM}"
			COMMAND ${CMAKE_COMMAND} -E rename
				"${PATH_TO_LLVM_SOURCE}/${MODFILE_LLVM}.orig"
				"${PATH_TO_LLVM_SOURCE}/${MODFILE_LLVM}"
		)
	endforeach(MODFILE_LLVM)
endif()

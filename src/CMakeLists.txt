find_package(BISONCPP REQUIRED 4.03)
find_package(FLEXCPP REQUIRED 1.04)
 
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -fno-rtti -fno-exceptions")

 # WORKAROUND: the bisonc++ $insert polymorphic directive redefines the Tag__ enum
 # which we don't want to do since ObjectKind from the Tales RTTI is used, and there's no
 # way to tell bisonc++ not to do that without altering its code.
 # So for now we manually include TalesParserPolymorphic.
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/TalesParserBase.h.cmake
"file(READ TalesParserBase.h PARSER_BASECLASS_READ)
file(READ TalesParserPolymorphic PARSER_POLYMORPHIC_READ)
string(REPLACE \"///@includepolymorphic\" \"\${PARSER_POLYMORPHIC_READ}\"
	PARSER_BASECLASS_TOWRITE \"\${PARSER_BASECLASS_READ}\")
file(WRITE \"@CMAKE_CURRENT_BINARY_DIR@/TalesParserBase.h.skel\"
	\"\${PARSER_BASECLASS_TOWRITE}\")
")
add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/TalesParserBase.h.skel"
	COMMAND ${CMAKE_COMMAND} -P "${CMAKE_CURRENT_BINARY_DIR}/TalesParserBase.h.cmake"
	WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
	DEPENDS TalesParserBase.h TalesParserPolymorphic)

BISONCPP_TARGET(TalesParser Tales.y "${CMAKE_CURRENT_BINARY_DIR}/TalesParser.cc"
	ERROR_VERBOSE IMPLEMENTATION_HEADERS "TalesParser"
	BASECLASS_SKELETON "${CMAKE_CURRENT_BINARY_DIR}/TalesParserBase.h.skel"
	POLYMORPHIC_SKELETON "TalesParserPolymorphic"
	POLYMORPHIC_INLINE_SKELETON "TalesParserPolymorphic.inline"
	PARSEFUN_SKELETON "${CMAKE_CURRENT_SOURCE_DIR}/TalesParser.cc")
FLEXCPP_TARGET(TalesLexer Tales.l "${CMAKE_CURRENT_BINARY_DIR}/TalesLexer.cc"
	IMPLEMENTATION_HEADERS "TalesLexer"
	BASECLASS_SKELETON "TalesLexerBase.h"
	LEX_SKELETON "${CMAKE_CURRENT_SOURCE_DIR}/TalesLexer.cc")
ADD_FLEXCPP_BISONCPP_DEPENDENCY(TalesLexer TalesParser)
include_directories( ${CMAKE_CURRENT_BINARY_DIR} )

set(TALESCLANG_EXECUTABLE "${CMAKE_BINARY_DIR}/tools/PH2Bitcode/TalesClang")
add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/TalesRuntime.s"
	COMMAND ${TALESCLANG_EXECUTABLE}
	ARGS -S -Xclang -emit-llvm -std=c11 -O3
		-Xclang -input-constraint -Xclang "${CMAKE_CURRENT_SOURCE_DIR}"
		-o "${CMAKE_CURRENT_BINARY_DIR}/TalesRuntime.s" TalesRuntime.c
	DEPENDS TalesClang TalesRuntime.c TalesRuntime.h
	COMMENT "[TALESCLANG][TalesRuntime] Building LLVM IR"
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
 
include_directories( ${LLVM_INCLUDE_DIRS} )
link_directories( ${LLVM_LIBRARY_DIRS} )
add_definitions( ${LLVM_DEFINITIONS} )
 
llvm_map_components_to_libraries(REQ_LLVM_LIBRARIES jit native IRReader IPO)

# Those libraries might not be built yet, strip the non-LLVM libraries and build them
set(LLVM_LIBRARIES_TO_BUILD ${REQ_LLVM_LIBRARIES})
list(REMOVE_ITEM LLVM_LIBRARIES_TO_BUILD
	dl pthread z) # a bit HACK-ish
string(REPLACE ";" " "
	LLVM_LIBRARIES_TO_BUILD_PRETTY
	"${LLVM_LIBRARIES_TO_BUILD}")

add_custom_command(OUTPUT "${PATH_TO_LLVM_BUILD}/TalesRegen.touch"
	COMMAND ${CMAKE_COMMAND} "${PATH_TO_LLVM_SOURCE}"
	COMMAND ${CMAKE_BUILD_TOOL} ${LLVM_LIBRARIES_TO_BUILD}
	COMMAND ${CMAKE_COMMAND} -E touch "TalesRegen.touch"
	WORKING_DIRECTORY "${PATH_TO_LLVM_BUILD}"
	COMMENT "Generating ${LLVM_LIBRARIES_TO_BUILD_PRETTY}")

# NOTE: I couldn't find a proper way to have the library filename
# e.g "${LLVM_LIBRARY_DIRS}/lib${REQ_LLVM_LIB}.a" as OUTPUT
# CMake doesn't seem to provide any way to get the system-specific filename.

include_directories( ${CMAKE_CURRENT_SOURCE_DIR} )
add_library(Tales STATIC
	${BISONCPP_TalesParser_OUTPUTS}
	${FLEXCPP_TalesLexer_OUTPUTS}
	TalesAST.cpp
	TalesCodegen.cpp
) 
target_link_libraries(Tales)
set_source_files_properties(${BISONCPP_TalesParser_OUTPUTS} ${FLEXCPP_TalesLexer_OUTPUTS}
	PROPERTIES COMPILE_FLAGS "-fexceptions")
set_source_files_properties(TalesCodegen.cpp
	PROPERTIES OBJECT_DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/TalesRuntime.s")

find_package(BOBCAT REQUIRED)

include_directories(${LIBBOBCAT_INCLUDE_DIR})
add_executable(Scheherazade Scheherazade.cpp)
target_link_libraries(Scheherazade Tales ${LIBBOBCAT_LIBRARY} ${REQ_LLVM_LIBRARIES})
set_source_files_properties(Scheherazade.cpp
	PROPERTIES OBJECT_DEPENDS "${PATH_TO_LLVM_BUILD}/TalesRegen.touch")

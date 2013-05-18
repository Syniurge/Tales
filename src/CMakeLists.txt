list(APPEND CMAKE_MODULE_PATH "/usr/share/llvm/cmake" "/usr/local/share/llvm/cmake" "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

find_package(BISONCPP REQUIRED)
find_package(FLEXCPP REQUIRED)
find_package(LLVM REQUIRED)
find_package(CLANG REQUIRED)
 
add_definitions(-std=c++11)

BISONCPP_TARGET(TalesParser Tales.y "${CMAKE_CURRENT_BINARY_DIR}/TalesParser.cc" ERROR_VERBOSE IMPLEMENTATION_HEADERS "${CMAKE_CURRENT_SOURCE_DIR}/TalesParser")
FLEXCPP_TARGET(TalesLexer Tales.l "${CMAKE_CURRENT_BINARY_DIR}/TalesLexer.cc" IMPLEMENTATION_HEADERS "${CMAKE_CURRENT_SOURCE_DIR}/TalesLexer")
ADD_FLEXCPP_BISONCPP_DEPENDENCY(TalesLexer TalesParser)
include_directories( ${CMAKE_CURRENT_BINARY_DIR} )

CLANG_LLVMIR(TalesRuntime TalesRuntime.c "${CMAKE_CURRENT_BINARY_DIR}/TalesRuntime.s" DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/TalesRuntime.h" COMPILE_FLAGS -std=c11 -O3)
# set(TalesRuntime_OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/TalesRuntime.s.hpp")
# add_custom_command(OUTPUT ${TalesRuntime_OUTPUT}
# 	COMMAND "sh" "${CMAKE_CURRENT_SOURCE_DIR}/IR_to_hpp.sh" ${CLANG_LLVMIR_TalesRuntime_OUTPUT} ${TalesRuntime_OUTPUT}
# 	DEPENDS ${CLANG_LLVMIR_TalesRuntime_OUTPUT})
 
include_directories( ${LLVM_INCLUDE_DIRS} )
link_directories( ${LLVM_LIBRARY_DIRS} )
add_definitions( ${LLVM_DEFINITIONS} )
 
llvm_map_components_to_libraries(REQ_LLVM_LIBRARIES jit native IRReader IPO)

include_directories( ${CMAKE_CURRENT_SOURCE_DIR} )
add_library(Tales STATIC
	${BISONCPP_TalesParser_OUTPUTS}
	${FLEXCPP_TalesLexer_OUTPUTS}
	TalesCodegen.cpp
) 
target_link_libraries(Tales)
set_source_files_properties(TalesCodegen.cpp PROPERTIES OBJECT_DEPENDS ${CLANG_LLVMIR_TalesRuntime_OUTPUT})

find_package(BOBCAT REQUIRED)

include_directories(${LIBBOBCAT_INCLUDE_DIR})
add_executable(Scheherazade Scheherazade.cpp)
target_link_libraries(Scheherazade Tales ${LIBBOBCAT_LIBRARY} ${REQ_LLVM_LIBRARIES})
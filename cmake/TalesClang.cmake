#=============================================================================
# Copyright 2013 Elie Morisse
# Copyright 2009 Kitware, Inc.
# Copyright 2006 Tristan Carel
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# (To distribute this file outside of CMake, substitute the full
#  License text for the above reference.)

SET(TALESCLANG_EXECUTABLE "${CMAKE_BINARY_DIR}/tools/PH2Bitcode/TalesClang")

#============================================================
# TALESCLANG_LLVMIR (public macro)
#============================================================
#
MACRO(TALESCLANG_LLVMIR Name ClangInput ClangOutput)
  SET(TALESCLANG_LLVMIR_usage "TALESCLANG_LLVMIR(<Name> <Input> <Output> [DEPENDS <additional deps>] [COMPILE_FLAGS <string> ...]")

  SET(TALESCLANG_LLVMIR_parse_options "")
  SET(TALESCLANG_LLVMIR_parse_onevalue "")
  SET(TALESCLANG_LLVMIR_parse_multivalues DEPENDS COMPILE_FLAGS)
  CMAKE_PARSE_ARGUMENTS(TALESCLANG_LLVMIR "${TALESCLANG_LLVMIR_parse_options}" "${TALESCLANG_LLVMIR_parse_onevalue}" "${TALESCLANG_LLVMIR_parse_multivalues}" ${ARGN})

  IF("${TALESCLANG_LLVMIR_UNPARSED_ARGUMENTS}")
    MESSAGE(SEND_ERROR "Usage")
  ELSE()
    ADD_CUSTOM_COMMAND(OUTPUT ${ClangOutput}
      COMMAND ${TALESCLANG_EXECUTABLE}
      ARGS -S -emit-llvm ${TALESCLANG_LLVMIR_COMPILE_FLAGS} -o ${ClangOutput} ${ClangInput}
      DEPENDS TalesClang ${ClangInput} ${TALESCLANG_LLVMIR_DEPENDS}
      COMMENT "[TALESCLANG][${Name}] Building LLVM IR with clang ${TALESCLANG_VERSION}"
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

    # define target variables
    SET(TALESCLANG_LLVMIR_${Name}_DEFINED TRUE)
    SET(TALESCLANG_LLVMIR_${Name}_INPUT ${ClangInput})
    SET(TALESCLANG_LLVMIR_${Name}_OUTPUT ${ClangOutput})
    SET(TALESCLANG_LLVMIR_${Name}_COMPILE_FLAGS ${TALESCLANG_LLVMIR_COMPILE_FLAGS})
  ENDIF()
ENDMACRO(TALESCLANG_LLVMIR)
#
#============================================================
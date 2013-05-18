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

FIND_PROGRAM(CLANG_EXECUTABLE clang DOC "path to the clang executable")
MARK_AS_ADVANCED(CLANG_EXECUTABLE)

IF(CLANG_EXECUTABLE)
  # the clang commands should be executed with the C locale, otherwise
  # the message (which are parsed) may be translated
  SET(_Clang_SAVED_LC_ALL "$ENV{LC_ALL}")
  SET(ENV{LC_ALL} C)

  EXECUTE_PROCESS(COMMAND ${CLANG_EXECUTABLE} --version
    OUTPUT_VARIABLE CLANG_version_output
    ERROR_VARIABLE CLANG_version_error
    RESULT_VARIABLE CLANG_version_result
    OUTPUT_STRIP_TRAILING_WHITESPACE)

  SET(ENV{LC_ALL} ${_Clang_SAVED_LC_ALL})

  IF(NOT ${CLANG_version_result} EQUAL 0)
    MESSAGE(SEND_ERROR "Command \"${CLANG_EXECUTABLE} --version\" failed with output:\n${CLANG_version_error}")
  ELSE()
    # Clang
    IF("${CLANG_version_output}" MATCHES "^clang version")
      STRING(REGEX REPLACE "^clang version ([^ \\n]+) *.*\\n.*" "\\1"
        CLANG_VERSION "${CLANG_version_output}")
    ENDIF()
  ENDIF()

  # internal macro
  MACRO(CLANG_LLVMIR_option_extraopts Options)
    SET(CLANG_LLVMIR_extraopts "${Options}")
    SEPARATE_ARGUMENTS(CLANG_LLVMIR_extraopts)
    LIST(APPEND CLANG_LLVMIR_cmdopt ${CLANG_LLVMIR_extraopts})
  ENDMACRO(CLANG_LLVMIR_option_extraopts)

  #============================================================
  # CLANG_LLVMIR (public macro)
  #============================================================
  #
  MACRO(CLANG_LLVMIR Name ClangInput ClangOutput)
    SET(CLANG_LLVMIR_usage "CLANG_LLVMIR(<Name> <Input> <Output> [DEPENDS <additional deps>] [COMPILE_FLAGS <string> ...]")

    SET(CLANG_LLVMIR_parse_options "")
    SET(CLANG_LLVMIR_parse_onevalue "")
    SET(CLANG_LLVMIR_parse_multivalues DEPENDS COMPILE_FLAGS)
    CMAKE_PARSE_ARGUMENTS(CLANG_LLVMIR "${CLANG_LLVMIR_parse_options}" "${CLANG_LLVMIR_parse_onevalue}" "${CLANG_LLVMIR_parse_multivalues}" ${ARGN})

    IF("${CLANG_LLVMIR_UNPARSED_ARGUMENTS}")
      MESSAGE(SEND_ERROR "Usage")
    ELSE()
      ADD_CUSTOM_COMMAND(OUTPUT ${ClangOutput}
        COMMAND ${CLANG_EXECUTABLE}
        ARGS ${CLANG_LLVMIR_cmdopt} -S -emit-llvm ${CLANG_LLVMIR_COMPILE_FLAGS} -o ${ClangOutput} ${ClangInput}
        DEPENDS ${ClangInput} ${CLANG_LLVMIR_DEPENDS}
        COMMENT "[CLANG][${Name}] Building LLVM IR with clang ${CLANG_VERSION}"
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

      # define target variables
      SET(CLANG_LLVMIR_${Name}_DEFINED TRUE)
      SET(CLANG_LLVMIR_${Name}_INPUT ${ClangInput})
      SET(CLANG_LLVMIR_${Name}_OUTPUT ${ClangOutput})
      SET(CLANG_LLVMIR_${Name}_COMPILE_FLAGS ${CLANG_LLVMIR_COMPILE_FLAGS})
    ENDIF()
  ENDMACRO(CLANG_LLVMIR)
  #
  #============================================================

ENDIF(CLANG_EXECUTABLE)

INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(CLANG REQUIRED_VARS  CLANG_EXECUTABLE
                                        VERSION_VAR CLANG_VERSION)
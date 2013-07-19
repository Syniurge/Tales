# internal macro
MACRO(FLEXCPP_TARGET_option_extraopts Options)
	SET(FLEXCPP_TARGET_extraopts "${Options}")
	SEPARATE_ARGUMENTS(FLEXCPP_TARGET_extraopts)
	LIST(APPEND FLEXCPP_EXECUTABLE_opts ${FLEXCPP_TARGET_extraopts})
ENDMACRO(FLEXCPP_TARGET_option_extraopts)

#============================================================
# FLEXCPP_TARGET (public macro)
#============================================================
#
MACRO(FLEXCPP_TARGET Name Input Output)
	SET(FLEXCPP_TARGET_usage "FLEXCPP_TARGET(<Name> <Input> <Output> [DEBUG] [IMPLEMENTATION_HEADERS <extensionless file>] [COMPILE_FLAGS <string>]")

	STRING(REGEX REPLACE "\\.[a-zA-Z0-9_]+$" "base.h" FLEXCPP_TARGET_baseclassh "${Output}")
	LIST(APPEND FLEXCPP_TARGET_extraoutputs "${FLEXCPP_TARGET_baseclassh}")
	LIST(APPEND FLEXCPP_EXECUTABLE_opts -b "${FLEXCPP_TARGET_baseclassh}")

	SET(FLEXCPP_parse_options DEBUG)
	SET(FLEXCPP_parse_onevalue
		IMPLEMENTATION_HEADERS
		BASECLASS_SKELETON
		CLASS_SKELETON
		LEX_SKELETON
		)
	SET(FLEXCPP_parse_multivalues COMPILE_FLAGS)
	CMAKE_PARSE_ARGUMENTS(FLEXCPP "${FLEXCPP_parse_options}" "${FLEXCPP_parse_onevalue}" "${FLEXCPP_parse_multivalues}" ${ARGN})

	IF(FLEXCPP_UNPARSED_ARGUMENTS)
		MESSAGE(SEND_ERROR ${FLEXCPP_TARGET_usage})
	ELSE()
		IF(FLEXCPP_DEBUG)
			LIST(APPEND FLEXCPP_EXECUTABLE_opts --debug)
		ENDIF()

		IF(FLEXCPP_IMPLEMENTATION_HEADERS)
			LIST(APPEND FLEXCPP_EXECUTABLE_opts -c "${FLEXCPP_IMPLEMENTATION_HEADERS}.h" -i "${FLEXCPP_IMPLEMENTATION_HEADERS}.ih")
			LIST(APPEND FLEXCPP_TARGET_extradeps ${BISONCPP_IMPLEMENTATION_HEADERS}.h ${BISONCPP_IMPLEMENTATION_HEADERS}.ih)
		ENDIF()

		IF(FLEXCPP_BASECLASS_SKELETON)
			LIST(APPEND FLEXCPP_EXECUTABLE_opts --baseclass-skeleton="${FLEXCPP_BASECLASS_SKELETON}")
			LIST(APPEND FLEXCPP_TARGET_extradeps ${FLEXCPP_BASECLASS_SKELETON})
		ENDIF()

		IF(FLEXCPP_CLASS_SKELETON)
			LIST(APPEND FLEXCPP_EXECUTABLE_opts --class-skeleton="${FLEXCPP_CLASS_SKELETON}")
			LIST(APPEND FLEXCPP_TARGET_extradeps ${FLEXCPP_CLASS_SKELETON})
		ENDIF()

		IF(FLEXCPP_LEX_SKELETON)
			LIST(APPEND FLEXCPP_EXECUTABLE_opts --lex-skeleton="${FLEXCPP_LEX_SKELETON}")
			LIST(APPEND FLEXCPP_TARGET_extradeps ${FLEXCPP_LEX_SKELETON})
		ENDIF()

		IF(FLEXCPP_COMPILE_FLAGS)
			LIST(APPEND FLEXCPP_EXECUTABLE_opts ${FLEXCPP_COMPILE_FLAGS})
		ENDIF()

		ADD_CUSTOM_COMMAND(OUTPUT ${Output} ${FLEXCPP_TARGET_extraoutputs}
			COMMAND ${FLEXCPP_EXECUTABLE}
			ARGS ${FLEXCPP_EXECUTABLE_opts} -l ${Output} ${Input}
			DEPENDS ${Input} ${FLEXCPP_TARGET_extradeps}
			COMMENT "[FLEXCPP][${Name}] Building scanner with flexc++ ${FLEXCPP_VERSION}"
			WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

		SET(FLEXCPP_${Name}_DEFINED TRUE)
		SET(FLEXCPP_${Name}_OUTPUTS ${Output})
		SET(FLEXCPP_${Name}_INPUT ${Input})
		SET(FLEXCPP_${Name}_COMPILE_FLAGS ${FLEXCPP_EXECUTABLE_opts})
	ENDIF()
ENDMACRO(FLEXCPP_TARGET)
#============================================================


#============================================================
# ADD_FLEXCPP_BISONCPP_DEPENDENCY (public macro)
#============================================================
#
MACRO(ADD_FLEXCPP_BISONCPP_DEPENDENCY FlexTarget BisonTarget)
	IF(NOT FLEXCPP_${FlexTarget}_OUTPUTS)
		MESSAGE(SEND_ERROR "Flexc++ target `${FlexTarget}' does not exists.")
	ENDIF()

	IF(NOT BISONCPP_${BisonTarget}_OUTPUTS)
		MESSAGE(SEND_ERROR "Bisonc++ target `${BisonTarget}' does not exists.")
	ENDIF()

	SET_SOURCE_FILES_PROPERTIES(${FLEXCPP_${FlexTarget}_OUTPUTS}
		PROPERTIES OBJECT_DEPENDS ${BISONCPP_${BisonTarget}_OUTPUTS})
ENDMACRO(ADD_FLEXCPP_BISONCPP_DEPENDENCY)
#============================================================

#
# OpenCogGenTypes.cmake
#
# Several macros that parse the `atom_types.script` file to extract
# the atom types, and put them into a form that the various different
# language generators can use.
#
# Both of these are single-line based. That is, they parse one line
# of type definitions, and set up various flags and values that are
# more convenient for the language bindings generators.
#
# ----------------------------------------------------------------------
#
MACRO(OPENCOG_TYPEINFO_REGEX)
	# This regular expression is more complex than required
	# due to cmake's regex engine bugs
	STRING(REGEX MATCH "^[ 	]*([A-Z0-9_]+)?([ 	]*<-[ 	]*([A-Z0-9_, 	]+))?[ 	]*(\"[A-Za-z]*\")?[ 	]*(//.*)?[ 	]*$" MATCHED "${LINE}")
ENDMACRO(OPENCOG_TYPEINFO_REGEX)

MACRO(OPENCOG_TYPEINFO_SETUP)
	SET(TYPE ${CMAKE_MATCH_1})
	SET(PARENT_TYPES ${CMAKE_MATCH_3})
	SET(TYPE_NAME "")
	IF (CMAKE_MATCH_4)
		# MESSAGE(STATUS "Custom atom type name specified: ${CMAKE_MATCH_4}")
		MESSAGE(DEBUG "Custom atom type name specified: ${CMAKE_MATCH_4}")

		# Convert ... uhh ..something...
		STRING(REGEX MATCHALL "." CHARS ${CMAKE_MATCH_4})
		LIST(LENGTH CHARS LIST_LENGTH)
		MATH(EXPR LAST_INDEX "${LIST_LENGTH} - 1")
		FOREACH(I RANGE ${LAST_INDEX})
			LIST(GET CHARS ${I} C)
			IF (NOT ${C} STREQUAL "\"")
				SET(TYPE_NAME "${TYPE_NAME}${C}")
			ENDIF (NOT ${C} STREQUAL "\"")
		ENDFOREACH(I RANGE ${LIST_LENGTH})
	ENDIF (CMAKE_MATCH_4)

	# Convert upper-case snake-case names like FOO_BAR_LINK
	# to CamelCase names like FooBarLink
	IF (TYPE_NAME STREQUAL "")
		# Set type name using camel casing
		STRING(REGEX MATCHALL "." CHARS ${TYPE})
		LIST(LENGTH CHARS LIST_LENGTH)
		MATH(EXPR LAST_INDEX "${LIST_LENGTH} - 1")
		FOREACH(I RANGE ${LAST_INDEX})
			LIST(GET CHARS ${I} C)
			IF (NOT ${C} STREQUAL "_")
				MATH(EXPR IP "${I} - 1")
				LIST(GET CHARS ${IP} CP)
				IF (${I} EQUAL 0)
					SET(TYPE_NAME "${TYPE_NAME}${C}")
				ELSE (${I} EQUAL 0)
					IF (${CP} STREQUAL "_")
						SET(TYPE_NAME "${TYPE_NAME}${C}")
					ELSE (${CP} STREQUAL "_")
						STRING(TOLOWER "${C}" CL)
						SET(TYPE_NAME "${TYPE_NAME}${CL}")
					ENDIF (${CP} STREQUAL "_")
				ENDIF (${I} EQUAL 0)
			ENDIF (NOT ${C} STREQUAL "_")
		ENDFOREACH(I RANGE ${LIST_LENGTH})
	ENDIF (TYPE_NAME STREQUAL "")

	# Create short CamelCase names
	STRING(REGEX REPLACE "([a-zA-Z]*)(Link|Node)$" "\\1" SHORT_NAME ${TYPE_NAME})
	# MESSAGE(STATUS "Atom type name: ${TYPE_NAME} ${SHORT_NAME}")
	MESSAGE(DEBUG "Atom type name: ${TYPE_NAME} ${SHORT_NAME}")

	# Convert upper-case snake-case to lower case.
	STRING(TOLOWER ${TYPE} LC_SNAKE_TYPE)
	STRING(REGEX REPLACE "([a-z_]*)(_link|_node)$" "\\1"
		LC_SNAKE_SHORT ${LC_SNAKE_TYPE})

	# -----------------------------------------------------------
	# Try to guess if the thing is a node or link based on its name
	STRING(REGEX MATCH "VALUE$" ISVALUE ${TYPE})
	STRING(REGEX MATCH "STREAM$" ISSTREAM ${TYPE})
	STRING(REGEX MATCH "ATOM_SPACE$" ISATOMSPACE ${TYPE})
	STRING(REGEX MATCH "NODE$" ISNODE ${TYPE})
	STRING(REGEX MATCH "LINK$" ISLINK ${TYPE})
	STRING(REGEX MATCH "AST$" ISAST ${TYPE})

	# If not explicitly named, assume its a link. This is kind of
	# hacky, but is needed for e.g. "VariableList" ...
	IF (NOT ISNODE STREQUAL "NODE"
		AND NOT ISVALUE STREQUAL "VALUE"
		AND NOT ISSTREAM STREQUAL "STREAM"
		AND NOT ISATOMSPACE STREQUAL "ATOM_SPACE"
		AND NOT ISAST STREQUAL "AST")
		SET(ISLINK "LINK")
	ENDIF (NOT ISNODE STREQUAL "NODE"
		AND NOT ISVALUE STREQUAL "VALUE"
		AND NOT ISSTREAM STREQUAL "STREAM"
		AND NOT ISATOMSPACE STREQUAL "ATOM_SPACE"
		AND NOT ISAST STREQUAL "AST")

	IF (${TYPE} STREQUAL "VALUATION")
		SET(ISLINK "")
	ENDIF (${TYPE} STREQUAL "VALUATION")
ENDMACRO(OPENCOG_TYPEINFO_SETUP)

#####################################################################

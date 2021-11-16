#
# Convert one ml file to an mli file.
# Additional arguments are dependencies

MACRO(OCAML_MAKE_INTERFACE name)
	SET(DEPENDS ${name})

	# Allow the user to specify dependencies as optional arguments
	SET(DEPENDS ${DEPENDS} ${ARGN})

	ADD_CUSTOM_COMMAND(
		OUTPUT ${name}i
		COMMAND ${CMAKE_OCaml_FIND} ocamlc -i ${name} >> ${name}i
		DEPENDS ${DEPENDS}
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
		COMMENT "Building the ${name}i file"
	)

	list(APPEND ADDITIONAL_MAKE_CLEAN_FILES "${name}i")
ENDMACRO()

#
# commented out because ti doesn't seem to do anything.
#
#MACRO(OCAML_MAKE_WRAP_INTF phony mlfile)
#
#	ADD_CUSTOM_TARGET(
#		${phony}
#		OUTPUT ${mlfile}i
#		COMMAND ${CMAKE_OCaml_FIND} ocamlc -i ${mlfile} >> ${mlfile}i
#		DEPENDS ${mlfile}
#		# WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
#		COMMENT "Building the ${mlfile}i file"
#	)
#
#	list(APPEND ADDITIONAL_MAKE_CLEAN_FILES "${mlfile}i")
#ENDMACRO()

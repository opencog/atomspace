
# Use UseOCaml.cmake, provided by cogutils
INCLUDE(UseOCaml)

OPENCOG_GEN_OCAML_ATOMTYPES(../atoms/atom_types/atom_types.script
	atoms.ml mlwrappers.cc)

ADD_LIBRARY (camlatoms
	CamlWrap.cc
	mlwrappers.cc
	# ${CMAKE_BINARY_DIR}/opencog/atoms/atom_types/mlwrappers
)

SET_TARGET_PROPERTIES(camlatoms PROPERTIES
	PREFIX "dll"
	INSTALL_PATH "lib${LIB_DIR_SUFFIX}/opencog/ocaml/"
)

TARGET_LINK_LIBRARIES(camlatoms
	atomspace
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(camlatoms opencog_atom_types)

# Add phony targets, so that CMake doesn't complain about missing
# files (which don't exist at cmake time). This sets the GENERATED
# property onn these files.
ADD_CUSTOM_TARGET(ocaml_wrapper_cc
	DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/mlwrapper.cc)

ADD_CUSTOM_TARGET(ocaml_wrapper_ml
	DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/atoms.ml)


# This does nothing!?? XXX and currently seems un-neeed...
# OCAML_MAKE_INTERFACE(atomse.ml)

# XXX FIXME, this doesn't seem to do anything...
# OCAML_MAKE_WRAP_INTF(ocaml_wrapper_mli
# 	${CMAKE_CURRENT_BINARY_DIR}/atoms.ml)

# Attention: the atomese.ml and atomese.mli files can be named anything.
# For example, atomfoo.mli. In this case, the user would say:
# #load "atomese.cma" ;;
# open Atomfoo ;;
# and it all works. Be sure to install atomfoo.cmi, below.
ADD_OCAML_LIBRARY(atomese
	SOURCES
		${CMAKE_CURRENT_BINARY_DIR}/atoms.mli
		${CMAKE_CURRENT_BINARY_DIR}/atoms.ml
		# atoms.ml
		# atomese.mli
		atomese.ml
	C_LIBRARIES
		camlatoms
)

INSTALL_OCAML_TARGETS(
	atomese
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog/ocaml")

# Attention: the second (and subsequent) argument is the name of
# the mli (cmi) file, for example atomfoo, as noted above.
INSTALL_OCAML_INTERFACES(
	atomese
	atomese
	atoms
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog/ocaml")

INSTALL (TARGETS camlatoms
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog/ocaml")

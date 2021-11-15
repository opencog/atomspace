
# Use UseOCaml.cmake, provided by cogutils
INCLUDE(UseOCaml)

INCLUDE_DIRECTORIES(
	${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_CURRENT_BINARY_DIR}
	${CMAKE_BINARY_DIR}/opencog/atoms/atom_types
)

ADD_LIBRARY (camlatoms
	CamlWrap.cc
	# mlwrappers.cc
	${CMAKE_BINARY_DIR}/opencog/atoms/atom_types/mlwrappers
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
ADD_CUSTOM_TARGET(ocaml_wrapper_cc
	DEPENDS ${CMAKE_BINARY_DIR}/opencog/atoms/atom_types/mlwrapper.cc)

# This does nothing!?? XXX
# OCAML_MAKE_INTERFACE(atomse.ml)

# The core_types.mli file must be generated before the module can be
# compiled. Thus we add a phony target.
#ADD_CUSTOM_TARGET(ocaml_wrapper_ml
#	DEPENDS ${CMAKE_BINARY_DIR}/opencog/atoms/atom_types/core_types.ml)

# XXX FIXME, this doesn't seem to do anything...
# OCAML_MAKE_WRAP_INTF(ocaml_wrapper_mli
# 	${CMAKE_BINARY_DIR}/opencog/atoms/atom_types/core_types.ml)

# Attention: the atomese.ml and atomese.mli files can be named anything.
# For example, atomfoo.mli. In this case, the user would say:
# #load "atomese.cma" ;;
# open Atomfoo ;;
# and it all works. Be sure to install atomfoo.cmi, below.
ADD_OCAML_LIBRARY(atomese
	SOURCES
		# ${CMAKE_BINARY_DIR}/opencog/atoms/atom_types/core_types.mli
		# ${CMAKE_BINARY_DIR}/opencog/atoms/atom_types/core_types.ml
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
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog/ocaml")

INSTALL (TARGETS camlatoms
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog/ocaml")

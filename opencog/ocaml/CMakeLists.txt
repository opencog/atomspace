
# Use UseOCaml.cmake, provided by cogutils
INCLUDE(UseOCaml)

ADD_LIBRARY (camlatoms
	CamlWrap.cc
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

OCAML_MAKE_INTERFACE(atomse.ml)

# Attention: the atomese.ml and atomese.mli files can be named anything.
# For example, atomfoo.mli. In this case, the user would say:
# #load "atomese.cma" ;;
# open Atomfoo ;;
# and it all works. Be sure to install atomfoo.cmi, below.
ADD_OCAML_LIBRARY(atomese
	SOURCES
		${CMAKE_BINARY_DIR}/opencog/atoms/atom_types/core_types.mli
		${CMAKE_BINARY_DIR}/opencog/atoms/atom_types/core_types.ml
		atomese.mli
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

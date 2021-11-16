
# Use UseOCaml.cmake, provided by cogutils
INCLUDE(UseOCaml)

OPENCOG_GEN_OCAML_ATOMTYPES(../atoms/atom_types/atom_types.script
	atoms.ml mlwrappers.cc)

ADD_LIBRARY (camlatoms
	CamlWrap.cc
	mlwrappers.cc
)

SET_TARGET_PROPERTIES(camlatoms PROPERTIES
	PREFIX "dll"
	INSTALL_PATH "lib${LIB_DIR_SUFFIX}/opencog/ocaml/"
)

TARGET_LINK_LIBRARIES(camlatoms
	atomspace
)

# This does nothing!?? and currently seems un-neeed...
# OCAML_MAKE_INTERFACE(atomese.ml)

# For each foo.mli listed below, the user needs to say:
# open Foo ;;
# (after saying #load "atomese.cma" ;; of course)
# Be sure to install foo.cmi, below.
ADD_OCAML_LIBRARY(atomese
	SOURCES
		# atoms.ml
		${CMAKE_CURRENT_BINARY_DIR}/atoms.ml
		atomese.ml
	C_LIBRARIES
		camlatoms
)

INSTALL_OCAML_TARGETS(
	atomese
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog/ocaml")

# Attention: the second (and subsequent) argument is the name of
# the mli (cmi) file, for example foo.mli, as noted above.
INSTALL_OCAML_INTERFACES(
	atomese
	atomese
	atoms
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog/ocaml")

INSTALL (TARGETS camlatoms
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog/ocaml")

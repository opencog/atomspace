
# Use UseOCaml.cmake, provided by cogutils
INCLUDE(UseOCaml)

# Generate wrappers from atoms from the script file.
OPENCOG_GEN_OCAML_ATOMTYPES(../atoms/atom_types/atom_types.script
	atoms.ml mlwrappers.cc opencog/atoms/atom_types/atom_types.h)

# Handle storage here, too. What the heck.
OPENCOG_GEN_OCAML_ATOMTYPES(../persist/storage/storage_types.script
	storage.ml storewrappers.cc opencog/persist/storage/storage_types.h)

ADD_LIBRARY (camlatoms
	CamlWrap.cc
	mlwrappers.cc
	storewrappers.cc
)

SET_TARGET_PROPERTIES(camlatoms PROPERTIES
	PREFIX "dll"
	INSTALL_PATH "lib${LIB_DIR_SUFFIX}/opencog/ocaml/"
)

TARGET_LINK_LIBRARIES(camlatoms
	atomspace
)

# For each foo.mli listed below, the user needs to say:
# open Foo ;;
# (after saying #load "atomese.cma" ;; of course)
# Be sure to install foo.cmi, below.
ADD_OCAML_LIBRARY(atomese
	SOURCES
		# atoms.ml
		${CMAKE_CURRENT_BINARY_DIR}/atoms.ml
		${CMAKE_CURRENT_BINARY_DIR}/storage.ml
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
	storage
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog/ocaml")

INSTALL (TARGETS camlatoms
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog/ocaml")

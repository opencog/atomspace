INSTALL(FILES
	OpenCogAtomTypes.cmake
	OpenCogCython.cmake
	OpenCogGenCxxTypes.cmake
	OpenCogGenOCamlTypes.cmake
	OpenCogGenPythonTypes.cmake
	OpenCogGenScmTypes.cmake
	OpenCogGenTypes.cmake
	OpenCogGenWrapper.cmake
	OpenCogGuile.cmake
	OpenCogOCaml.cmake
	OpenCogMacros.cmake
	OpenCogFunctions.cmake
	DESTINATION ${DATADIR}/cmake/)

# PROGRAMS marks the file ugo+x
install(PROGRAMS
	cython-wrapper.sh
	DESTINATION ${DATADIR}/cmake/)

LINK_LIBRARIES(atomspace load_scm)

ADD_CXXTEST(FastLoadUTest)
ADD_CXXTEST(CommandsUTest)
ADD_CXXTEST(DispatchUTest)

ADD_GUILE_TEST(FileStorageTest file-storage.scm)
ADD_GUILE_TEST(FileEpisodicTest file-episodic.scm)

LINK_LIBRARIES(atomspace load_scm)

ADD_CXXTEST(FastLoadUTest)
ADD_CXXTEST(CommandsUTest)

ADD_GUILE_TEST(FileStorageUTest file-storage.scm)

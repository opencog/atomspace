
ADD_GUILE_TEST(ProxyNodeTest proxy-node-test.scm)

# This tests uses the postgres backend and fails on
# (use-modules (opencog persist-sql))
# if Postgres wasn't built.
IF (PGSQL_FOUND)
	ADD_GUILE_TEST(ReadProxyTest read-proxy-test.scm)
ENDIF (PGSQL_FOUND)


// All of the unit tests print this exact same message.
void friendlyFailMessage()
{
	TS_FAIL("This unit test failed.\n"
		"This is probably because you do not have SQL installed\n"
		"or configured the way that AtomSpace expects.\n\n"
		"SQL persistence is optional for AtomSpace, so if you\n"
		"don't want it or need it, just ignore this test failure.\n"
		"Otherwise, please be sure to read opencong/persist/sql/README,\n"
		"and create/configure the SQL database as described there.\n"
		"Next, edit tests/persist/sql/atomspace-test.conf appropriately,\n"
		"so as to indicate the location of your database. If this is\n"
		"done correctly, then this test will pass.\n");
	exit(1);
}

void getDBconfig()
{
	try
	{
		config().load("atomspace-test.conf");
	}
	catch (RuntimeException &e)
	{
		std::cerr << e.get_message() << std::endl;
	}

	try {
		// Get the database logins & etc from the config file.
		dbname = config().get("TEST_DB_NAME", "opencog_test").c_str();
		username = config().get("TEST_DB_USERNAME", "opencog_tester").c_str();
		passwd = config().get("TEST_DB_PASSWD", "cheese").c_str();
	}
	catch (InvalidParamException &e)
	{
		friendlyFailMessage();
	}
}

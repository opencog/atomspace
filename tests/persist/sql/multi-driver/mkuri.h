std::string mkuri(std::string driver, std::string dbname,
                  std::string username, std::string passwd)
{
	std::string uri;

	// Sigh. The buildbot postgres config balks at the
	// "localhost" URI scheme.
	if (driver == "postgres")
	{
		uri = driver;
		uri += ":///";
		uri += dbname;
		uri += "?user=";
		uri += username;
		uri += "&password=";
		uri += passwd;
	}
	else
	{
		uri = driver;
		uri += "://";
		uri += username;
		uri += ":";
		uri += passwd;
		uri += "/";
		uri += dbname;
	}
	return uri;
}

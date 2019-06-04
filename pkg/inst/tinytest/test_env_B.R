
# The "hihihaha" environment variable was set in test_env_A and should be unset
# now.
expect_equal(Sys.getenv("hihihaha"), "")

# The "hihihaha" option was set in test_env_A and should be unset now
expect_true(is.null(getOption("hihihaha")))

expect_equal(Sys.getenv("hoho"), "")
expect_true( is.null(getOption("hoho")) )

# Surviving envvars and options
expect_equal(Sys.getenv("hehe"), "3")
Sys.unsetenv("hehe")
expect_equal(getOption("hehe"), 3)
options(hehe=NULL)


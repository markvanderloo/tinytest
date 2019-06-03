
# The "hihihaha" environment variable was set in test_env_A and should be unset
# now.
expect_equal(Sys.getenv("hihihaha"), "")

# The "hihihaha" option was set in test_env_B and should be unset now
expect_true(is.null(getOption("hihihaha")))



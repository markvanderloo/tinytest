
# We set an environment variable in this file. It must be unset by tinytest
# before running the next file (test_env_B.R)
Sys.setenv(hihihaha=8)

expect_equal(Sys.getenv("hihihaha"), "8")

# We set an option in this file. It must be unset by tinytest before running
# the next file (test_env_B.R)
options(hihihaha=8)
expect_equal(getOption("hihihaha"), 8)



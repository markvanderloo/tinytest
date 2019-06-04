
# We set an environment variable in this file. It must be unset by tinytest
# before running the next file (test_env_B.R)
Sys.setenv(hihihaha=8)

expect_equal(Sys.getenv("hihihaha"), "8")

# We set an option in this file. It must be unset by tinytest before running
# the next file (test_env_B.R)
options(hihihaha=8)
expect_equal(getOption("hihihaha"), 8)

# We set another envvar and unset it as well. We don't want to bother
# users already following good practice 
k <- Sys.getenv("hoho")
Sys.setenv(hoho=2)
Sys.setenv(hoho=k)

# We set another option and unset it as well. We don't want to bother
# users already following good practice
oldopt <- options("hoho")
options(hoho=2)
options(hoho=oldopt)

# Here's an envvar that must survive the slaughter after this file was run
base::Sys.setenv(hehe=3)

# And one for options as well
base::options(hehe=3)


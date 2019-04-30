## note: the system.file call is just a convenience for my local test.r script

# run all tests in test_tiny again, this time 
# with the file runner.
results <- run_test_file(system.file("tinytest/test_tiny.R",package="tinytest"),verbose=FALSE)
bools <- sapply(results, as.logical)
expect_true(all(bools))

# more complicated tests, using ignore() to skip nested expectations
results <- run_test_file(system.file("tinytest/programming.R",package="tinytest"), verbose=FALSE)
expect_equal(11, length(results))


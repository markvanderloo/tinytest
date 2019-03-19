

# run all tests in test_tiny again, this time 
# with the file runner.
results <- run_test_file("test_tiny.R")
bools <- sapply(results, as.logical)
expect_true(all(bools))

# more complicated tests
results <- run_test_file("programming.R")
expect_equal(11, length(results))


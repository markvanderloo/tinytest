## note: the system.file call is just a convenience for my local test.r script

# run all tests in test_tiny again, this time 
# with the file runner.
results <- run_test_file(system.file("tinytest/test_tiny.R",package="tinytest"),verbose=FALSE)
bools <- sapply(results, as.logical)
expect_true(all(bools))

# more complicated tests, using ignore() to skip nested expectations
results <- run_test_file(system.file("tinytest/programming.R",package="tinytest"), verbose=FALSE)
expect_equal(11, length(results))

expect_true(all_pass(results))
expect_false(any_fail(results))
expect_false(all_fail(results))
expect_true(any_pass(results))

expect_error(all_pass("hihi"))
expect_error(any_pass("hihi"))
expect_error(all_fail("hihi"))
expect_error(any_fail("hihi"))



# collect side effects using the call in the test file
out <- run_test_file("runs/test_envvar.R",verbose=0)
expect_true(is.na(out[[2]]))
expect_equal(sum(is.na(sapply(out, c))),1)

# only perform this test when LC_COLLATE is not "C", because
# setting this is what we try to detect.
if (Sys.getlocale("LC_COLLATE") != "C"){
  out <- run_test_file("runs/test_locale.R", verbose=0)
  expect_true(is.na(out[[2]]))
}

out <- run_test_file("runs/test_cwd.R", verbose=0)
expect_true(is.na(out[[1]]))
expect_true(is.na(out[[2]]))

# controll collecting side-effects from file runner.
out <- run_test_file("runs/test_envvar2.R", verbose=0, side_effects=TRUE)
expect_true(is.na(out[[2]]))
expect_equal(sum(is.na(sapply(out, c))),1)
# detailed control
out <- run_test_file("runs/test_cwd2.R", side_effects=list(pwd=FALSE), verbose=0)
expect_equal(length(out),0)

# premature exit testing
out <- run_test_file("runs/test_exit.R", verbose=0)
expect_equal(length(out), 2)

out <- run_test_file("runs/test_exit_if_not.R", verbose=0)
expect_equal(length(out), 2)



# plots should not cause an 'Rplots.pdf' file being created
plot(1:10, 1:10)
expect_false(exists("Rplots.pdf"))


# test that files are run with environment variables set
out <- run_test_file("runs/test_set_env.R", set_env=list("wa_babalooba"="ba_la_bamboo"), verbose=0)
expect_true(all_pass(out))

expect_message(run_test_file("runs/test_double_colon.R", verbose=0))
expect_message(tinytest:::check_double_colon("runs/test_double_colon.R"))

# uncomment to see the warning with 'make test'
# this also tests that commented lines are not counted.
# tinytest:::expect_equal(2,5)



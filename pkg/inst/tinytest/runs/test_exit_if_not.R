expect_true(TRUE)
expect_true(TRUE)

# test against an illegal pkg name that could never exist on CRAN :-).
exit_if_not(requireNamespace("123__slartibartfast__321", quietly=TRUE))

expect_true(TRUE)
expect_true(TRUE)


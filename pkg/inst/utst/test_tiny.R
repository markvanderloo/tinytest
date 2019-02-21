


expect_true(TRUE)
expect_false(FALSE)

expect_false(expect_equal(1+1, 3))
expect_true(expect_equal(1+1, 2))

expect_false(expect_equal(1+1, 3))

dat <- read.csv("women.csv")
expect_equal(women, dat)



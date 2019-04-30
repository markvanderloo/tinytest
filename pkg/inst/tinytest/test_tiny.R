

# check behavior
expect_true(TRUE)
expect_false(FALSE)

# check output value (wow, this is pretty meta---man)
expect_true(ignore(expect_true)(TRUE))
expect_false(ignore(expect_true)(FALSE))
expect_true(ignore(expect_false)(FALSE))
expect_false(ignore(expect_false)(TRUE))

# check behavior
expect_equal(1+1,2)

# check output value
expect_false(ignore(expect_equal)(1+1, 3))
expect_true(ignore(expect_equal)(1+1, 2))

# check behavior
expect_equivalent(2,c(x=2))

# check output value
expect_true(ignore(expect_equivalent)(2,c(x=2)))
expect_false(ignore(expect_equivalent)(2,c(x=3)))


dat <- read.csv("women.csv")
expect_equal(women, dat)


# check behavior
expect_warning(warning("foo"))
expect_error(stop("bar"))

expect_true(ignore(expect_error)(stop("foo")))
expect_false(ignore(expect_error)(stop("foo"),pattern="bar"))

expect_true(ignore(expect_warning)(warning("fu!")))
expect_false(ignore(expect_warning)(warning("fu!"), pattern="bar"))



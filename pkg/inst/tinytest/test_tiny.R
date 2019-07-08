

# check behavior
expect_true(TRUE)
expect_false(FALSE)
expect_equal(1,1)
expect_identical(1L,1L)


# check output value (wow, this is pretty meta---man)
expect_true( ignore(expect_true)(TRUE))
expect_false(ignore(expect_true)(FALSE))
expect_true( ignore(expect_false)(FALSE))
expect_false(ignore(expect_false)(TRUE))
expect_false( ignore(expect_identical)(1L,2L) )


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

# check NULL
expect_true(ignore(expect_null)(NULL))
expect_false(ignore(expect_null)(1))



# reading from file
dat <- read.csv("women.csv")
expect_equal(women, dat)


# check behavior
expect_warning(warning("foo"))
expect_error(stop("bar"))

expect_true(ignore(expect_error)(stop("foo")))
expect_false(ignore(expect_error)(stop("foo"),pattern="bar"))

expect_true(ignore(expect_warning)(warning("fu!")))
expect_false(ignore(expect_warning)(warning("fu!"), pattern="bar"))

expect_true(ignore(expect_silent)(1 + 1))
expect_false(ignore(expect_silent)(1 + "a"))
expect_false(ignore(expect_silent)(1:3 + 1:2))

expect_false(ignore(expect_message)(message("hihi"),"lol"))
expect_false(ignore(expect_message)(stop("hihi"),"lol"))
expect_false(ignore(expect_message)(warning("hihi"),"lol"))
expect_message(message("hihi, I lol"),"lol")







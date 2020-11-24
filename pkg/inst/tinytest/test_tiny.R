

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

expect_true(ignore(expect_inherits)(1, "numeric"))
expect_false(ignore(expect_inherits)(1, c("matrix", "array")))


# check NULL
expect_true(ignore(expect_null)(NULL))
expect_false(ignore(expect_null)(1))

fl <- tempfile()
expect_true(ignore(expect_equal_to_reference)(1, file=fl))
expect_true(ignore(expect_equal_to_reference)(1, file=fl))
expect_false(ignore(expect_equal_to_reference)(2, file=fl))

xx <- c(fu=1)
expect_true(ignore(expect_equivalent_to_reference)(xx, file=fl))
expect_false(ignore(expect_equal_to_reference)(xx, file=fl))




# reading from file
dat <- read.csv("women.csv")
expect_equal(women, dat)


# check behavior
expect_warning(warning("foo"))
expect_error(stop("bar"))

# class of error condition
ec <- errorCondition(message="wa babalooba", class="foo")
expect_false(ignore(expect_error)( stop(ec), class="bar" ))
expect_true (ignore(expect_error)( stop(ec), class="foo" ))

# class of warning condition
wc <- warningCondition(message="ba la bamboo", class="foo")
expect_false(ignore(expect_warning)( warning(wc), class="bar"))
expect_true (ignore(expect_warning)( warning(wc), class="foo"))

# messages to stdout
expect_stdout(print("hihi"))
expect_stdout(cat("hihi"))
expect_stdout(cat("hoho"), pattern="ho")
expect_false(ignore(expect_stdout)(cat("hihi"),pattern="ho"))



expect_true(ignore(expect_error)(stop("foo")))
expect_false(ignore(expect_error)(stop("foo"),pattern="bar"))

# single warning
expect_true(ignore(expect_warning)(warning("fu!")))
expect_false(ignore(expect_warning)(warning("fu!"), pattern="bar"))

# filtering from multiple warnings
multiwrn <- function(){
  warning("hihi")
  warning("haha")
  m <- tryCatch(warning("huhu"), warning = function(m) m)
  class(m) <- c("lulz", class(m))
  warning(m)
}

expect_warning(multiwrn(), pattern="haha")
expect_warning(multiwrn(), pattern="huhu")
expect_warning(multiwrn(), pattern="huhu", class="lulz")

# single messages
expect_true(ignore(expect_silent)(1 + 1))
expect_false(ignore(expect_silent)(1 + "a"))
expect_false(ignore(expect_silent)(1:3 + 1:2))

expect_false(ignore(expect_message)(message("hihi"),"lol"))
expect_false(ignore(expect_message)(stop("hihi"),"lol"))
expect_false(ignore(expect_message)(warning("hihi"),"lol"))
expect_message(message("hihi, I lol"),"lol")

# filtering from multiple messages
multimsg <- function(){
  message("hihi")
  message("haha")
  m <- tryCatch(message("huhu"), message = function(m) m)
  class(m) <- c("lulz", class(m))
  message(m)
}

expect_message(multimsg(), pattern="haha")
expect_message(multimsg(), pattern="huhu")
expect_message(multimsg(), pattern="huhu", class="lulz")


expect_stdout(print("hihi"),pattern="hi")
expect_stdout(cat("hihi"),pattern="hi")
expect_false(ignore(expect_stdout)(print("hihi"),pattern="ho"))


# check that info fields are filled.
msg <- "whoO0Oop"

L <- list(
    ignore(expect_true)(TRUE, info=msg)
  , ignore(expect_true)(FALSE, info=msg)
  , ignore(expect_false)(TRUE, info=msg)
  , ignore(expect_false)(FALSE, info=msg)
  , ignore(expect_equal)(1,1, info=msg)
  , ignore(expect_equal)(1,2, info=msg)
  , ignore(expect_equivalent)(1,1, info=msg)
  , ignore(expect_equivalent)(1,2, info=msg)
  , ignore(expect_identical)(1,1, info=msg)
  , ignore(expect_identical)(1,2, info=msg)
  , ignore(expect_null)(NULL,info=msg)
  , ignore(expect_null)(NA,info=msg)
  , ignore(expect_message)(message("hihi"),info=msg)
  , ignore(expect_message)(1+1, info=msg)
  , ignore(expect_warning)(warning("hihi"), info=msg)
  , ignore(expect_warning)(1+1, info=msg)
  , ignore(expect_error)(stop("hihi"),info=msg)
  , ignore(expect_silent)(1+1, info=msg)
  , ignore(expect_silent)(stop("hihi"), info=msg)
)

for ( x in L ) expect_equal(attr(x,"info"), msg)








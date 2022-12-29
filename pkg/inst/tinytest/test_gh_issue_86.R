# test the ... argument
expect_error(stop("chocolate foo(bar)"), pattern="foo(", fixed=TRUE)

expect_warning(warning("chocolate foo(bar)"), pattern="foo(", fixed=TRUE)

expect_message(message("chocolate foo(bar)"), pattern="foo(", fixed=TRUE)


expect_stdout(cat("chocolate foo(bar)"), pattern="foo(", fixed=TRUE)


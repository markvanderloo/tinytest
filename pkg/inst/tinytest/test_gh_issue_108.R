
expect_false(ignore(expect_warning)({ warning("warn"); stop("err") }, strict=TRUE))
expect_true( ignore(expect_warning)({ warning("warn")              }, strict=TRUE))
expect_false(ignore(expect_warning)({                  stop("err") }, strict=TRUE))
expect_false(ignore(expect_warning)({ 1 + 1                        }, strict=TRUE))

expect_true( ignore(expect_warning)({warning("warn"); stop("err")}, strict=FALSE))
expect_true( ignore(expect_warning)({warning("warn")             }, strict=FALSE))
expect_false(ignore(expect_warning)({                 stop("err")}, strict=FALSE))
expect_false(ignore(expect_warning)({1 + 1                       }, strict=FALSE))


expect_false(ignore(expect_message)({ 1 + 1                                       }, strict=FALSE))
expect_false(ignore(expect_message)({                                 stop("err") }, strict=FALSE))
expect_false(ignore(expect_message)({                warning("warn")              }, strict=FALSE))
expect_false(ignore(expect_message)({                warning("warn"); stop("err") }, strict=FALSE))
expect_true( ignore(expect_message)({message("msg")                               }, strict=FALSE))
expect_true( ignore(expect_message)({message("msg");                  stop("err") }, strict=FALSE))
expect_true( ignore(expect_message)({message("msg"); warning("warn")              }, strict=FALSE))
expect_true( ignore(expect_message)({message("msg"); warning("warn"); stop("err") }, strict=FALSE))


expect_false(ignore(expect_message)({ 1 + 1                                       }, strict=TRUE))
expect_false(ignore(expect_message)({                                 stop("err") }, strict=TRUE))
expect_false(ignore(expect_message)({                warning("warn")              }, strict=TRUE))
expect_false(ignore(expect_message)({                warning("warn"); stop("err") }, strict=TRUE))
expect_true(ignore(expect_message)({message("msg")                               }, strict=TRUE))
expect_false(ignore(expect_message)({message("msg");                  stop("err") }, strict=TRUE))
expect_false(ignore(expect_message)({message("msg"); warning("warn")              }, strict=TRUE))
expect_false(ignore(expect_message)({message("msg"); warning("warn"); stop("err") }, strict=TRUE))




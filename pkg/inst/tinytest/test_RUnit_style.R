
# check if RUnit style functions also work.

expect_true(ignore(checkTrue)(TRUE))
expect_true(ignore(checkFalse)(FALSE))
expect_true(ignore(checkEqual)(1+1,2))
expect_true(ignore(checkIdentical)(1L,1L))
expect_true(ignore(checkEquivalent)(c(a=1),1))





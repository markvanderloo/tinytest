
# some test to check that we can program over tests.

# this should yield 10 testresults
for ( i in 1:10 ){
  expect_equal(1+1,2)
}

# this should yield a single testresult
if ( ignore(expect_equal)(1+1,2) ){
  expect_true(TRUE)
}



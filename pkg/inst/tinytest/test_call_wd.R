# Only test at home, because R CMD check uses different working directories and
# who knows what happens at CRAN.
if( at_home() ){
  expect_false(getwd() == get_call_wd())
}


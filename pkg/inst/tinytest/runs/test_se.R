report_side_effects()
expect_equal(1+1,2)

Sys.setenv("foo"="bar")
expect_equal("a","b")



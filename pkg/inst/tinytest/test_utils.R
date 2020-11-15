
expect_error(tinytest:::stopf("foo %s","bar"),"foo bar")

expect_equal(tinytest:::humanize(0.3),"0.3s")
expect_equal(tinytest:::humanize(61),"1m 1.0s")
expect_equal(tinytest:::humanize(3601),"1h 0m 1.0s")




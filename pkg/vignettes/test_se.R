report_side_effects()
expect_equal(1+1, 2)
Sys.setenv(hihi="lol")
expect_equal(1+1, 3)
Sys.setenv(hihi="lulz ftw")


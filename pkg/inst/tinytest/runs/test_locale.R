report_side_effects()
expect_equal(1+1,2)

Sys.setlocale("LC_COLLATE","C")
expect_equal("a","b")


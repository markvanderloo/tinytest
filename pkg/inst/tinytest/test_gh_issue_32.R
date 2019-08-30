
out <- ignore(expect_equal)("foo","bar")
# Message should read 'Expected "target" got "current"',
# not the other way around
expect_true(grepl("bar.+foo",attr(out,"diff")))

out <- ignore(expect_identical)("foo","bar")
expect_true(grepl("bar.+foo",attr(out,"diff")))

out <- ignore(expect_equivalent)("foo","bar")
expect_true(grepl("bar.+foo",attr(out,"diff")))


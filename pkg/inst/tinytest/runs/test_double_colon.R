
# there should be a warning when running this test file,
# explaning that tests that are prefixed with ::: are 
# not registered (GH issue #60)

tinytest::expect_equal(1,1)


expect_equal(1,1)

tinytest::expect_equal(2,2)


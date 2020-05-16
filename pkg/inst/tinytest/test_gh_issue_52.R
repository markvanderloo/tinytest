
# small reproducible provided by Ralf Herold
expect_message(
  {
    message("catch_one")
    message("catch_two")
  },
  pattern = "two"
)







expect_stdout( str(mtcars), "carb")
expect_false(ignore(expect_stdout)(str(cars),"carb"))

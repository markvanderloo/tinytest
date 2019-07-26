

register_tinytest_extension(pkg="lulz"
  , functions=c("fee","fi","fu","bar"))

ext <- getOption("tt.extensions")
expect_equal(ext, list(lulz = c("fee","fi","fu","bar")  ))

unregister_tinytest_extension(pkg="lulz")

ext <- getOption("tt.extensions")
expect_null(ext)



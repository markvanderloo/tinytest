### A brief overview of `tinytest`


#### Package setup

I assume that `pkg` is your package directory.

1. Put files with names starting with `test` in `pkg/inst/utst`, e.g. `test_haha.R`. Test files are normal
   R scripts, interspersed with test commands, such as `expect_equal(0, myfunc(1))`.
2. Put a file named `tinytest.R` in `pkg/tests` and give it the following contents.
```
if ( require(tinytest) ){
  test_package("packagename")
}
```
Here, you need to replace `packagename` with the name of your package.


Tests will now be run by

```
R CMD build path/to/your/package
R CMD check packagename_x.y.z.tar.gz
```

3. Add `tinytest` to `Suggests:` in the `DESCRIPTION` file.

#### Interactive package testing


| Function                   | description                                 |
|----------------------------|---------------------------------------------|
| `test_all(pkg)`            | run all test files (package must be loaded).|
| `build_install_test(pkg)`  | build, install, and test in temp dir.       |
| `run_test_dir(dir)`        | run all test files in a directory.          |
| `run_test_file(file)`      | run a single test file.                     |


All functions return an object of class `tinytests`. Results can be printed to
screen or converted to data frame  with `as.data.frame` for analyses. The option
`verbose` (default: `TRUE`) toggles between showing test progress in the
terminal.

#### Test functions

The syntax of test functions closely resembles that of [testthat](https://CRAN.R-project.org/package=testthat). The main difference is that the first argument is the _desired_, and the
second argument is the _current_ value.

|Function                     | description                                   |
|-----------------------------|-----------------------------------------------|
| `expect_true`               | Argument must evaluate to `TRUE`              |
| `expect_false`              | Argument must evaluate to `FALSE`             |
| `expect_equal`              | Data and attributes of arguments must be equal|
| `expect_equivalent`         | Data of arguments must be equal               |
| `expect_warning`            | Expression must yield a warning               |
| `expect_error`              | Expression must yield an error                |


#### Print options

Test results (objects of class `tinytests`) have two printing modes: a long
format and a short, one-line format. Information that is always shown includes:

- File name and line number of failed test.
- The test call that resulted in test failure.
- The type of failure. This can be 'data' (for differences in variable
  content), 'attr' (for differences in attributes like column names), or 'xcpt'
  for exceptions (warnings, errors).

In long format, the test call and difference between desired and realized input
are shown in full. Global printing options can be set with `options(option=value)`.

|Option         | default  | description                   |
|---------------|----------|-------------------------------|
| `tt.pr.passes`| FALSE    | print passing tests?          |
| `tt.pr.limit` | 10       | how many results to print?    |
| `tt.pr.nlong` | 3        | how many tests in long format?|
| `tt.pr.color` | TRUE     | print colored output?|

It is also possible to influence these options using `print.tinytest`.


#### Run tests for an installed package

For a package called `haha` that is tested using `tinytest`, any user that has
`haha` and `tinytest` installed can run tests as follows.

```
library(haha)
library(tinytest)
run_test_dir( system.file("utst",package="haha") )
```

#### Skipping or ignoring tests 

Use `ignore(testfunction)` to run a test but not include the result in the output.

```
# both tests run, but only second is recorded.
if ( ignore(expect_equal)(2, 1+1) ){
  expect_true( 1 > 0 )
}
```
Note the placement of brackets.


Use `at_home()` to detect whether a test is running interactively, or via 
`test_package()` (i.e. the way `R CMD check` will run it).

```
if ( at_home() ){
  # run tests requiring lots of time.
}
```

#### Comparing with data stored on file

Data can be loaded from `pkg/inst/utst` (or subdirectories). A simple
test file might look like this.

```
desired <- read.csv("mycsvoutput.csv", stringsAsFactors=FALSE)
obtained <- compute_my_result()
expect_equal(desired, obtained)
```
If you wish to publish the package on CRAN, make sure that the files are small
enough for the package to be acceptable. See the [cran repository
policy](https://cran.r-project.org/web/packages/policies.html) for explicit
bounds on package size. Alternatively you can avoid installing
the data and associated test files by adding them to [.Rbuildignore](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Building-package-tarballs).



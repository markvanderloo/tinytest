### A brief overview of `tinytest`


#### Package setup

A quick way to set things up is as follows.

```
tinytest::setup_tinytest("pkgdir")
```
where `pkgdir` is a package source directory with a valid `DESCRIPTION` file

The setup is as follows.

1. Files having names starting with `test` are in `pkg/inst/tinytest`, e.g.
   `test_haha.R`. Test files are R scripts interspersed with test commands, such
   as `expect_equal(myfunc(1), 0)`.
2. `tinytest` is added to `Suggests:` in the `DESCRIPTION` file.
3. A file named `tinytest.R` is set up in `pkg/tests` to make sure that tests
   will be run by `R CMD check`.

A nice way to set up a completely new package that passes `R CMD check` is as follows
```
pkgKitten::kitten("hihi")
tinytest::setup_tinytest("hihi")
```
where `hihi` is the name of the new package.


#### Interactive package testing


| Function                        | description                                              |
|---------------------------------|----------------------------------------------------------|
| `test_all("pkgdir")`            | run all test files (pkg must be loaded).                 |
| `build_install_test("pkgdir")`  | build, install, and test in temp dir.                    |
| `run_test_dir("pkgdir")`        | run all test files in a directory (pkg must be loaded).  |
| `run_test_file("testfile")`     | run a single test file (pkg must be loaded).             |


All functions return an object of class `tinytests`. Results can be printed to
screen, summarized with `summary` or converted to data frame  with
`as.data.frame` for analyses. The option `verbose` (default: `2`) controls
showing test progress in the terminal.

#### Test functions

The syntax of test functions resembles that of
[testthat](https://CRAN.R-project.org/package=testthat).  For expectations
comparing two results, the first argument represents the _observed_ value while
the second argument represents the _desired_ value.

|Function                          | description                                          |
|----------------------------------|------------------------------------------------------|
| `expect_true`                    | Argument must evaluate to `TRUE`                     |
| `expect_false`                   | Argument must evaluate to `FALSE`                    |
| `expect_equal`                   | Data and attributes of arguments must be equal       |
| `expect_equivalent`              | Data of arguments must be equal                      |
| `expect_identical`               | Target and current must be `identical`               |
| `expect_inherits`                | Current object must inherit from the desired class   |
| `expect_null`                    | Expression must evaluate to `NULL`                   |
| `expect_equal_to_reference`      | Object must be equal to an object stored on file     |
| `expect_equivalent_to_reference` | Object must be equivalent to an object stored on file|
| `expect_stdout`                  | Expect a printed message (via `print` or `cat`)      |
| `expect_message`                 | Expression must yield a message                      |
| `expect_warning`                 | Expression must yield a warning                      |
| `expect_error`                   | Expression must yield an error                       |
| `expect_silent`                  | Expect no errors, no warnings                        |


For tests in a script there is an alternative syntax in the style of 
[RUnit](https://CRAN.R-project.org/package=RUnit). For each function of the
form `expect_lol` there is a function of the form `checkLol`.

#### Monitor side-effects

Side-effects, such as changing environment variables, locale settings, or
changing the working directory can cause hard-to-trace bugs. Add the statement
```
report_side_effects()
```
to a test file and certain types of side-effects, if any, are reported.

Alternatively, use the `side_effects` argument to any of the test runners,
for example
```
test_all("/path/to/package", side_effects=TRUE)
```

#### Run test with custom environment variables set

Temporarily set environment variables for the run of the test. For example:

```
test_all("/path/to/package", setenv=list("wa_babalooba" = "ba_la_bamboo"))
```


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
| `tt.pr.color` | TRUE     | print colored output?         |

It is also possible to influence these options using `print.tinytest`.
Colored output is suppressed on systems with a
[`"dumb"`](https://en.wikipedia.org/wiki/Computer_terminal#Dumb_terminals)
terminal.


#### Run tests for an installed package

For a package called `haha` that is tested with `tinytest`, any user that has
`haha` and `tinytest` installed can run tests as follows.

```
tinytest::test_package("haha")
```

#### Run tests in parallel

Run tests in parallel over files.
```
tinytest::test_package("haha", ncpu=3)
```
Or, for more control:
```
cl <- parallel::makeCluster(4)
parallel::clusterCall(cl, source, "R/functions.R")
test_all(cluster=cl)
stopCluster(cl)
```

#### Use extension packages

Add the following to a test file to use assertions exported by
[ttdo](https://CRAN.r-project.org/package=ttdo).
```
using(ttdo)
```
this will give you excellent diff output of the
[diffobj](https://CRAN.r-project.org/package=diffobj) package in `tinytest`
test results. The high-performance
[checkmate](https://CRAN.r-project.org/package=checkmate) package also extends
`tinytest`.


#### Skipping or ignoring tests 

Use `exit_file()` to stop executing a test file, with an optional message.
```
exit_file("I'm too tired to test today")
```

Use `ignore(testfunction)` to run a test but not include the result in the output.

```
# both tests run, but only second is recorded.
if ( ignore(expect_equal)(1 + 1, 2) ){
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
The package vignette has some tips on how to use this feature, and how you can
set up your package so `R CMD check` also runs tests protected by `at_home()`
in your environment.

#### Comparing with data stored on file

Data can be loaded from `pkg/inst/tinytest` (or subdirectories). A simple
test file might look like this.

```
desired <- read.csv("mycsvoutput.csv", stringsAsFactors=FALSE)
obtained <- compute_my_result()
expect_equal(obtained, desired)
```
If you wish to publish the package on CRAN, make sure that the files are small
enough for the package to be acceptable. See the [CRAN repository
policy](https://cran.r-project.org/web/packages/policies.html) for explicit
bounds on package size. Alternatively you can avoid installing the data and
associated test files by adding them to
[.Rinstignore](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-subdirectories).


#### More information

See the vignette.
```
vignette("using_tinytest", package="tinytest")
```






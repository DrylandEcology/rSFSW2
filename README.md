
| Unix | Windows | Release | License | Coverage | Downloads |
| :---- | :---- | :---- | :---- | :---- | :---- |
[ ![Travis build status][1]][2] | [![Appveyor build status][3]][4] | [ ![github release][5]][6] | [![license][7]][8] | [![codecov status][9]][10] | [![github downloads][11]][12] |

[1]: https://travis-ci.org/DrylandEcology/rSFSW2.svg?branch=master
[2]: https://travis-ci.org/DrylandEcology/rSFSW2
[3]: https://ci.appveyor.com/api/projects/status/die00t8rjjhrb8i0/branch/master?svg=true
[4]: https://ci.appveyor.com/project/dschlaep/rSFSW2/branch/master
[5]: https://img.shields.io/github/release/DrylandEcology/rSFSW2.svg?label=current+release
[6]: https://github.com/DrylandEcology/rSFSW2/releases
[7]: https://img.shields.io/github/license/DrylandEcology/rSFSW2.svg
[8]: https://www.gnu.org/licenses/gpl.html
[9]: https://codecov.io/gh/DrylandEcology/rSFSW2/branch/master/graph/badge.svg
[10]: https://codecov.io/gh/DrylandEcology/rSFSW2
[11]: https://img.shields.io/github/downloads/DrylandEcology/rSFSW2/total.svg
[12]: https://github.com/DrylandEcology/rSFSW2

<br>

# rSFSW2: A R package to create soil water balance simulation experiment

Please cite the package if you publish results based on simulations carried
out with our package, see `citation("rSFSW2")`, and we would like to hear
about your publication.

Some other references

* Bradford, J. B., D. R. Schlaepfer, and W. K. Lauenroth. 2014. Ecohydrology of
  adjacent sagebrush and lodgepole pine ecosystems: The consequences of climate
  change and disturbance. Ecosystems 17:590-605.
* Palmquist, K.A., Schlaepfer, D.R., Bradford, J.B., and Lauenroth, W.K. 2016.
  Mid-latitude shrub steppe plant communities: climate change consequences for
  soil water resources. Ecology 97:2342-2354.
* Schlaepfer, D. R., W. K. Lauenroth, and J. B. Bradford. 2012. Ecohydrological
  niche of sagebrush ecosystems. Ecohydrology 5:453-466.


### Obtain the source package

There are several options:

- Download the
  [package zip file](https://github.com/DrylandEcology/rSFSW2/archive/master.zip)
  via your web browser.

- Use git to clone
  ```
  git clone -b master --single-branch https://github.com/DrylandEcology/rSFSW2.git rSFSW2
  ```

- Use git to clone step by step
  ```
  git clone https://github.com/DrylandEcology/rSFSW2.git rSFSW2
  cd rSFSW2/
  git checkout master
  ```

### Installation

'rSFSW2' will compile some c code via 'Rcpp'. Your computer must be set up
adequately.
- If you use a Windows OS, then you need the
  [Rtools](http://cran.us.r-project.org/bin/windows/Rtools/)
  installed that match your R version; please find further information for
  instance [here](https://www.biostat.wisc.edu/~kbroman/Rintro/Rwinpack.html).
- If you use a macOS, then you need [Xcode](https://developer.apple.com/xcode/)
  and its
  [command-line tools](https://developer.apple.com/library/content/technotes/tn2339/_index.html)
  installed; please find further information for instance
  [here](https://railsapps.github.io/xcode-command-line-tools.html).


After you downloaded the source package, run
```
R CMD INSTALL rSFSW2
```

Or do all at once from within R:
```{r}
system2(command = "git", args = "clone -b master --single-branch https://github.com/DrylandEcology/rSFSW2.git rSFSW2")
tools::Rcmd(args = paste("INSTALL rSFSW2"))
```

### Binary package version
If you want a binary version of the 'rSFSW2' package (e.g., to distribute to
someone without development tools) for a platform to which you do not have
access, then you may consider using one of the cloud services (no endorsements):
- [r-hub](https://builder.r-hub.io) offers different Linux, Windows, and mac OS
  flavors as targets
- [win-builder](http://win-builder.r-project.org/) offers Windows OS as target

Alternatively, you may access the previous binary package version for Windows
OS from our CI appveyor service if the build was successful and an artifact was
generated for the binary package (this would be named 'rSWSF2_X.Y.Z.zip' with
version number X.Y.Z) from
[here](https://ci.appveyor.com/project/dschlaep/rSFSW2/build/artifacts).
If the latest build should have failed, then you may want to check out the
'History' tab for binaries of older versions.


<br>

# Use rSFSW2 for your simulation project

Familiarize yourself with the demos and information at ```package?rSFSW2```
as well as FAQs with ```vignette("rSFSW2_FAQs", package = "rSFSW2")```.

__Setup a new simulation project__:
1) Install and attach 'rSFSW2' if not already done so (Note: required version
   of rSOILWAT2 must already be present)
2) Create a skeleton project `setup_rSFSW2_project_infrastructure(dir_prj =
   "path/to/project_folder")
   - This function will copy a default version of '1_Input' and the three
   demo R files to your directory
3) Work your way through 'SFSW2_project_code.R', i.e., define paths and actions,
   and provide simulation project description in 'SFSW2_project_descriptions.R'
   and run settings in 'SFSW2_project_settings.R'


<br>

# How to contribute
You can contribute to this project in different ways:

1. Reporting [issues](https://github.com/DrylandEcology/rSFSW2/issues)
2. Contributing code following our
   [protocols/guidelines](https://github.com/DrylandEcology/DrylandEcologyProtocols)
   and sending a
   [pull request](https://github.com/DrylandEcology/rSFSW2/pulls)

<br>

### __Code development: Tests, documentation, and code__ form a trinity

- __Code style__
  * Use code style that passes our
    [`lintr`](https://github.com/jimhester/lintr) unit tests
    which basically reflect
    [Hadley's style recommendation](http://r-pkgs.had.co.nz/r.html#style).
    Note: we require `lintr v1.0.2.900` or later.
  * Use 2-spaces instead of tabs and indent code hierarchically
  * Note, many function and variable names are "ancient" (too long; combine
    snake and camel-case)

- __Updates to input files and/or demo code__
  * If `SOILWAT2` and `rSOILWAT2` change their `default` inputs, then `rSFSW2`
    will automatically experience these changes through function
    `read_SOILWAT2_DefaultInputs`. This function may need to be updated
    accordingly to provide suitable `defaults` for `rSFSW2` runs.
  * If you change 'input files' in `data-raw/1_Input` (e.g., added a new column
    to experimental/design treatment file) then update the `R/sysdata.rda`
    object by running the Rscript from terminal
    `./data-raw/prepare_default_project_infrastructure.R`.
    The file `R/sysdata.rda` is used to setup a new simulation project.
  * Additionally, if 'input files' and/or 'demo code' in `demo/` changes, then
    update the unit test 'test project' `tests/test_data/TestPrj4/` by
    running the Rscript from terminal
    `./data-raw/update_test_project_infrastructure.R` and make any necessary
    additional changes by hand.

- __Interactive code development__
  * Use (a copy of) `tests/test_data/TestPrj4/` as basis to interact with code
    and a real simulation project, for instance:
    ```{r}
    library("devtools")
    load_all()
    setwd("rSFSW2/tests/test_data/TestPrj4/")

    # Adjust inputs if necessary and insert break points (e.g., calls to
    # `browser()` or `stop()`) if needed
    source("SFSW2_project_code.R") # run `TestPrj4` and stop where you need it

    # Develop/debug code
    # e.g., compare output with reference as if running `tests()`:
    compare_two_dbOutput("../0_ReferenceOutput/dbOutput_TestPrj4_v2.7.4.sqlite3", "4_Simulation/dbOutput.sqlite3")

    # Clean up `TestPrj4/`
    delete_test_output(dir_test = ".")
    ```

  * *Do not* commit, please:
    * Any changes to settings/inputs etc. in your local copy unless those
      changes are a feature of your coding task
    * Do not comment/turn-off any tests and/or checks
    * Print statements for local debugging purposes
    * Package bundles or binaries (e.g., as from `R CMD build`)
    * Package check reports (e.g., as from `R CMD check`)
    * Built vignettes
    * etc


- __Code documentation__
  * Read the section 'Object documentation' in
    [Wickham's book 'R packages'](http://r-pkgs.had.co.nz/man.html)
  * Use [roxygen2](https://cran.r-project.org/web/packages/roxygen2/vignettes/formatting.html)
    to write inline code documentation
  * Update help pages and NAMESPACE with the command `devtools::document()`
  * Ideally, add examples to function documentation and check these examples
    with the command `devtools::run_examples()`
  * Ideally, expand and/or add vignettes.


- __Code tests__
  * Notes:
    * Our code coverage is incomplete as of now at [![codecov status][9]][10];
      thus, any change may introduce bugs that may not be detected by our
      testing framework.
    * Please be careful and considerate and strive to write tests for any new
      feature.
    * Our tests behave differently depending on several run-time conditions:
      1) Level of verbosity is determined by interactive/non-interactive status
         whether or not simulations are run in parallel (non-interactive) or
         sequentially (interactive),
      2) Simulation runs are processed in parallel if session is
         non-interactive, not on travis-ci, not on appveyor-ci, and not on CRAN
         whereas they are processed sequentially if session is interactive,
         on travis-ci, on appveyor-ci, or on CRAN
      3) Live access to the internet is required by some unit tests, e.g.,
         `tests/testthat/test_netCDF_functions.R` and
         `tests/testthat/test_WeatherDB_DayMet.R`
      4) Some unit tests are skipped if on CRAN, and/or on travis-ci, and/or
         on appveyor-ci, e.g.,
         `test/testthat/test_rSFSW2_Spelling.R` and
         `test/testthat/test_rSFSW2_CodeStylePractices.R`

  * __Test code locally during code development:__
    * Interactive execution and exploration
    * Interactive execution of individual expectations `expect_*` and/or
      `test_that()` statements; write new expectations at the same time as
      writing and developing code.
    * Run tests from an individual test file with `testthat::test_file()`
        - You may likely need to first load the latest code version with
          `devtools::load_all()` for R code, and with
          `devtools::load_all(recompile = TRUE)` if your changes include C code
        - Most likely, this will run tests as if on CRAN (depending on your
          specific setup), i.e., it will skip several of our tests:
           * it is set as `NOT_CRAN="true"` if run with:
              * `devtools::test()` unless `NOT_CRAN` was previously set
              * `devtools::check(cran = FALSE)`
              * `R CMD check *tar.gz`
           * it is set as `NOT_CRAN="false"` (i.e, behaving as if run on CRAN)
           if run with:
              * `Sys.setenv(NOT_CRAN = "false"); devtools::test()`
              * `devtools::check(cran = TRUE)`
              * `R CMD check *tar.gz --as-cran`

        - If you don't like the output format of the tests (which differs
          depending on whether you run R interactively or not, whether you run
          R via RStudio or not, etc.), then chose a testthat-'reporter'
          explicitly, e.g., `testthat::test_file(reporter = SummaryReporter)`
    * Run all tests together with `testthat::test()`, but it is a waste of time
      and resources to re-run tests again and again during development
      that are not affected by your code changes
    * Currently defunct: Ideally, run test projects in repository
      [rSFSW2_tools](https://github.com/DrylandEcology/rSFSW2_tools)
      and add a new test project, if necessary due to new features.

  * __Run the following steps locally__
    in order to prepare a pull-request or commit that will be reviewed.
    Fix any problem and repeat as necessary.

    1. Make sure that the anticipated version of `rSOILWAT2` is indeed
       installed, e.g.,
       ```{r}
       packageVersion("rSOILWAT2")
       ```

    1. Make sure that the documentation is up-to-date with:
       ```{r}
       devtools::document()
       ```

    1. Run and check the code from the examples and vignettes:
       ```{r}
       devtools::run_examples()
       ```

    1. Run tests as if not on CRAN, in an interactive R session,
       and with a sequential schedule.
       ```{r}
       # Run in R.app, RStudio, or in an R terminal-session:
       Sys.setenv(NOT_CRAN = "true")
       devtools::test()
       ```
       Notes:
        - Make sure that no test is skipped. Investigate if any is skipped.
        - Investigate if any warning is reported.
        - This combines unit tests, documentation and code-style checks,
          and integration tests (e.g., `TestPrj4`); the latter two take a
          substantial amount of time to complete.
       The environmental variable `RSFSW2_ALLTESTS` determines whether or not
       long-running expectations/unit-tests are skipped; the default is "true",
       i.e., run all expectations/unit-tests. You may decide to run tests
       while temporary skipping time-intensive tests, e.g.,
       - `Sys.setenv(RSFSW2_ALLTESTS = "false"); devtools::test()`
       - `RSFSW2_ALLTESTS="false" R CMD check *tar.gz`

    1. Run tests as if not on CRAN, in an non-interactive session,
       and with a parallel schedule.
       ```{bash}
       # Run via shell in the terminal:
       R CMD INSTALL .
       Rscript -e 'Sys.setenv(NOT_CRAN = "true"); devtools::test()'
       ```
       Notes:
        - Parallel workers will load the package `rSFSW2` "normally", i.e.,
          from the R library path. Thus, the workers do not see the development
          version. Therefore, we need to install the current version before
          running tests in parallel.
          You can convince yourself of this by first removing `rSFSW2` with
          `remove.packages("rSFSW2")` and then run above command -- the tests
          will fail with errors such as `object 'SFSW2_glovars' not found` or
          `all(tp[["res"]][, "has_run"]) isn't true`.
        - Make sure that the integration test (e.g., `TestPrj4`) was indeed run
          in parallel (output reports on the number of workers).

    1. The environmental variable `RSFSW2_SAVETESTS` determines whether or not
       the otherwise invisible internal `testthat` results are saved to file
       which can be useful for debugging; the default is "true" in
       non-interactive mode, i.e., save results, and "false" in interactive
       mode.
       To set it to true, e.g.,
         * `Sys.setenv(RSFSW2_SAVETESTS = "true"); devtools::test()`
         * `RSFSW2_SAVETESTS="true" R CMD check *tar.gz`

       To illustrate how to read in such reporter output and display
       its content (note: you may need to adjust the file path):
       ```{r}
         utres <- readRDS(file.path("rSFSW2.Rcheck", "tests", "testthat_results.rds"))
         r <- ListReporter$new()
         r$start_reporter()
         force(utres) # print test results with `ListReporter`
         r$end_reporter()

         utres[[1]] # explore results of first set of tests
       ```



    1. Run R package-level checks as if on CRAN.
       ```{r}
       # Run in R.app, RStudio, or in an R terminal-session:
       Sys.setenv(NOT_CRAN = "false")
       devtools::check(cran = TRUE)
       ```
       Notes:
        - Avoid adding new `R CMD check` warnings and/or notes; see, milestone
          [Clean code](https://github.com/DrylandEcology/rSFSW2/milestone/2)


    __Notes__: The above steps can also be executed with different commands
    and there are more combinations that could be tested. For instance, you
    could use `R CMD` instead of `devtools::check`, e.g., see
    [Writing R Extensions](https://cran.r-project.org/doc/manuals/R-exts.html).
    As an example, R-package level checks could also be run with:
    ```{bash}
    R CMD build . && R CMD check *tar.gz
    ```
    Unless the build-step fails due to latex-troubles while building the
    vignette and/or help pages, then maybe:
    ```{bash}
    R CMD build --no-build-vignettes --no-manual .
    R CMD check *tar.gz --ignore-vignettes --no-manual
    ```
    You could also pass the argument `--as-cran` to `R CMD check` to simulate
    checks as if on CRAN.

    1. Fix any problem and repeat.


  * On github:
    * The command-line checks which include our unit tests will be run on the
      continuous integration frameworks 'travis' and 'appveyor'
    * Development/feature branches can only be merged into master if they pass
      all checks
    * Ideally, each pull-request will include fully-tested changes, at least
      they should be as thoroughly tested as master so that overall code
      coverage after merging into master does not decrease.
    * Please, don't use the CIs for debugging -- debug locally

  * We use the framework of [testthat](https://github.com/hadley/testthat) for
    unit testing and other tests for the package

  * Read the section 'Testing' in
    [Wickham's book 'R packages'](http://r-pkgs.had.co.nz/tests.html)
    for additional information


<br>

# Code of conduct
Please note that this project is released with a
[Contributor Code of Conduct](CONDUCT.md). By participating in this project you
agree to abide by its terms.


<br>

# Funding
Work on this package has been supported by various funds managed by
Dr. Bill Lauenroth (Yale University), Dr. John Bradford (USGS), and
Dr. Daniel Schlaepfer.


<br>

# License
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, [version 3 of the License](LICENSE).

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.


<br>

# Notes

__Organization renamed from Burke-Lauenroth-Lab to DrylandEcology on Dec 22, 2017__

All existing information should
[automatically be redirected](https://help.github.com/articles/renaming-a-repository/) to the new name.
Contributors are encouraged, however, to update local clones to [point to the new URL](https://help.github.com/articles/changing-a-remote-s-url/), i.e.,
```
git remote set-url origin https://github.com/DrylandEcology/rSFSW2.git
```


__Repository renamed from SoilWat_R_Wrapper to rSFSW2 on Feb 23, 2017__
All existing information should [automatically be redirected](https://help.github.com/articles/renaming-a-repository/) to the new name.
Contributors are encouraged, however, to update local clones to [point to the new URL](https://help.github.com/articles/changing-a-remote-s-url/), i.e.,
```
git remote set-url origin https://github.com/DrylandEcology/rSFSW2.git
```

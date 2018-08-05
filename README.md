
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
2. Contributing code and sending a [pull request](https://github.com/DrylandEcology/rSFSW2/pulls)

Please follow our [guidelines](https://github.com/DrylandEcology/workflow_guidelines),
particularly,
  * Use code style that passes our `lintr` unit tests which basically reflect
    [Hadley's style recommendation](http://r-pkgs.had.co.nz/r.html#style)
  * Use 2-spaces as one tab and indent code hierarchically
  * Note, many function and variable names are "ancient" (too long; combine
    snake and camel-case)


### __Tests, documentation, and code__ form a trinity
- Interactive code development
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
    * Package bundles or binaries (e.g., as from `R CMD build`)
    * Package check reports (e.g., as from `R CMD check`)
    * Built vignettes

- Code documentation
  * Read the section 'Object documentation' in
    [Wickham's book 'R packages'](http://r-pkgs.had.co.nz/man.html)
  * Use [roxygen2](https://cran.r-project.org/web/packages/roxygen2/vignettes/formatting.html)
    to write inline code documentation
  * Update help pages and NAMESPACE with the command `devtools::document()`
  * Ideally, add examples to function documentation and check these examples
    with the command `devtools::run_examples()`
  * Ideally, expand the vignettes.

- Code tests
  * Notes:
    * Our code coverage is incomplete as of now at [![codecov status][9]][10];
      thus, any change may introduce bugs that may not be detected by our
      testing framework.
    * Please be careful and considerate and strive to write tests for any new
      feature.
  * Locally during code development:
    * Interactive execution and exploration
    * Run tests from an individual test file with `testthat::test_file()`.
    * Currently defunct: Ideally, run test projects in repository
      [rSFSW2_tools](https://github.com/DrylandEcology/rSFSW2_tools)
      and add a new test project, if necessary due to new features.
  * Locally before finalizing a pull-request and/or code review:
    1) Run code from examples and vignettes with `devtools:run_examples()`
    2) Run all tests with the command `devtools::test()`. Note: this combines
       unit tests and integration tests (e.g., `TestPrj4`); the latter take a
       substantial amount of time to complete.
    3) Run command-line checks, i.e., `R CMD check` or `devtools::check()`.
       - Note: `R CMD check` requires a built package, i.e.,
         run `R CMD build . && R CMD check *tar.gz`; see `.travis.yml` if the
         build-step fails due to latex-troubles while vignette/help building.
       - Different tests/checks are run under different settings depending on
         the environmental setting `NOT_CRAN` and whether or not integration
         tests (i.e., those that run `TestPrj4`) are executed in parallel or
         serial mode. Thus, for greatest coverage, run checks both with and
         without option `--as-cran` respectively argument `cran` of function
         `devtools::check()` -- on the command line and interactively.
    4) Fix any problem and repeat.
  * On github:
    * The command-line checks which include our unit tests will be run on the
      continuous integration frameworks 'travis' and 'appveyor'
    * Development/feature branches can only be merged into master if they pass
      all checks
    * Please, don't use the CIs for debugging -- debug locally
  * We use the framework of [testthat](https://github.com/hadley/testthat) for
    unit testing and other tests for the package
  * Read the section 'Testing' in
    [Wickham's book 'R packages'](http://r-pkgs.had.co.nz/tests.html)
    for additional information

### __Updates to input files and/or demo code__
- If `SOILWAT2` and `rSOILWAT2` change their `default` inputs, then `rSFSW2`
  will automatically experience these changes through function
  `read_SOILWAT2_DefaultInputs`. This function may need to be updated
  accordingly to provide suitable `defaults` for `rSFSW2` runs.
- If you change 'input files' in `data-raw/1_Input` (e.g., added a new column
  to experimental/design treatment file) then update the `R/sysdata.rda`
  object by running the Rscript from terminal
  `./data-raw/prepare_default_project_infrastructure.R`.
  The file `R/sysdata.rda` is used to setup a new simulation project.
- Additionally, if 'input files' and/or 'demo code' in `demo/` changes, then
  update the unit test 'test project' `tests/test_data/TestPrj4/` by
  running the Rscript from terminal
  `./data-raw/update_test_project_infrastructure.R` and make any necessary
  additional changes by hand.


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

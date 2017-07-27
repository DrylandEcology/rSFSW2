[![Travis-CI Build Status](https://travis-ci.org/Burke-Lauenroth-Lab/rSFSW2.svg?branch=master)](https://travis-ci.org/Burke-Lauenroth-Lab/rSFSW2)
  [![Appveyor Build status](https://ci.appveyor.com/api/projects/status/90jbn06qby9a8b7n?svg=true)](https://ci.appveyor.com/project/dschlaep/rSFSW2)
  [![codecov status](https://codecov.io/gh/Burke-Lauenroth-Lab/rSFSW2/branch/master/graph/badge.svg)](https://codecov.io/gh/Burke-Lauenroth-Lab/rSFSW2)



# rSFSW2: A R package to create soil water balance simulation experiment

Some recent references

* Bradford, J. B., D. R. Schlaepfer, and W. K. Lauenroth. 2014. Ecohydrology of adjacent
  sagebrush and lodgepole pine ecosystems: The consequences of climate change and
  disturbance. Ecosystems 17:590-605.
* Palmquist, K.A., Schlaepfer, D.R., Bradford, J.B., and Lauenroth, W.K. 2016.
  Mid-latitude shrub steppe plant communities: climate change consequences for soil water
  resources. Ecology 97:2342–2354.
* Schlaepfer, D. R., W. K. Lauenroth, and J. B. Bradford. 2012. Ecohydrological niche of
  sagebrush ecosystems. Ecohydrology 5:453-466.


### Obtain the source package

There are several options:

- Download the
  [package zip file](https://github.com/Burke-Lauenroth-Lab/rSFSW2/archive/master.zip)
  via your web browser.

- Use git to clone
  ```
  git clone -b master --single-branch https://github.com/Burke-Lauenroth-Lab/rSFSW2.git rSFSW2
  ```

- Use git to clone step by step
  ```
  git clone https://github.com/Burke-Lauenroth-Lab/rSFSW2.git rSFSW2
  cd rSFSW2/
  git checkout master
  ```

### Installation

'rSFSW2' will compile some c code via 'Rcpp'. Your computer must be set up adequately.
- If you use a Windows OS, then you need the
  [Rtools](http://cran.us.r-project.org/bin/windows/Rtools/)
  installed that match your R version; please find further information for instance
  [here](https://www.biostat.wisc.edu/~kbroman/Rintro/Rwinpack.html).
- If you use a macOS, then you need [Xcode](https://developer.apple.com/xcode/) and
  its [command-line tools](https://developer.apple.com/library/content/technotes/tn2339/_index.html)
  installed; please find further information for instance
  [here](https://railsapps.github.io/xcode-command-line-tools.html).


After you downloaded the source package, run
```
R CMD INSTALL rSFSW2
```

Or do all at once from within R:
```{r}
system2(command = "git", args = "clone -b master --single-branch https://github.com/Burke-Lauenroth-Lab/rSFSW2.git rSFSW2")
tools::Rcmd(args = paste("INSTALL rSFSW2"))
```

### Binary package version
If you want a binary version of the 'rSFSW2' package (e.g., to distribute to someone
without development tools) for a platform to which you do not have access, then you may
consider using one of the cloud services (no endorsements):
- [r-hub](https://builder.r-hub.io) offers different Linux, Windows, and mac OS flavors as targets
- [win-builder](http://win-builder.r-project.org/) offers Windows OS as target

Alternatively, you may access the previous binary package version for Windows OS from our
CI appveyor service if the build was successful and an artifact was generated for the
binary package (this would be named 'rSWSF2_X.Y.Z.zip' with version number X.Y.Z) from
[here](https://ci.appveyor.com/project/dschlaep/rSFSW2/build/artifacts). If the latest
build should have failed, then you may want to check out the 'History' tab for binaries
of older versions.

# Use rSFSW2 for your simulation project

Familiarize yourself with the demos and information at ```package?rSFSW2```

__Setup a new simulation project__:
1) Install and attach 'rSFSW2' if not already done so (Note: required version of
   rSOILWAT2 must already be present)
2) Create a skeleton project `setup_rSFSW2_project_infrastructure(dir_prj =
   "path/to/project_folder")
    - This function will copy a default version of '1_Data_SWInput' and the three demo R
    files to your directory
3) Work your way through 'SFSW2_project_code.R', i.e., define paths and actions, and
   provide simulation project description in 'SFSW2_project_descriptions.R' and run
   settings in 'SFSW2_project_settings.R'


# How to contribute
You can contribute to this project in different ways:

1. Reporting [issues](https://github.com/Burke-Lauenroth-Lab/rSFSW2/issues)
2. Contributing code and sending a [pull request](https://github.com/Burke-Lauenroth-Lab/rSFSW2/pulls)

Please follow our [guidelines](https://github.com/Burke-Lauenroth-Lab/workflow_guidelines).


# Code of conduct
Please note that this project is released with a
[Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree
to abide by its terms.


# License
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, [version 3 of the License](LICENSE).

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.


# Notes
__Repository renamed from SoilWat_R_Wrapper to rSFSW2 on Feb 23, 2017__
All existing information should [automatically be redirected](https://help.github.com/articles/renaming-a-repository/) to the new name.
Contributors are encouraged, however, to update local clones to [point to the new URL](https://help.github.com/articles/changing-a-remote-s-url/), i.e.,
```
git remote set-url origin https://github.com/Burke-Lauenroth-Lab/rSFSW2.git
```

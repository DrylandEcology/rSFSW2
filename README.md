[![Travis-CI Build Status](https://travis-ci.org/Burke-Lauenroth-Lab/rSFSW2.svg?branch=master)](https://travis-ci.org/Burke-Lauenroth-Lab/rSFSW2)
  [![Build status](https://ci.appveyor.com/api/projects/status/kpbf892lmb77x69i?svg=true)](https://ci.appveyor.com/project/dschlaep/rSFSW2)
  [![Coverage Status](https://coveralls.io/repos/github/Burke-Lauenroth-Lab/rSFSW2/badge.svg?branch=master)](https://coveralls.io/github/Burke-Lauenroth-Lab/rSFSW2?branch=master)


# rSFSW2: A R package to create soil water balance simulation experiment

Start with the demos and information at ```package?rSFSW2```

Some recent references

* Bradford, J. B., D. R. Schlaepfer, and W. K. Lauenroth. 2014. Ecohydrology of adjacent
  sagebrush and lodgepole pine ecosystems: The consequences of climate change and
  disturbance. Ecosystems 17:590-605.
* Schlaepfer, D. R., W. K. Lauenroth, and J. B. Bradford. 2012. Ecohydrological niche of
  sagebrush ecosystems. Ecohydrology 5:453-466.

## Note: repository renamed from SoilWat_R_Wrapper to rSFSW2 on Feb 23, 2017
All existing information should [automatically be redirected](https://help.github.com/articles/renaming-a-repository/) to the new name.
Contributors are encouraged, however, to update local clones to [point to the new URL](https://help.github.com/articles/changing-a-remote-s-url/), i.e.,
```
git remote set-url origin https://github.com/Burke-Lauenroth-Lab/rSFSW2.git
```

# __Setup a new simulation project__:
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


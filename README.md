[![Build Status](https://travis-ci.org/gluc/data.tree.svg?branch=master)](https://travis-ci.org/gluc/data.tree)
[![codecov.io](http://codecov.io/github/gluc/data.tree/coverage.svg?branch=master)](http://codecov.io/github/gluc/data.tree?branch=master) [![CRAN Version](http://www.r-pkg.org/badges/version/data.tree)](http://cran.rstudio.com/web/packages/data.tree)


# data.tree
An R package to manage hierarchical data and tree structures

Hierarchical data is ubiquitous in statistics and programming (XML, search trees, family trees, classification, file system, etc.). However, no general-use *tree data structure* is available in R. 
Where tabular data has data.frame, hierarchical data is often modeled in lists of lists or similar makeshifts. These
structures are often dificult to manage.
This is where the data.tree package steps in. It lets you build trees of hierarchical
data for various uses: to print, to rapid prototype search algorithms, to test out new classification algorithms, and much more.

The package provides functionality to convert from various formats such as data.frames, list of lists, dendrograms, ape phylo, igraph, JSON, YAML, and more.


# NOTE:
The latest from github has some breaking changes compared to CRAN. See [NEWS](https://github.com/gluc/data.tree/blob/master/NEWS) for details.

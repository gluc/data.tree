CRAN: [![CRAN Version](https://www.r-pkg.org/badges/version/data.tree)](https://cran.r-project.org/package=data.tree/) [![CRAN downloads](https://cranlogs.r-pkg.org/badges/data.tree)](https://cran.r-project.org/package=data.tree/)


<!-- badges: start -->
[![R-CMD-check](https://github.com/gluc/data.tree/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gluc/data.tree/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


# data.tree
An R package to manage hierarchical data and tree structures

Hierarchical data is ubiquitous in statistics and programming (XML, search trees, family trees, classification, file system, etc.). However, no general-use *tree data structure* is available in base R. 
Where tabular data has data.frame, hierarchical data is often modeled in lists of lists or similar makeshifts. These
structures are often difficult to manage.
This is where the data.tree package steps in. It lets you build trees of hierarchical
data for various uses: to print, plot and visualize, to generate breakdowns, to integrate with html widgets, to rapid prototype search algorithms, to test out new classification ideas, and much more.

Tree structures can be created programmatically, or by conversion. The package provides functionality to convert from and to various formats such as data.frames, list of lists, dendrograms, partykit, ape phylo, igraph, JSON, YAML, and more.

# Learn More

To get started, you might want to read the [introduction vignette](https://CRAN.R-project.org/package=data.tree/vignettes/data.tree.html). There is also a vignette containing some [examples and applications](https://CRAN.R-project.org/package=data.tree/vignettes/applications.html).

The manual is [here](https://CRAN.R-project.org/package=data.tree/data.tree.pdf)

# NOTE:
The latest from github dev branch may have some breaking changes compared to CRAN. See [NEWS](https://github.com/gluc/data.tree/blob/dev/NEWS) for details.


# Conventions:

Coding Conventions: Google Style Guide, see https://google.github.io/styleguide/Rguide.xml

Versioning Conventions: SemanticVersioning. See https://semver.org/ for details

Branching Conventions: GitFlow. See https://www.atlassian.com/git/tutorials/comparing-workflows/gitflow-workflow

Pull Requests: Very welcome! Please branch from the dev branch.

# Dev Guide

To develop on a new environment, you need to:

1. pull from CRAN (dev branch)
2. install R
3. install RStudio
4. install RTools (https://cran.rstudio.com/bin/windows/Rtools/)
4. install tinytex
  1. install.packages('tinytex')
  2. tinytex::install_tinytex()
  3. tinytex:::install_yihui_pkgs()
5. re-start RStudio
6. install devtools by running `install.packages("devtools")`
 
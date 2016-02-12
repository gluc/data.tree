Before a release, do the following:
1. make sure tests pass
2. review documentation, especially Node
3. review vignettes
4. Check build
5. Commit to git, make sure travis and appveyor pass
6. Make sure devel passes, by running devtools::build_win(version = "R-devel")
7. Make sure r-oldrel passes (easiest is to run it on local windows)
-> if any of these fail, go back to 4.!
8. check reverse dependencies by running devtools::revdep_check()
9. update cran-comments.md
10. merge into master and push
11. create release on github in master, tag it as pre-release
12. submit it to cran by calling devtools::release()
13. once accepted by CRAN, remove the pre-release flag on github

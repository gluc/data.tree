Before a release, do the following:
1. Set date in DESCRIPTION
2. Make sure NEWS is up to date
3. make sure tests pass
4. review documentation, especially Node
5. review vignettes (especially if png need updating)
6. Check build
7. Commit to git, make sure travis and appveyor pass
8. make sure we have adequate coverage
9. Make sure devel passes, by running devtools::build_win(version = "R-devel")
10. Make sure r-oldrel passes (easiest is to run it on local windows)
-> if any of these fail, go back to 6.!
11. check reverse dependencies by running devtools::revdep_check()
12. update cran-comments.md
13. merge into master and push
14. create release on github in master, tag it as pre-release
15. submit it to cran by calling devtools::release()
16. once accepted by CRAN, remove the pre-release flag on github

Before a release, do the following:
1. Set date in DESCRIPTION
2. Make sure NEWS is up to date
3. make sure tests pass (by running devtools::test())
4. review documentation, especially Node
5. review vignettes (especially if png need updating). Run devtools::spell_check()
6. Check build by running devtools::check()
7. Commit to git, make sure github actions run through
8. make sure we have adequate coverage (tests are run by github actions and uploaded to codecov)
9. make sure the github action for R-CMD-check is running through for all environments
10. Make sure devel passes, by running devtools::check_win_devel
11. Make sure r-oldrel passes (easiest is to run it on local windows)
12. Run devtools::check_rhub()
13. Run rhub::check_for_cran()
-> if any of these fail, go back to 6.!
13. check reverse dependencies by running 
    1. crancache_clean(force = FALSE)
    1. revdepcheck::reset()
    1. revdepcheck::revdep_check(pkg = ".", quiet = TRUE, num_workers = 2, timeout = as.difftime(300, units="mins")) 
    1. you might need to restart the R session. (from github if not yet published to CRAN devtools::install_github("r-lib/revdepcheck") 
14. update cran-comments.md
15. merge into master and push
16. create release on github in master, tag it as pre-release
17. submit it to cran by calling devtools::release()
18. once accepted by CRAN, remove the pre-release flag on github

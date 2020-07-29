Before a release, do the following:
1. Set date in DESCRIPTION
2. Make sure NEWS is up to date
3. make sure tests pass
4. review documentation, especially Node
5. review vignettes (especially if png need updating). Run devtools::spell_check()
6. Check build
7. Commit to git, make sure travis and appveyor pass (in case it fails, you may want to clean cache)
8. make sure we have adequate coverage
9. Make sure devel passes, by running devtools::check_win_devel
10. Make sure r-oldrel passes (easiest is to run it on local windows)
11. Run devtools::check_rhub()
12. Run rhub::check_for_cran()
-> if any of these fail, go back to 6.!
12. check reverse dependencies by running revdepcheck::revdep_check(num_workers = 4) (from github if not yet published to CRAN devtools::install_github("r-lib/revdepcheck")
13. update cran-comments.md
14. merge into master and push
15. create release on github in master, tag it as pre-release
16. submit it to cran by calling devtools::release()
17. once accepted by CRAN, remove the pre-release flag on github

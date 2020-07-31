## General Comments

The devel checks on CRAN were failing due to a fix in R base. I adjusted the package accordingly.
Otherwise, a few improvements (code and documentation).
Best Regards, Christoph

## Test environments

* linux / travis (release, oldrel, devel) -> OK
* Win / appveyor (devel 32, release 64, stable, patched) -> devel fails because utf8 not available (unrelated to my package). release and stable OK
* rhub::check_on_solaris -> OK
* rhub::check(".", platform = "macos-highsierra-release-cran") -> OK
* rhub::check_for_cran -> OK for WIN. PREPERROR for linux platforms. I believe this is unrelated to my package. I commented on an existing issue  mentioned in github: https://github.com/r-hub/rhub/issues/173#issuecomment-666947965

## R CMD check results

There were no ERRORs or WARNINGs.

## Downstream dependencies checked

The following downstream dependencies were checked:

✓ ahp 0.2.12                             ── E: 0     | W: 0     | N: 0    
✓ behaviorchange 0.2.2                   ── E: 0     | W: 0     | N: 1    
✓ Cluster.OBeu 1.2.3                     ── E: 0     | W: 0     | N: 0    
✓ cola 1.4.1                             ── E: 0     | W: 0     | N: 1    
✓ collapsibleTree 0.1.7                  ── E: 0     | W: 0     | N: 1    
✓ DecisionAnalysis 1.1.0                 ── E: 0     | W: 0     | N: 1    
✓ directotree 1.0.0                      ── E: 0     | W: 0     | N: 0    
✓ echarts4r 0.3.2                        ── E: 0     | W: 0     | N: 0    
I finbif 0.3.0                           ── E: 1     | W: 0     | N: 1    (finbif broken, unrelated to data.tree)
✓ forestry 0.1.0                         ── E: 0     | W: 0     | N: 0    
✓ GE 0.1.4                               ── E: 0     | W: 0     | N: 0    
✓ gimme 0.7-1                            ── E: 0     | W: 0     | N: 0    
✓ HCD 0.1                                ── E: 0     | W: 0     | N: 0    
✓ hR 0.2.2                               ── E: 0     | W: 0     | N: 0    
✓ justifier 0.1.0                        ── E: 0     | W: 0     | N: 0    
✓ magclass 5.7.3                         ── E: 0     | W: 0     | N: 0    
✓ mindr 1.2.3                            ── E: 0     | W: 0     | N: 0    
✓ momentuHMM 1.5.1                       ── E: 0     | W: 0     | N: 1    
✓ nonlinearICP 0.1.2.1                   ── E: 0     | W: 0     | N: 0    
✓ prof.tree 0.1.0                        ── E: 0     | W: 0     | N: 0    
✓ radiant.model 1.3.10                   ── E: 1     | W: 0     | N: 0    
✓ rock 0.1.0                             ── E: 0     | W: 0     | N: 0    
✓ Rodam 0.1.6                            ── E: 0     | W: 0     | N: 0    
✓ SACCR 2.3                              ── E: 0     | W: 0     | N: 0    
✓ stoRy 0.1.5                            ── E: 0     | W: 0     | N: 2    
✓ styler 1.3.2                           ── E: 0     | W: 0     | N: 0    
✓ tidygraph 1.2.0                        ── E: 0     | W: 0     | N: 0    
✓ triversity 1.0                         ── E: 0     | W: 0     | N: 0    
✓ UniprotR 1.4.0                         ── E: 0     | W: 0     | N: 0    
✓ userfriendlyscience 0.7.2              ── E: 0     | W: 0     | N: 0    
✓ voronoiTreemap 0.2.0                   ── E: 0     | W: 0     | N: 0    
✓ webchem 1.0.0                          ── E: 0     | W: 0     | N: 1    
✓ wrMisc 1.3.0                           ── E: 0     | W: 0     | N: 0    
✓ yum 0.0.1                              ── E: 0     | W: 0     | N: 0    

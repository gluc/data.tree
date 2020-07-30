## General Comments

Few bug fixes.
Best Regards, Christoph

## Test environments

* linux / travis (release, oldrel, devel) -> ok
* Win / appveyor (devel 32, release 64, stable, patched) -> ok
* Winbuilder (devel) -> OK
* rhub ()

## R CMD check results

There were no ERRORs or WARNINGs.

## Downstream dependencies checked

The following downstream dependencies were checked:

✓ ahp 0.2.12                             ── E: 0     | W: 0     | N: 0    
✓ behaviorchange 0.2.2                   ── E: 0     | W: 0     | N: 1   -> unrelated
✓ Cluster.OBeu 1.2.3                     ── E: 0     | W: 0     | N: 0    
E cola                                   ── E: ?/?   | W: ?/?   | N: ?/?  -> not available for 4.02
✓ collapsibleTree 0.1.7                  ── E: 0     | W: 0     | N: 1   -> unrelated
✓ DecisionAnalysis 1.1.0                 ── E: 0     | W: 0     | N: 1   -> unrelated 
✓ directotree 1.0.0                      ── E: 0     | W: 0     | N: 0    
✓ echarts4r 0.3.2                        ── E: 0     | W: 0     | N: 0    
E finbif                                 ── E: ?/?   | W: ?/?   | N: ?/?  -> cannot build for 4.02 (check: updated on 30. July)
✓ forestry 0.1.0                         ── E: 0     | W: 0     | N: 0    
✓ GE 0.1.4                               ── E: 0     | W: 0     | N: 0    
✓ gimme 0.7-1                            ── E: 0     | W: 0     | N: 0    
✓ HCD 0.1                                ── E: 0     | W: 0     | N: 0    
✓ hR 0.2.2                               ── E: 0     | W: 0     | N: 0    
✓ justifier 0.1.0                        ── E: 0     | W: 0     | N: 0    
✓ magclass 5.7.3                         ── E: 1     | W: 0     | N: 2   -> Problem with units
✓ mindr 1.2.3                            ── E: 0     | W: 0     | N: 0    
✓ momentuHMM 1.5.1                       ── E: 0     | W: 0     | N: 1   -> unrelated 
✓ nonlinearICP 0.1.2.1                   ── E: 0     | W: 0     | N: 0    
✓ prof.tree 0.1.0                        ── E: 0     | W: 0     | N: 0    
✓ radiant.model 1.3.10                   ── E: 1     | W: 0     | N: 0   -> fails on CRAN
✓ rock 0.1.0                             ── E: 0     | W: 0     | N: 0    
✓ Rodam 0.1.6                            ── E: 0     | W: 0     | N: 0    
✓ SACCR 2.3                              ── E: 0     | W: 0     | N: 0    
✓ stoRy 0.1.5                            ── E: 0     | W: 0     | N: 2   -> unrelated 
✓ styler 1.3.2                           ── E: 0     | W: 0     | N: 0    
✓ tidygraph 1.2.0                        ── E: 0     | W: 0     | N: 0    
✓ triversity 1.0                         ── E: 0     | W: 0     | N: 0    
✓ UniprotR 1.4.0                         ── E: 0     | W: 0     | N: 0    
✓ userfriendlyscience 0.7.2              ── E: 0     | W: 0     | N: 0    
✓ voronoiTreemap 0.2.0                   ── E: 0     | W: 0     | N: 0    
✓ webchem 1.0.0                          ── E: 0     | W: 0     | N: 1  -> Problem with usethis   
✓ wrMisc 1.3.0                           ── E: 0     | W: 0     | N: 0    
✓ yum 0.0.1                              ── E: 0     | W: 0     | N: 0    


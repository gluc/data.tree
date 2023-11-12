## General Comments

This release was done as requested by Kurt Hornik, because of a problem with roxygen2. I now used "_PACKAGE" and it should solvel the problem.
Also I added a few features and fixed a few bugs. Finally, I deprecated two functions, as anounced earlier ($fields and $fieldsAll).

Best Regards, Christoph

## Test environments

* github -> (macos-latest release, windows-latest release, ubuntu-latest devel, ubuntu-latest release, ubuntu-latest oldrel-1) -> OK
* rhub::check_for_cran -> NOTES

## R CMD check results

There were no ERRORs or WARNINGs.

I'm getting funny NOTES on rhub::check_for_cran:

```
* checking CRAN incoming feasibility ... [11s] NOTE
Maintainer: 'Christoph Glur <christoph.glur@powerpartners.pro>'

New maintainer:
  Christoph Glur <christoph.glur@powerpartners.pro>
Old maintainer(s):
  Christoph Glur <christoph.glur@ipub.com>
* checking Rd files ... NOTE
checkRd: (-1) s3_register.Rd:46-48: Lost braces
    46 | if (getRversion() >= "3.6.0") {
       |                               ^
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

I couldn't reproduce this anywhere else, and I'm not sure if this is an issue with my code or with the check environment.

Let me know if I was careless and there is anything for me to fix.


## revdepcheck results

We checked 53 reverse dependencies (46 from CRAN + 7 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 3 new problems
 * We failed to check 0 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* collapsibleTree
  checking examples ... WARNING

* directotree
  checking examples ... WARNING

* forestry
  checking examples ... WARNING
  
✔ behaviorchange 0.5.5                   ── E: 0     | W: 0     | N: 0  
✖ collapsibleTree 0.1.7                  ── E: 0     | W: 0  +1 | N: 2   
✔ covid19dbcand 0.1.1                    ── E: 0     | W: 0     | N: 0  
✔ Cluster.OBeu 1.2.3                     ── E: 0     | W: 0     | N: 0   
✔ CondCopulas 0.1.3                      ── E: 0     | W: 0     | N: 0   
✖ directotree 1.0.0                      ── E: 0     | W: 0  +1 | N: 1    
✔ CovRegRF 1.0.4                         ── E: 0     | W: 0     | N: 0   
✔ echarty 1.6.2                          ── E: 0     | W: 0     | N: 0   
✔ changepoints 1.1.0                     ── E: 0     | W: 0     | N: 0   
✔ filterNHP 0.1.2                        ── E: 0     | W: 0     | N: 1    
✖ forestry 0.1.0                         ── E: 0     | W: 0  +1 | N: 0   
✔ echarts4r 0.4.5                        ── E: 0     | W: 0     | N: 0   
✔ GE 0.4.0                               ── E: 0     | W: 0     | N: 0   
✔ gimme 0.7.15                           ── E: 0     | W: 0     | N: 0   
✔ galah 1.5.4                            ── E: 0     | W: 0     | N: 0   
✔ icesTAF 4.2.0                          ── E: 0     | W: 0     | N: 0   
✔ htetree 0.1.17                         ── E: 0     | W: 0     | N: 0  
✔ justifier 0.2.6                        ── E: 0     | W: 0     | N: 0  
✔ LinTInd 1.6.0                          ── E: 1     | W: 0     | N: 2  
✔ cola 2.8.0                             ── E: 1     | W: 0     | N: 1   
✔ nmarank 0.3.0                          ── E: 0     | W: 0     | N: 0   
✔ LACE 2.6.0                             ── E: 0     | W: 0     | N: 1   
✔ momentuHMM 1.5.5                       ── E: 1     | W: 0     | N: 0   
✔ pmxTools 1.3                           ── E: 0     | W: 1     | N: 0   
✔ nonlinearICP 0.1.2.1                   ── E: 0     | W: 0     | N: 0  
✔ radiant.model 1.6.3                    ── E: 0     | W: 0     | N: 0  
✔ Pi 2.14.0                              ── E: 0     | W: 0     | N: 1  
✔ ranktreeEnsemble 0.22                  ── E: 0     | W: 0     | N: 0  
✔ randomForestSRC 3.2.2                  ── E: 0     | W: 0     | N: 0 
✔ RFpredInterval 1.0.7                   ── E: 0     | W: 0     | N: 0 
✔ Rgff 0.1.6                             ── E: 0     | W: 0     | N: 1  
✔ SACCR 3.2                              ── E: 0     | W: 0     | N: 0   
✔ shinyTree 0.3.1                        ── E: 0     | W: 0     | N: 0  
✔ ClassifyR 3.6.2                        ── E: 1     | W: 0     | N: 3  
✔ rock 0.6.7                             ── E: 0     | W: 0     | N: 0  
✔ scicomptools 1.0.0                     ── E: 0     | W: 0     | N: 0  
✔ SoilTaxonomy 0.2.3                     ── E: 0     | W: 0     | N: 0  
✔ rocTree 1.1.1                          ── E: 0     | W: 0     | N: 1   
✔ styler 1.10.2                          ── E: 0     | W: 0     | N: 0   
✔ supportR 1.2.0                         ── E: 0     | W: 0     | N: 0   
✔ tidygraph 1.2.3                        ── E: 0     | W: 0     | N: 0   
✔ starvz 0.7.1                           ── E: 0     | W: 0     | N: 0 
✔ triversity 1.0                         ── E: 0     | W: 0     | N: 0 
✔ TT 0.98                                ── E: 0     | W: 0     | N: 0  
✔ VERSO 1.12.0                           ── E: 0     | W: 0     | N: 0  
✔ voronoiTreemap 0.2.0                   ── E: 0     | W: 0     | N: 0  
✔ TKCat 1.0.7                            ── E: 0     | W: 0     | N: 0  
✔ webchem 1.3.0                          ── E: 0     | W: 0     | N: 0 
✔ yum 0.1.0                              ── E: 0     | W: 0     | N: 0 
✔ wrTopDownFrag 1.0.2                    ── E: 0     | W: 0     | N: 1 
✔ scAnnotatR 1.8.0                       ── E: 0     | W: 0     | N: 0  
✔ wrMisc 1.13.0                          ── E: 0     | W: 0     | N: 0 
✔ UniprotR 2.3.0                         ── E: 0     | W: 0     | N: 0 

### Problems Description

All three problems are a result of the deprecated functions.
I notified the maintainers of the packages.
## General Comments

Added conditional S3 for generics, as requested by Kurt.
Best Regards, Christoph

## Test environments

* linux / travis (release, oldrel, devel) -> ok
* Win / appveyor (devel 32, release 64, stable, patched) -> ok
* Winbuilder (devel) -> OK

## R CMD check results

There were no ERRORs or WARNINGs.

## Downstream dependencies checked

The following downstream dependencies were checked:

Checked ahp             : 0 errors | 0 warnings | 0 notes
Checked Cluster.OBeu    : 0 errors | 0 warnings | 0 notes
Checked collapsibleTree : 0 errors | 0 warnings | 0 notes
Checked DecisionAnalysis: 0 errors | 0 warnings | 0 notes
Checked echarts4r       : 0 errors | 0 warnings | 1 note 
Checked hR              : 0 errors | 0 warnings | 0 notes
Checked nonlinearICP    : 0 errors | 0 warnings | 0 notes
Checked prof.tree       : 0 errors | 0 warnings | 0 notes
Checked qlcData         : 0 errors | 0 warnings | 0 notes
Checked qwraps2         : 0 errors | 0 warnings | 0 notes
Checked radiant.model   : 2 errors | 0 warnings | 0 notes
Checked rENA            : 0 errors | 0 warnings | 1 note 
Checked Rodam           : 0 errors | 0 warnings | 0 notes
Checked SACCR           : 0 errors | 0 warnings | 0 notes
Checked sdcTable        : 0 errors | 0 warnings | 1 note 
Checked stoRy           : 0 errors | 0 warnings | 1 note 
Checked styler          : 0 errors | 0 warnings | 0 notes
Checked tidygraph       : 0 errors | 0 warnings | 1 note 
Checked triversity      : 0 errors | 0 warnings | 0 notes

The problem in radiant.model doesn't seem to be related to data.tree

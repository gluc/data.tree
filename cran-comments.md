## General Comments

## Test environments
* linux / travis (oldrel, release, devel) -> ok
* Win / appveyor (devel 32, devel 64, release 64, stable, patched) -> ok

## R CMD check results
There were no ERRORs or WARNINGs. 

NOTE: On some environments, there was 
the following note: 

´´´
Possibly mis-spelled words in DESCRIPTION:
  JSON (16:37)
  cumulate (14:44)
´´´
  
Both are not misspelled.

## Downstream dependencies checked

* ahp
* prof.tree
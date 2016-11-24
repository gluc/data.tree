## General Comments

Sorry about the problems in the first submission. Both should be fixed now.
Best Regards, Christoph

## Test environments

* linux / travis (release, devel) -> ok
* Win / appveyor (devel 32, devel 64, release 64, stable) -> ok

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

The following downstream dependencies were checked and had no problems:

Checked hypoparsr    : 0 errors | 0 warnings | 0 notes
Checked prof.tree    : 0 errors | 0 warnings | 0 notes
Checked radiant.model: 0 errors | 0 warnings | 0 notes
Checked Rodam        : 0 errors | 0 warnings | 0 notes
Checked SACCR        : 0 errors | 0 warnings | 0 notes

The following downstream dependencies were check and produced warnings:

Checked ahp          : 0 errors | 1 warning  | 0 notes
1 WARN because of a deprecated method. I am the package maintainer and will send a new ahp version shortly.
## General Comments

As requested by Kurz, because of wrong links. Sorry about that :(

## Test environments
* ubuntu R 3.2.3 -> ok
* ubuntu R 3.2.4 -> ok
* win R 3.1.3 -> ok
* win-builder (devel) -> ok

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

## Downstream dependencies

* ahp : I ran CMD CHECK on ubuntu 14.04 with R 3.2.4
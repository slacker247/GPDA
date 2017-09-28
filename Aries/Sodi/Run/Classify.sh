#!/bin/csh
#
cd CLASSIFY
#
#   Classify the training cases and save results just in case
#
autoclass -search `pwd`/$1.db2 `pwd`/$1.hd2 `pwd`/$1.model `pwd`/$1.s-params
autoclass -reports `pwd`/$1.results-bin `pwd`/$1.search `pwd`/$1.r-params
#
#   If 2nd arg present, we want to classify a test case using above results
#
if ($2 != ) then
  autoclass -predict `pwd`/$2.db2 `pwd`/$1.results-bin `pwd`/$1.search `pwd`/$1.r-params
endif

#!/bin/sh

nparallel=11

mkdir -p logs

Rscript SFSW2_project_code.R -nparallel=${nparallel} > logs/$(date +%Y%m%d-%H%M%S)_log-local_rSFSW2.txt 2>&1

#Rscript SFSW2_project_zip3runs.R -path=. -nparallel=${nparallel} -delete > logs/$(date +%Y%m%d-%H%M%S)_log-local_zip3runs.txt 2>&1

#!/bin/bash

echo 0.5

#Runs the R script that compiles the samples
Rscript /30days/s4590609/exp_2_analysis/1.7_compile_samples.R --${1} > /30days/s4590609/exp_2_analysis/1.8_output/1.7_compile_samples_${1}.Rout 2>&1

#!/bin/bash

#Runs the R script that computes the log likelihood of each observation
Rscript /30days/s4590609/exp_2_analysis/2.4_run_loglik.R --${1} > /30days/s4590609/exp_2_analysis/2.5_loglik_output/2.4_log_lik_${1}.Rout
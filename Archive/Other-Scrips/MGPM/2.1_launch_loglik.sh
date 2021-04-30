#!/bin/bash

#navigate to directory
cd /30days/s4590609/exp_2_analysis/

for R in {1..5}
do

#submit job for log likelihood calculation
job1=$(qsub -v RUN=$R /30days/s4590609/exp_2_analysis/2.2_launch_loglik.pbspro)

done
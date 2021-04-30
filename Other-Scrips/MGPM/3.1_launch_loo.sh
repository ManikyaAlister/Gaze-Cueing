#!/bin/bash

#navigate to directory
cd /30days/s4590609/exp_2_analysis/

for R in {1..5}
do

#submit job
job1=$(qsub -v RUN=$R /30days/s4590609/exp_2_analysis/3.2_launch_loo.pbspro)

done
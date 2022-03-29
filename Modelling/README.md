# Individual-level modelling

Each data set will have the following scripts, ordered based on what should be run first:

- 00.#.run-scripts.command: if you have a mac and click on all of these scripts, it runs all of the models through your command terminal so long as the working directories are correct. Essentially, it’s a crude way of parallelising the code as each file will run two scripts, one after the other, so if you open all of the files at once it will run four models at once, and then run the other four models once the first four are finished. 

- 02_megaBackground.R: background code sourced by all of the individual-level modelling scripts, including functions for calculating the likelihood and migration. <-- Don't run, sourced by other scripts

- 03_background.R: more background code, including the priors of each parameter. <-- Don't run, sourced by other scripts

- 04_runIterativeProcess.R: more background code that sets up the iterative process of the parameter estimation. <-- Don't run, sourced by other scripts


- 05.#_runDiffModel_.R: eight seperate scripts, each runs one of the eight individual-level models as described in the manuscript. 

- 06_Weight-Plots.R: calculates the AIC and BIC weighted model probabilities and parameter inclusion probabilities. 

-07_quantiles: scripts for generating the model fits (quantile plots).

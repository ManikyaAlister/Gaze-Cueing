# Hierarchical Modelling

## Folders labeled t0-unconstrained, z-unconstrained, v-unconstrained

These contain the hierarchical modelling analyses shown in the manuscript. Each folder has a subfolder for each data set. Each subfolder has the following scripts: 

- 01_Hier_Megabackground.R: background code sourced by the run model scripts (identical for each model). Contains functions needed for parameter estimation.<-- Don't run, sourced by other scripts  

- 02_Hier_Background.R: more background code, including the priors for the group and individual-level parameters. <-- Don't run, sourced by other scripts  

- 03_Hier_iterative.R: sets up the iterative process fo parameter estimation. <-- Don't run, sourced by other scripts   

- 04_Hier_run_complex.R: runs and defines the specific model.   

- 05_Simulate-Complex.R: simulates the model based on the estimated parameters. Used for the model fits.   

- 06.Hier_quantiles.R: generates the model fits.   

- 09_Hier-param-density.R: calculates the differences in parameter estimates across conditions.

- 07_Output: IMPORTANT FOR RUNNING SCRIPTS 04,05,06,09: for dataset3 of each model, there needs to be a folder called 07_Output, otherwise the models won't run (this is the folder the output is saved into). All of the other datasets should already have this, but we were not able to upload the hierarchical modelling output from dataset3 to GitHub as they were too large, but they can be found on the OSF: https://osf.io/ghket/, so if you just want to see the output, you can find them there. If you are trying to reproduce our analyses from scratch, just make sure there is a folder called "07_Output" in each subfolder where the model scripts are. This applies to dataset3 below as well. 

## Folders labeled dataset1a-dataset3

These are the hierarchical modelling analyses we specified in the preregistration but that we do not use in the manuscript (but are shown in the supplementary materials). They contain the same scripts in them as above, although the models are specified slightly differently.



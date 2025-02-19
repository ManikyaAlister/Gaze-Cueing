# Gaze-Cueing

This repository contains all of the data, scripts, and output* used for analyses in the paper "Uncovering the cognitive mechanisms underlying the gaze-cueing effect", by Alister, McKay, Sewell, and Evans. 

** small update 19th Feb 2025: ** removed some redundant files and removed absolute file paths. I first started this project when I was only a year or so into learning to code so plz don't judge me too harshly. 
Feel free to reach out if you have any questions. 

Some of the output may not be exactly the same as that which is reported in the manuscript as they were recently reproduced and each iteration will produce slightly different results, but the conclusions remain the same. 

*some of the output was too large for GitHub but is available on the OSF. 

## Folder/File key: 

To download all necessary packages, run `renv::restore()` in the R console. 

- `Data`: all of the raw, clean, and derived data pertaining to each of the datasets including the code used to clean the raw data 

- `Descriptives`: scripts for calculating descriptive statistics reported in the manuscript including the gaze cueing magnitudes, and standardised mean change scores.

- `Hierarchical modelling`: scripts for running the hierarchical model and the hierarchical model analyses. 

- `Manuscript figures`: scripts for creating all of the figures in the manuscript and the corresponding figures themselves.

- `Modelling`: scripts for running the individual-level models and the individual-level modelling analyses. 

- `Recovery`: scripts for running the recovery analysis.

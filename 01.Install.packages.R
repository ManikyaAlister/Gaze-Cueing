### Run this script to install all of the packages you need for this code to work ###

# Package names
packages <- c("tidyverse","msm","rtdists","jtools","xtable","metafor","lhs")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

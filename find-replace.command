#! /bin/bash

cd ~
cd ~/cloudstor/Gaze-Cueing/Hierarchical-Modelling/z-unconstrained/dataset1a/
sed -i '' 's:evansetal-18:chenetal-18:g' *.R

cd ~
cd ~/cloudstor/Gaze-Cueing/Hierarchical-Modelling/t0-unconstrained/dataset1a/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R

cd ~
cd ~/cloudstor/Gaze-Cueing/Hierarchical-Modelling/v-unconstrained/dataset1a/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R

cd ~
cd ~/cloudstor/Gaze-Cueing/Hierarchical-Modelling/z-unconstrained/dataset1b/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R

cd ~
cd ~/cloudstor/Gaze-Cueing/Hierarchical-Modelling/t0-unconstrained/dataset1b/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R

cd ~
cd ~/cloudstor/Gaze-Cueing/Hierarchical-Modelling/v-unconstrained/dataset1b/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R

cd ~
cd ~/cloudstor/Gaze-Cueing/Hierarchical-Modelling/z-unconstrained/dataset2/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R

cd ~
cd ~/cloudstor/Gaze-Cueing/Hierarchical-Modelling/t0-unconstrained/dataset2/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R

cd ~
cd ~/cloudstor/Gaze-Cueing/Hierarchical-Modelling/v-unconstrained/dataset2/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R

cd ~
cd ~/cloudstor/Gaze-Cueing/Hierarchical-Modelling/z-unconstrained/dataset3/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R

cd ~
cd ~/cloudstor/Gaze-Cueing/Hierarchical-Modelling/t0-unconstrained/dataset3/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R

cd ~
cd ~/cloudstor/Gaze-Cueing/Hierarchical-Modelling/v-unconstrained/dataset3/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R

cd ~
cd ~/cloudstor/Gaze-Cueing/Hierarchical-Modelling/dataset3/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R

cd ~
cd ~/cloudstor/Gaze-Cueing/Hierarchical-Modelling/dataset1a/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R


cd ~
cd ~/cloudstor/Gaze-Cueing/Hierarchical-Modelling/dataset1b/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R


cd ~
cd ~/cloudstor/Gaze-Cueing/Hierarchical-Modelling/dataset2/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R


cd ~
cd ~/cloudstor/Gaze-Cueing/Modelling/dataset3/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R
sed -i '' 's,library(msm),library(msm, lib.loc = lib),g' *.command

cd ~
cd ~/cloudstor/Gaze-Cueing/Modelling/dataset1a/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R
sed -i '' 's,library(msm),c,g' *.command

cd ~
cd ~/cloudstor/Gaze-Cueing/Modelling/dataset1b/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R
sed -i '' 's,library(msm),library(msm, lib.loc = lib),g' *.command

cd ~
cd ~/cloudstor/Gaze-Cueing/Modelling/dataset2/
sed -i '' 's:nSub = 41:nSub = 50:g' *.R
sed -i '' 's,library(msm),library(msm, lib.loc = lib),g' *.command

cd ~
cd ~/cloudstor/Gaze-Cueing/Manuscript-figures/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R

cd ~
cd ~/cloudstor/Gaze-Cueing/Descriptives/
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R

cd ~
cd ~/cloudstor/Gaze-Cueing/Data/dataset1a/clean
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R

cd ~
cd ~/cloudstor/Gaze-Cueing/Data/dataset1b/clean
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R

cd ~
cd ~/cloudstor/Gaze-Cueing/Data/dataset2/clean
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R

cd ~
cd ~/cloudstor/Gaze-Cueing/Data/dataset3/clean
sed -i '' 's:library(msm):library(msm, lib.loc = lib):g' *.R

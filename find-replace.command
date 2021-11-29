#! /bin/bash


cd ~
cd ~/Documents/2021/Gaze-Cueing/Hierarchical-Modelling/dataset3/
sed -i '' 's/dataset2/dataset3/g' *.R
sed -i '' 's/Dataset2/dataset3/g' *.R
sed -i '' 's/Dataset 2/dataset3/g' *.R
sed -i '' 's/nSub = 50/nSub = 71/g' *.R
cd ~
cd ~/Documents/2021/Gaze-Cueing/Modelling/dataset3/
sed -i '' 's/dataset2/dataset3/g' *.R
sed -i '' 's/Dataset2/dataset3/g' *.R
sed -i '' 's/Dataset 2/dataset3/g' *.R
sed -i '' 's/dataset2/dataset3/g' *.command
sed -i '' 's/S = 50/S = 71/g' *.R
sed -i '' 's/nSub = 50/nSub = 71/g' *.R

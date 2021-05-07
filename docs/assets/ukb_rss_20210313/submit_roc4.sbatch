#!/bin/bash

#SBATCH --time=48:00:00
#SBATCH --partition=mstephens
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=40G
#SBATCH --job-name=roc4

# cd ~/GitHub/mvarbvs/dsc/mnm_prototype

#Rscript result/ukb_rss_20210313/query.R

cd ~/GitHub/mvarbvs/dsc/mnm_prototype/result/ukb_rss_20210313

# Rscript pip_extract.R

Rscript ROC_4.R 


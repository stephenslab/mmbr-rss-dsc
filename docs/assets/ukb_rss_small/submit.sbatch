#!/bin/bash

#SBATCH --time=24:00:00
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=40G

cd ~/GitHub/mvarbvs/dsc/mnm_prototype

Rscript result/ukb_rss_small/query.R

# cd ~/GitHub/mvarbvs/dsc/mnm_prototype/result/ukb_rss_20210107

# Rscript pip_extract.R

# Rscript ROC.R 

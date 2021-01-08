#!/bin/bash

#SBATCH --time=10:00:00
#SBATCH --partition=broadwl
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=30G

cd ~/GitHub/mvarbvs/dsc/mnm_prototype/result/ukb_rss_20210107/

Rscript pip_extract.R 

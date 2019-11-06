#!/bin/bash

#SBATCH --job-name=cvfg1
#SBATCH --workdir=/home/fitzpatr/party/cv_array_job/full_grid_1/
#SBATCH --output=cvfg1_%A_%a.txt
#SBATCH --mem-per-cpu=6000
#SBATCH --time=7:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --array=1-45%25

module load r-3.5.1-gcc-4.8.5-esnjoca

R --file=./party_cv_array_job_full_grid_1.R

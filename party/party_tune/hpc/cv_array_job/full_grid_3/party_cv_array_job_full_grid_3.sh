#!/bin/bash

#SBATCH --job-name=cvfg3
#SBATCH --workdir=/home/fitzpatr/party/cv_array_job/full_grid_3/
#SBATCH --output=cvfg3_%A_%a.txt
#SBATCH --mem-per-cpu=3000
#SBATCH --time=18:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=20
#SBATCH --array=1-45%15

module load r-3.5.1-gcc-4.8.5-esnjoca

R --file=./party_cv_array_job_full_grid_3.R

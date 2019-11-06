#!/bin/bash

#SBATCH --job-name=cfvi
#SBATCH --workdir=/home/fitzpatr/party/variable_importance/
#SBATCH --output=cfvi_%A_%a.txt
#SBATCH --mem-per-cpu=3000
#SBATCH --time=5:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=20
#SBATCH --array=1-50%10

module load r-3.5.1-gcc-4.8.5-esnjoca

R --file=./party_vi_array_job.R

#!/bin/bash

#SBATCH --job-name=ctc25
#SBATCH --workdir=/home/fitzpatr/party/ice/ct_c25
#SBATCH --output=ice_ct_c25_out.txt
#SBATCH --mem-per-cpu=16000
#SBATCH --time=54:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

module load r-3.5.1-gcc-4.8.5-esnjoca

R --file=./ice_ct_c25.R

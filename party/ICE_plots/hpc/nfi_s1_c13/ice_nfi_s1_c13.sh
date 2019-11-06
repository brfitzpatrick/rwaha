#!/bin/bash

#SBATCH --job-name=nfis1c13
#SBATCH --workdir=/home/fitzpatr/party/ice/nfi_s1_c13
#SBATCH --output=ice_nfi_s1_c13_out.txt
#SBATCH --mem-per-cpu=16000
#SBATCH --time=54:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

module load r-3.5.1-gcc-4.8.5-esnjoca

R --file=./ice_nfi_s1_c13.R

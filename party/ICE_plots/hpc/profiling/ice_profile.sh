#!/bin/bash

#SBATCH --job-name=ipt
#SBATCH --workdir=/home/fitzpatr/party/ice/
#SBATCH --output=ice_profile_out.txt
#SBATCH --mem-per-cpu=16000
#SBATCH --time=24:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

module load r-3.5.1-gcc-4.8.5-esnjoca

R --file=./ice_profile.R

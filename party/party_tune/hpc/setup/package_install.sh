#!/bin/bash
#
#SBATCH --job-name=PACKINST
#SBATCH --workdir=/home/fitzpatr/party/setup/
#SBATCH --output=package_install.txt
#SBATCH --mem-per-cpu=2000
#SBATCH --ntasks=8

#SBATCH --time=0:30:00

module load r-3.5.1-gcc-4.8.5-esnjoca

R --file=/home/fitzpatr/party/setup/package_install.R

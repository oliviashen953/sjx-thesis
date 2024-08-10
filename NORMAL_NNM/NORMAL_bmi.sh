#!/bin/bash
#SBATCH -n 1
#SBATCH -N 1
#SBATCH -t 0-24:00:00
#SBATCH -p shared
#SBATCH --mem=40G
#SBATCH -c 2
#SBATCH -J NORMAL_bmi
#SBATCH -o NORMAL_bmi.out
#SBATCH -e NORMAL_bmi.err
#SBATCH --mail-user=oliviashen953@gmail.com

# Load the MATLAB module
module load matlab

# Run the MATLAB script
matlab -nodisplay -r "run('/n/home00/jiaxinshen/NORMAL_bmi.m'); exit;"


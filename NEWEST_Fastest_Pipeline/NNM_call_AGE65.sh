#!/bin/bash
#SBATCH -n 1                     # Number of cores
#SBATCH -N 1                     # Ensure that all cores are on one machine
#SBATCH -t 0-48:00:00            # Runtime in D-HH:MM format (here, 24 hours)
#SBATCH -p shared                # Partition to submit to (shared or any other partition available on your cluster)
#SBATCH --mem=40G                # Memory required per node
#SBATCH -c 2                     # Number of CPUs per task
#SBATCH -J NNM_call_AGE65               # Job name
#SBATCH -o NNM_call_AGE65.out           # Standard output file
#SBATCH -e NNM_call_AGE65.err           # Standard error file
#SBATCH --mail-user=YOUR_EMAIL

# Load the MATLAB module
module load matlab

# Run the MATLAB script
matlab -nodisplay -r "run('/n/home00/jiaxinshen//NNM_call_AGE65.m'); exit;"

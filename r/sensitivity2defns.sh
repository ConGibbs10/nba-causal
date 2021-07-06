#!/bin/bash

#SBATCH --job-name sensitivity2defns
#SBATCH --array=1-16
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --nodes=1
#SBATCH --partition shas
#SBATCH --qos=long
#SBATCH --time=168:00:00
#SBATCH --chdir=/scratch/summit/cgibbs10@colostate.edu/projs/nba-causal-sensitivity/
#SBATCH -o msgs/O%a.output
#SBATCH -e msgs/E%a.error
#SBATCH --mail-type=ALL
#SBATCH --mail-user=connor.gibbs@colostate.edu

module purge
module load anaconda

Rscript r/sensitivity2defns.R index $SLURM_ARRAY_TASK_ID

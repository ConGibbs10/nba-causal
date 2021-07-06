#!/bin/bash

#SBATCH --job-name matching-forest
#SBATCH --array=1-20
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --nodes=1
#SBATCH --partition shas
#SBATCH --time=24:00:00
#SBATCH --chdir=/scratch/summit/cgibbs10@colostate.edu/projs/nba-causal-sensitivity/
#SBATCH -o msgs/matching-forest/O%a.output
#SBATCH -e msgs/matching-forest/E%a.error
#SBATCH --mail-type=ALL
#SBATCH --mail-user=connor.gibbs@colostate.edu

module purge
module load anaconda

Rscript r/matching_forest.R index $SLURM_ARRAY_TASK_ID

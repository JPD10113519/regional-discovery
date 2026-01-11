#!/bin/bash
#SBATCH --array=0-199
#SBATCH --time=04:00:00
#SBATCH --mem=8G
#SBATCH --cpus-per-task=4

Rscript code/exe_batch.R $SLURM_ARRAY_TASK_ID ${JOB_NAME} ${SEED_NAME}


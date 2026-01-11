#!/bin/bash
#SBATCH --time=02:00:00
#SBATCH --mem=32G
#SBATCH --cpus-per-task=1

JOB_NAME=$1
SEED_NAME=$2
MAKE_INTERACTIVES=$3

Rscript code/stitch_statics.R $JOB_NAME 

if [ "${MAKE_INTERACTIVES}" == "TRUE" ]; then
    Rscript code/make_interactive_rois.R $JOB_NAME $SEED_NAME
    Rscript code/make_interactive_features.R $JOB_NAME
fi

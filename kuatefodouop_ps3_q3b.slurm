#!/bin/bash
#SBATCH -J timing-glmnet # name for job array
#SBATCH -o ./dump/q3btask.out #Standard output
#SBATCH -e ./dump/q3btask.err #Standard error
#SBATCH -p general #Partition
#SBATCH -t 02:00:00 #Running time of 2 hours.
#SBATCH --mem-per-cpu 40000 #Memory request
#SBATCH -n 1 #Number of cores
#SBATCH -N 1 #All cores on one machine
#SBATCH --mail-user=kuatefodouop@fas.harvard.edu
#SBATCH --mail-type=ALL    # When to send mail

# argument = job id

Rscript kuatefodouop_ps3_q3b.R $SLURM_ARRAY_TASK_ID

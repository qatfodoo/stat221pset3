#!/bin/bash
#SBATCH -J regr-sgd # name for job array
#SBATCH -o ./dump/q2btask.out #Standard output
#SBATCH -e ./dump/q2btask.err #Standard error
#SBATCH -p general #Partition
#SBATCH -t 06:00:00 #Running time of 6 hours.
#SBATCH --mem-per-cpu 40000 #Memory request
#SBATCH -n 1 #Number of cores
#SBATCH -N 1 #All cores on one machine
#SBATCH --mail-user=kuatefodouop@fas.harvard.edu
#SBATCH --mail-type=ALL    # When to send mail

# argument = job id

Rscript kuatefodouop_ps3_q2b_task.R $SLURM_ARRAY_TASK_ID

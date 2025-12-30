#!/bin/bash
#SBATCH --job-name=L63fti
#SBATCH --output=logs/L63_xdx_%a.out
#SBATCH --error=logs/L63_xdx_%a.err 
#SBATCH --array=1-100             
#SBATCH --time=00:20:00              # Time limit (hh:mm:ss) per job
#SBATCH --mem=30G                     # Memory per job
#SBATCH --ntasks=1                   # Number of tasks
#SBATCH --cpus-per-task=1            # CPU cores per task
#SBATCH --qos=short

# --- PREPARE DIRECTORIES ---
# Ensure the log directory exists so Slurm doesn't fail
mkdir -p logs

# --- RUN COMMAND ---
echo "Starting task $SLURM_ARRAY_TASK_ID on $(hostname)"

julia --project=. --startup-file=no solverError_L63_x.jl --randomSeed $SLURM_ARRAY_TASK_ID --truthName L63_yd --testDuration 50 --nReps 10

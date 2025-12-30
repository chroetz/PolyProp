#!/bin/bash
#SBATCH --job-name=L63_xd_chain
#SBATCH --output=L63_xd_chain_%j.log
#SBATCH --error=L63_xd_chain_%j.err
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=10G
#SBATCH --time=01:00:00
#SBATCH --qos=short

# 1. Get current chunk ID from argument, default to 1
CURRENT_CHUNK=${1:-1}
MAX_CHUNKS=902  # 9012 total time / 10 time per chunk = ~902 chunks

echo "--- SLURM JOB STARTING ---"
echo "Job ID: $SLURM_JOB_ID"
echo "Processing Chunk: $CURRENT_CHUNK of $MAX_CHUNKS"

# 2. Run Julia
# We pass the chunk number as an argument to the julia script
julia --project=. --startup-file=no L63_xd_chain.jl $CURRENT_CHUNK

# Capture the exit status of Julia
JULIA_EXIT_CODE=$?

# 3. Check Logic and Resubmit
if [ $JULIA_EXIT_CODE -eq 0 ]; then
    echo "Julia script finished successfully."
    
    if [ $CURRENT_CHUNK -lt $MAX_CHUNKS ]; then
        NEXT_CHUNK=$((CURRENT_CHUNK + 1))
        echo "Submitting next chunk: $NEXT_CHUNK"
        
        # RESUBMISSION:
        # We submit this same bash script again, passing the next chunk ID
        sbatch submit_L63_xd_chain.sh $NEXT_CHUNK
    else
        echo "All chunks completed!"
    fi
else
    echo "Julia script failed with error code $JULIA_EXIT_CODE. Stopping chain."
    exit 1
fi

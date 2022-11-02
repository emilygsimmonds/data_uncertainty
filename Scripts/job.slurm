#!/bin/sh
#SBATCH --partition=CPUQ
#SBATCH --account=share-nv-bio
#SBATCH --time=30:00:00
#SBATCH --output=job_%A.out
#SBATCH --error=job%A.err
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=6000
#SBATCH --job-name="baseline"
#SBATCH --mail-user=emily.g.simmonds@ntnu.no
#SBATCH --mail-type=ALL

WORKDIR=${SLURM_SUBMIT_DIR}
cd ${WORKDIR}
echo "we are running from this directory: $SLURM_SUBMIT_DIR"
echo " the name of the job is: $SLURM_JOB_NAME"
echo "The job ID is $SLURM_JOB_ID"
echo "The job was run on these nodes: $SLURM_JOB_NODELIST"
echo "Number of nodes: $SLURM_JOB_NUM_NODES"
echo "We are using $SLURM_CPUS_ON_NODE cores"
echo "We are using $SLURM_CPUS_ON_NODE cores per node"
echo "Total of $SLURM_NTASKS cores"

module purge
module load R/4.1.0-foss-2021a

Rscript T1.1_Run_Model_SS_Server.R

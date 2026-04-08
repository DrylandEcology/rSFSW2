#!/bin/sh

#SBATCH --account=swbsc
#SBATCH --partition=cpu
#SBATCH --time=01:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=64
#SBATCH --mail-type=ALL
#SBATCH --mail-user=dschlaepfer@usgs.gov
#SBATCH --job-name=rmz
#SBATCH --output=logs/%j_log-slurm_pipofm-zip.txt


# Add modules
module load cray-R/4.2.1.2
module load gdal/3.8.0 geos/3.12.1 proj/9.3.0 udunits2/2.2.28
module load cray-netcdf-hdf5parallel/4.9.0.5


# Provide run information
echo "slurm setup:"
scontrol show hostname ${SLURM_JOB_NODELIST}
echo "SLURM_NNODES" ${SLURM_NNODES}
echo "SLURM_NTASKS" ${SLURM_NTASKS}
echo "SLURM_CPUS_ON_NODE" ${SLURM_CPUS_ON_NODE}
echo "SLURM_CPUS_PER_TASK" ${SLURM_CPUS_PER_TASK}

echo ""
echo "slurm file:"
cat "$0"
echo ""


# Run R (one R instance `SLURM_NTASKS` which will create many `SLURM_CPUS_ON_NODE` processes within)
srun date
srun Rscript SFSW2_project_zip3runs.R -nparallel=${SLURM_CPUS_ON_NODE} -delete > logs/$(date +%Y%m%d-%H%M%S)_log-hw_zip3runs.txt 2>&1
srun date

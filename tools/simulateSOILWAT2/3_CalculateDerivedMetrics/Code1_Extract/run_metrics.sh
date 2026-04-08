#!/bin/bash

# Run R
mode="-mode=full"
parallel="-ncores=32 -cllog=FALSE"


declare -a prjoptions=(
  "-fparam=PrjParams_for_20241029_PIPOFutureManagement_SOILWAT2_simulations.R"
)


nprjs=${#prjoptions[@]}

for ((k = 0; k < nprjs; k++)); do

  date
  #Rscript Script_to_Extract_Metric.R -o=SW2toTable_daily -fun=metric_SW2toTable_daily ${mode} ${parallel} ${prjoptions[k]}

  Rscript Script_to_Extract_Metric.R -o=EcologicalDroughtMetrics2023_annual -fun=metric_EcologicalDroughtMetrics2023_annual ${mode} ${parallel} ${prjoptions[k]}

  Rscript Script_to_Extract_Metric.R -o=veg_biomass_annual -fun=metric_veg_biomass_annual_v2 ${mode} ${parallel} ${prjoptions[k]}
  Rscript Script_to_Extract_Metric.R -o=LandCover_annualClim -fun=metric_land_cover_v2 ${mode} ${parallel} ${prjoptions[k]}
done

unset -v prjoptions

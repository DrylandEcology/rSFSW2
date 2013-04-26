seq.tr <- (1:trow)[include_YN > 0]	# sequence of row numbers in the master and treatment input files that are included
seq.todo <- 1:(runs * ifelse(trowExperimentals==0, 1, trowExperimentals)) # consecutive number of all (tr x exp) simulations to be executed
runsN.todo <- length(seq.todo)

workersN <- num_cores

ifirst <- seq.todo[1]

i <- 1
runs.completed <- i-1

i_tr <- seq.tr[(1+runs.completed - 1) %% runs + 1]
i_labels <- labels[i_tr]
i_SWRunInformation <- SWRunInformation[i_tr, ]
i_sw_input_soillayers <- sw_input_soillayers[i_tr, ]
i_sw_input_treatments <- sw_input_treatments[i_tr, ]
i_sw_input_cloud <- sw_input_cloud[i_tr, ]
i_sw_input_prod <- sw_input_prod[i_tr, ]
i_sw_input_site <- sw_input_site[i_tr, ]
i_sw_input_soils <- sw_input_soils[i_tr, ]
i_sw_input_weather <- sw_input_weather[i_tr, ]
i_sw_input_climscen <- sw_input_climscen[i_tr, ]
i_sw_input_climscen_values <- sw_input_climscen_values[i_tr, ]

runs.completed <- length(seq.todo)
complete.aggregations <- TRUE

concats.completed <- length(seq.concats)

#' Framework for soilwat simulations: creating simulation runs, executing simulations,
#'  and aggregating outputs
#'
#' SOILWAT is forced by
#'    daily: rainfall (cm), maximum and minimum air temperature at 2-m height (C)
#'    mean monthly: wind speed at 2-m height (miles/h before v24, m/s starting with v24),
#'      relative humidity at 2-m height (%), and cloud cover (%)
#'
#' @docType package
#' @name rSWSF
NULL

##------ Package level variables
swsf_vars <- new.env()


##------ Package uses S3 classes - they are defined in package:methods
#' @importFrom methods isGeneric setGeneric standardGeneric setMethod signature slot as
NULL

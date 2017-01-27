#' Framework for SOILWAT2 simulations: creating simulation runs, executing simulations,
#'  and aggregating outputs
#'
#' SOILWAT2 is forced by \itemize{
#'    \item daily time traces: \itemize{
#'            \item rainfall (cm)
#'            \item minimum air temperature at 2-m height (C)
#'            \item maximum air temperature at 2-m height (C)
#'          }
#'    \item mean monthly values (climate normals): \itemize{
#'            \item wind speed at 2-m height (miles/h before SOILWAT v24;
#'                  m/s starting with SOILWAT v24)
#'            \item relative humidity at 2-m height (%)
#'            \item cloud cover (%)
#'          }
#'    \item mean vegetation
#'    \item soil characterization
#'    \item et cetera
#'  }
#'
#' @docType package
#' @name rSWSF
"_PACKAGE"

##------ Package level variables
swsf_glovars <- new.env()

##------ Import from other packages
## Package uses S3/S4 classes - they are defined in package:methods
#' @importFrom methods isGeneric setGeneric setMethod signature slot slot<- as new
NULL


##------ Support Rcpp
#' @useDynLib rSWSF
#' @importFrom Rcpp sourceCpp evalCpp
NULL

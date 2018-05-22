#' \pkg{rSFSW2}: Framework for \pkg{rSOILWAT2} simulations: creating simulation runs,
#'  executing simulations, and aggregating outputs
#'
#' @references Bradford, J. B., D. R. Schlaepfer, and W. K. Lauenroth. 2014. Ecohydrology
#'  of adjacent sagebrush and lodgepole pine ecosystems: The consequences of climate
#'  change and disturbance. Ecosystems 17:590-605.
#' @references Schlaepfer, D. R., W. K. Lauenroth, and J. B. Bradford. 2012.
#' Ecohydrological niche of sagebrush ecosystems. Ecohydrology 5:453-466.
#'
#' @section FORCING: \pkg{SOILWAT2} is forced by \itemize{
#'    \item daily time traces: \itemize{
#'            \item rainfall (cm)
#'            \item minimum air temperature at 2-m height (C)
#'            \item maximum air temperature at 2-m height (C)
#'          }
#'    \item mean monthly values (climate normals): \itemize{
#'            \item wind speed at 2-m height (miles/h before \pkg{SOILWAT} v24;
#'                  m/s starting with \pkg{SOILWAT} v24)
#'            \item relative humidity at 2-m height (\%)
#'            \item cloud cover (\%)
#'          }
#'    \item mean vegetation
#'    \item soil characterization
#'    \item et cetera
#'  }
#'
#' @section LICENSE:
#'    Copyright (C) \Sexpr{format(Sys.Date(), "\%Y")} by
#'    \Sexpr{packageDescription("rSFSW2")[["Maintainer"]]}
#'
#'    This program is free software: you can redistribute it and/or modify
#'    it under the terms of the GNU General Public License as published by
#'    the Free Software Foundation, version 3 of the License.
#'
#' @section DISCLAIMER:
#'    This program is distributed in the hope that it will be useful,
#'    but WITHOUT ANY WARRANTY; without even the implied warranty of
#'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#'    GNU General Public License for more details.
#'
#' @useDynLib rSFSW2, .registration = TRUE
#' @docType package
#' @name rSFSW2
"_PACKAGE"

##------ Package level variables
SFSW2_glovars <- new.env()

##------ Import from other packages
## Package uses S3/S4 classes - they are defined in package:methods
#' @importFrom methods isGeneric setGeneric setMethod signature slot slot<- as new
NULL


##------ Support Rcpp
#' @importFrom Rcpp sourceCpp evalCpp
NULL

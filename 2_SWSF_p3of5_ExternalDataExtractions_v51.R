#--------------------------------------------------------------------------------------------------#
#------------------------OBTAIN INFORMATION FROM EXTERNAL DATASETS PRIOR TO SIMULATION RUNS TO CREATE THEM

exinfo$GDODCPUCLLNL <-  exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_USA ||
		exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_Global ||
		exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_USA ||
		exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_Global

#------Load required packages
if (exinfo$ExtractSkyDataFromNOAAClimateAtlas_USA) {
	if (!require(rgdal, quietly=TRUE)) {
		tryCatch(install.packages("rgdal", repos=url.Rrepos, lib=dir.libraries), warning=function(w) { print(w); print("rgdal failed to install"); stop("Stopping") })
		stopifnot(require(rgdal, quietly=TRUE))
	}
}

if (	exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_Global ||
		exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_USA ||
		exinfo$ExtractSoilDataFromCONUSSOILFromSTATSGO_USA ||
		exinfo$ExtractElevation_NED_USA ||
		exinfo$ExtractElevation_HWSD_Global) {
	
	if (!require(raster, quietly=TRUE)) {
		tryCatch(install.packages("raster", repos=url.Rrepos, lib=dir.libraries), warning=function(w) { print(w); print("raster failed to install"); stop("Stopping") })
		stopifnot(require(raster, quietly=TRUE))
	}
}

if (exinfo$GDODCPUCLLNL) {
	if (!require(ncdf4, quietly=TRUE)) {
		tryCatch(install.packages("ncdf4", repos=url.Rrepos, lib=dir.libraries), warning=function(w) { print(w); print("ncdf4 failed to install"); stop("Stopping") })
		stopifnot(require(ncdf4, quietly=TRUE))
	}

	if (!require("compiler", quietly=TRUE)) {
		tryCatch(install.packages("compiler", repos=url.Rrepos, lib=dir.libraries), warning=function(w) { print(w); print("'compiler' failed to install"); stop("Stopping") })
		stopifnot(require("compiler", quietly=TRUE))
	}
	
	useRCurl <- FALSE
}

if (exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA) {
	useRCurl <- FALSE
	if (!require(RCurl, quietly=TRUE)) {
		tryCatch(install.packages("RCurl", repos=url.Rrepos, lib=dir.libraries), warning=function(w) { print(w); print("RCurl failed to install, using 'download.files' instead") })
	}
	if (require(RCurl, quietly=TRUE)) useRCurl <- TRUE
}


if (exinfo$use_sim_spatial) {
	# extraction functions
	if (sim_cells_or_points == "point") {
		#' extract raster data for sites
		#'
		#' @param x Points represented by a two-column matrix or data.frame, or SpatialPoints*; SpatialPolygons*; SpatialLines; Extent; or a numeric vector representing cell numbers.
		#' @param data A raster* object from which data is extracted.
		#' @return A vector or matrix with length/rows corresponding to the elements of \code{x} and columns to layers of \code{data}.
		extract_from_external_raster <- compiler::cmpfun(function(x, data, ...) {
			raster::extract(data, x)
		})
		
		#' extract spatial polygon data for sites
		#'
		#' @param x Points represented by an object inheriting from \linkS4class{SpatialPoints}.
		#' @param data A object inheriting from \linkS4class{SpatialPolygons} from which data is extracted.
		#' @param file_path A character string. The directory of the shapefile if \code{data == NULL}.
		#' @param file_shp A character string. The filename of the shapefile if \code{data == NULL}.
		#' @param fields A character vector. If not \code{NULL}, then \code{fields} selects columns of the extracted object.
		#' @param code A vector. If not \code{NULL}, then the extracted data are treated as integer codes of a factor whose levels are encoded by \code{code}.
		#'
		#' @seealso \code{\link[sp]{over}}
		#'
		#' @return A vector or matrix with length/rows corresponding to the elements of \code{x} and available columns requested by \code{fields}. If \code{!is.null(code)}, then the encoded 'factor levels' are returned.
		extract_from_external_shapefile <- compiler::cmpfun(function(x, data = NULL, file_path = NULL, file_shp = NULL, fields = NULL, code = NULL, ...) {
			if (is.null(data)) {
				if (!requireNamespace("rgdal")) stop("'rgdal' is required but not available")
				data <- rgdal::readOGR(dsn = file_path, layer = file_shp, verbose = FALSE)
			}
			
			val <- sp::over(x = x, y = data)
			if (!is.null(fields)) val <- val[, colnames(val) %in% fields, drop = FALSE]
			
			if (!is.null(code)) apply(val, 2, function(x) code[as.integer(x)]) else val
		})
			
	} else if (sim_cells_or_points == "cell") {
		
		add_weights <- compiler::cmpfun(function(i, vals, x, cell_blocks, halfres, exts) {
			if (length(cell_blocks[[i]]) > 0) {
				xy <- raster::xyFromCell(object = x, cell = cell_blocks[[i]])
				xy <- cbind(xy[, 1] - halfres[1], xy[, 1] + halfres[1],
							xy[, 2] - halfres[2], xy[, 2] + halfres[2])
				overlap_dx <- pmin(exts[i, 2], xy[, 2]) - pmax(exts[i, 1], xy[, 1])
				overlap_dy <- pmin(exts[i, 4], xy[, 4]) - pmax(exts[i, 3], xy[, 3])
				w <- overlap_dx * overlap_dy
				cbind(vals[[i]], weight = w / sum(w))
			
			} else {
				cbind(vals[[i]], weight = numeric(0))
			}
		})

		#' Extract values from Raster* objects that are covered by an extent rectangle.
		#'
		#' A cell is covered if its center is inside the polygon (but see the weights option for considering partly covered cells).
		#' 
		#' @paramInherits raster::extract
		#' @param y A matrix with four columns, xmin, xmax, ymin, ymax; each row represents the corners of an \code{\linkS4class{Extent}} object.
		#' @seealso \code{\link[raster]{extract}}
		
		#' @return A list with one item for each extent of \code{y}.
		#' 	Each element is a matrix where each row corresponds to one of the cells of \code{x} contained in a SpatialPolygon
		#' 	and where columns correspond to layers of \code{x}.
		#'	If \code{weights} is \code{TRUE}, then an additional last column is added which contains the weights of the rows.
		extract_blocks <- compiler::cmpfun(function(x, y, weights = FALSE) {
			fun_match <- if (requireNamespace("fastmatch")) fastmatch::fmatch else match
			stopifnot(ncol(y) == 4L)
			
			grid_res <- raster::res(x)
			iseq <- seq_len(nrow(y))
			
			cell_blocks <- lapply(iseq, function(i) {
					ext <- raster::extent(y[i, 1], y[i, 2], y[i, 3], y[i, 4])
					if (weights) ext <- ext + grid_res
					raster::cellsFromExtent(object = x, extent = ext, expand = FALSE)})
		
			unique_cells <- sort(unique(unlist(cell_blocks)))
			vtemp <- raster::extract(x, y = unique_cells)

			vals <- if (raster::nlayers(x) == 1) {
						lapply(cell_blocks, function(block) vtemp[fun_match(block, unique_cells, nomatch = NA)])
					} else {
						lapply(cell_blocks, function(block) vtemp[fun_match(block, unique_cells, nomatch = NA), ])
					}	
			
			if (weights) {
				halfres <- grid_res / 2
				vals <- lapply(iseq, add_weights, vals = vals, x = x, cell_blocks, halfres = halfres, exts = y)
			}
			
			vals
		})
		
		if (!exists("extract2_Raster_SpatialPolygons")) {
			extract2_Raster_SpatialPolygons <- function(x, ...) {
				stop("Function 'extract2_Raster_SpatialPolygons' is not defined")
			}
		}

		#' Extract all raster cell values that occur within each rectangle
		#'
		#' This is similar to \code{\link[raster]{aggregate}} but with more control and information.
		#'
		#' Available extraction \code{method}s include
		#'  \describe{
		#' 		\item{raster}{Uses the function \code{\link[raster]{extract}}}
		#' 		\item{raster_con}{Uses the function \code{extract2_Raster_SpatialPolygons}. This is a modified version of \code{\link[raster]{extract}} where the 'connection' to the raster file is stable for the entire extraction call and not re-opened/closed for each read event. This allows a massive speed-up which scales with the number of 'rectangles' to be extracted.}
		#' 		\item{block}{Uses the function \code{extract_blocks}.
		#' 	}
		#' The weighted mean of the extracted values can be calculated as weighted.mean(values, w = weights)
		#' 
		#' @param x A raster* object from which data is extracted.
		#' @param coord A numeric vector of length two or a matrix with two columns. The x and y coordinates of the center(s) of the rectangle(s).
		#' @param to_res A numeric vector of length two. The x- and y-extent of the rectangle(s).
		#' @param with_weigths A logical value or \code{NULL}. If \code{NULL}, then code attempts to determine whether weights are required for the call to \code{\link[raster]{extract}}.
		#' @param method A character string. Selects the extraction method, see details.
		#' @param tol A numeric value. The absolute tolerance for deviation used if \code{is.null(with_weights)}.
		#'
		#' @return A list of length corresponding to the number of rectangles. Each element is a list which contains three items each
		#'	\describe{
		#'		\item{N}{An integer vector. The number of unique values within the rectangle for each layer of \code{x}.}
		#'		\item{values}{A list of numeric vectors. The sorted unique values as vector for each layer.}
		#'		\item{weigths}{A list of numeric vectors. The weights of the \code{values} for each layer.}
		#'	}
		reaggregate_raster <- compiler::cmpfun(function(x, coords, to_res = c(0, 0), with_weights = NULL, method = c("raster", "raster_con", "block"), tol = 1e-2) {
			if (is.null(dim(coords)) && length(coords) == 2L) {
				coords <- matrix(coords, ncol = 2)
			}
			
			if (is.vector(to_res) && length(to_res) == 2L && to_res > 0) {
				to_res <- matrix(to_res, ncol = 2)
			}
			stopifnot(is.matrix(to_res), nrow(to_res) == 1L || nrow(to_res) == nrow(coords))
			
			fun_extract <- switch(EXPR = method,
									raster = raster::extract,
									raster_con = extract2_Raster_SpatialPolygons,
									block = extract_blocks)
			
			to_halfres <- to_res / 2
			cxy <- cbind(coords[, 1] - to_halfres[, 1], coords[, 1] + to_halfres[, 1],
						 coords[, 2] - to_halfres[, 2], coords[, 2] + to_halfres[, 2])

			if (method %in% c("raster", "raster_con")) {
				# Create SpatialPolygons with #features == # rectangles
				ptemp0 <- lapply(seq_len(nrow(coords)),
									function(i) matrix(c(cxy[i, 1], cxy[i, 3], cxy[i, 1], cxy[i, 4], cxy[i, 2], cxy[i, 4], cxy[i, 2], cxy[i, 3]),
												ncol = 2, byrow = TRUE))
				ptemp1 <- lapply(ptemp0, sp::Polygon)
				ptemp2 <- lapply(seq_along(ptemp1), function(i) sp::Polygons(ptemp1[i], ID = i))
				ys <- sp::SpatialPolygons(ptemp2, proj4string = raster::crs(x))
			} else if (method == "block") {
				ys <- cxy
			}
			
			if (is.null(with_weights)) {
				# determine if we need the 'weights' argument to extract (c. 2-times slower)
				if (nrow(to_res) > 1) {
					with_weights <- TRUE
				} else {
					#	- to_res is a (whole number) multiple of res(grid)
					fact <- to_res[1, ] / raster::res(x)
					#	- corners of cell_poly align with the origin of grid
					orig <- (c(cxy[1, 1], cxy[1, 3]) - raster::origin(x)) / to_res[1, ]
					with_weights <- !all(sapply(c(fact, orig), function(f) isTRUE(all.equal(round(f), f, tolerance = tol, scale = 1))))
				}
			}
			
			# extract
			nl <- raster::nlayers(x)
			
			# @param sval A list with one item for each 'ys'. Each element is a matrix where each row corresponds to one of the cells contained in the 'rectangle' and where columns correspond to layers in 'x' plus the last column which contain the weights of the rows.
			if (with_weights) {
				sval <- fun_extract(x = x, y = ys, weights = TRUE)
			} else {
				temp <- fun_extract(x = x, y = ys, weights = FALSE)
				sval <- lapply(temp, function(v) if (length(v) > 0) cbind(v, nl / length(v)) else NA)
			}
			
			# Return object
			lapply(sval, function(v)
				if (length(v) > 0) {
					w <- v[, ncol(v)]
					f <- lapply(seq_len(nl), function(i) as.vector(tapply(w, v[, i], sum)))
					vals <- lapply(seq_len(nl), function(i) sort(unique(v[, i])))
					list(N = lengths(f), values = vals, fraction = f)
				} else {
					list(N = -1, values = NULL, fraction = NULL)
				})
		})


		
		#' The 'weighted mean' (and sample quantiles) of re-aggregation output
		#'
		#' @param reagg A list. The output object of a call to \code{reaggregate_raster} or to \code{reaggregate_shapefile}.
		#' @param probs A numeric vector of probabilities with values in \code{[0,1]} at which sample quantiles are returned or \code{NA}.
		#'
		#' @return An array. The first dimension corresponds to each rectangle, i.e., a row of \code{coords};
		#' the second dimension corresponds to the layers of the re-aggregated and -sampled Raster* object \code{x};
		#' And the third dimension corresponds to the aggregation type(s) (weighted mean, and sample quantiles if any).
		weighted.agg <- compiler::cmpfun(function(reagg, probs = NA) {
			stopifnot(requireNamespace("Hmisc"))
			
			nf <- 1 + if (anyNA(probs)) 0 else length(probs)
			nl <- max(1, sapply(reagg, function(x) length(x[["N"]])))

			res <- array(NA_real_,
				dim = c(length(reagg), nl, nf),
				dimnames = list(NULL, paste0("Layer", seq_len(nl)), c("wmean", if (!anyNA(probs)) paste0("q", probs))))
			FUN.VALUE <- rep(NA_real_, nf)
			
			for (k in seq_along(reagg)) {
				res[k, , ] <- t(vapply(seq_along(reagg[[k]][["N"]]), function(i) {
						if (reagg[[k]][["N"]][i] > 0 && reagg[[k]][["fraction"]][[i]] > 0) {
							out <- weighted.mean(reagg[[k]][["values"]][[i]],
								w = reagg[[k]][["fraction"]][[i]])
							if (!anyNA(probs)) {
								out <- c(wmean = out,
									Hmisc::wtd.quantile(reagg[[k]][["values"]][[i]],
										weights = reagg[[k]][["fraction"]][[i]],
										probs = probs,
										normwt = TRUE))
							}
							out
						} else FUN.VALUE
					}, FUN.VALUE))
			}
			
			res
		})


		#' function to extract data for raster cells
		#'
		#' @description This function is slow because of the call to \code{\link[raster]{resample}}.
		#' The result is also too smooth because of 'two' smoothing steps: (i) aggregation and (ii) 'bilinear' resampling method.
		#' 
		#' @param x A RasterLayer object for which !NA cells, values of 'data' are resampled and extracted
		#' @param data A raster* object from which data is extracted
		#' @param ...
		#'	\describe{
		#'		\item{method}{A character string. The method used to resample values for the new RasterLayer, should be "bilinear" for bilinear interpolation, or "ngb" for using the nearest neighbor.}
		#'		\item{coords}{Points represented by a two-column matrix or data.frame, or SpatialPoints*; SpatialPolygons*; SpatialLines; Extent; or a numeric vector representing cell numbers.}
		#'		\item{crit_v_exclude}{A character string representing a logical expression based on a variable named 'v'. If present, then the condition(s) are applied to 'data' before resampling.}
		#'	}
		#' @seealso \code{\link[raster]{extract}}
		#' @return A vector or matrix with length/rows corresponding to the !NA cells of \code{x} and columns to layers of \code{data}.
		extract_from_external_raster_old <- compiler::cmpfun(function(x, data, ...) {
			dots <- list(...)	# coords, method
			if (!("method" %in% names(dots))) dots[["method"]] <- "bilinear"
			if ("crit_v_exclude" %in% names(dots)) {
				vexpr <- parse(text = dots[["crit_v_exclude"]])
				fun_crit <- compiler::cmpfun(function(v) {ifelse(is.na(v) | eval(vexpr), NA, v)})
				data <- raster::calc(data, fun = fun_crit)
			}
		
			data2 <- raster::resample(x = data, y = x, method = dots[["method"]])
			raster::extract(x = data2, y = dots[["coords"]])	# extract by coords == run_sites[do_extract, ] to get the correct order
		})
		

		#' Extract the weighted mean (and sample quantiles) for raster cells or rectangles.
		#'
		#' @param x Either A RasterLayer OR raster resolution (of rectangles) as a numeric vector of length two or a matrix with two columns.
		#'		If a RasterLayer, then values of \code{data} are resampled and extracted for !NA cells.
		#'		If the latter, then the vector or matrix represents the rectangle extent/resolution in x- and y-coordinates.
		#'		If a matrix, then rows must match \code{coords}.
		#' @param data A raster* object from which data is extracted
		#' @param ...
		#'	\describe{
		#'		\item{method}{A character string. The method argument passed to \code{reaggregate_raster}. Default is 'block' which is the fastest.}
		#'		\item{coords}{Cell centers (corresponding to !NA cells of \code{x}) that are represented by a two-column matrix of xy coordinates. If not provided, then extracted from \code{x}.}
		#'		\item{probs}{A numeric vector of probabilities with values in \code{[0,1]} at which sample quantiles are returned.}
		#'	}
		#' @seealso \code{\link[raster]{extract}}
		#' @return A matrix with rows corresponding to the !NA cells of \code{x} and columns to layers of \code{data}.
		extract_from_external_raster <- compiler::cmpfun(function(x, data, ...) {
			dots <- list(...)

			if (!("method" %in% names(dots)))
				dots[["method"]] <- "block"
			
			if (!("coords" %in% names(dots))) {
				dots[["coords"]] <- raster::xyFromCell(x, cell = seq_len(raster::ncell(x)))
			} else {
				dots[["coords"]] <- sp::coordinates(dots[["coords"]])
			}
			if (nrow(dots[["coords"]]) == 0)
				return(matrix(NA, ncol = raster::nlayers(data)))
				
			if (!("probs" %in% names(dots)))
				dots[["probs"]] <- NA
			
			if (inherits(x, "Raster")) {
				to_res <- raster::res(x)
			} else {
				if (all(is.vector(x), length(x) == 2L, x > 0) ||	# x is x- and y-resolution
					all(is.matrix(x), ncol(x) == 2L, nrow(x) == nrow(dots[["coords"]]), x > 0)) {	# x are x- and y-resolution for each coord 
					to_res <- x
				} else {
					return(matrix(NA, ncol = raster::nlayers(data)))
				}
			}
			
			reagg <- reaggregate_raster(x = data,
				coords = dots[["coords"]],
				to_res = to_res,
				with_weights = TRUE,
				method = dots[["method"]],
				tol = 1e-2)
			
			weighted.agg(reagg, probs = dots[["probs"]])
		})

		#' Re-aggregation of spatial polygon data by spatial rectangles/polygons
		#'
		#' Code based on sp:::aggregatePolyWeighted version 1.2.3 and modified to return complete information and not the area-weigthed sum.
		#'
		#' @param x A \linkS4class{SpatialPolygons} object from which data is extracted.
		#' @param by A \linkS4class{SpatialPolygons} object. The 'extents' representing the rectangle(s) for which data is re-aggregated.
		#' @param fields A character vector. If not \code{NULL}, then \code{fields} selects columns of the extracted object.
		#' @param code A vector. If not \code{NULL}, then the extracted data are treated as integer codes of a factor whose levels are encoded by \code{code}.
		#'
		#' @return A list of length corresponding to the number of rectangles. Each element is a list which contains three items each
		#'	\describe{
		#'		\item{N}{An integer vector. The number of unique values within the rectangle for each layer of \code{x}.}
		#'		\item{values}{A list of numeric vectors or matrices. The sorted unique values as vector or matrix for each layer.}
		#'		\item{weigths}{A list of numeric vectors. The weights of the \code{values} for each layer.}
		#'	}
		reaggregate_shapefile <- compiler::cmpfun(function(x, by, fields = NULL, code = NULL) {
			# Code from sp:::aggregatePolyWeighted version 1.2.3
			if (!requireNamespace("rgeos", quietly = TRUE)) stop("rgeos required")

			i <- rgeos::gIntersection(x, by, byid = TRUE, drop_lower_td = TRUE)
			
			# Modified code
			if (is.null(i)) return(rep(list(list(N = -1, values = NULL, fraction = NULL)), length(by)))
			
			rect_subs <- t(sapply(i@polygons, function(p) {
							IDs <- as.integer(strsplit(slot(p, name = "ID"), " ")[[1]])
							if (!(length(IDs) == 2)) stop("IDs contain spaces: this breaks identification after gIntersection()")

							c(	area = slot(p, name = "area"),
								ID_data = which(row.names(x) == IDs[1]),
								ID_rect = which(row.names(by) == IDs[2]))
						}))
			subs_agg <- aggregate(rect_subs[, "area"],
									by = list(rect_subs[, "ID_data"], rect_subs[, "ID_rect"]),
									sum)

			# Return object
			lapply(seq_along(by), function(k) {
					v <- subs_agg[subs_agg[, "Group.2"] == k, , drop = FALSE]
					
					if (nrow(v) > 0) {
						vals <- v[, "Group.1"]
						vals <- if (is.null(dim(x@data))) x@data[vals] else x@data[vals, ]
						if (!is.null(fields)) vals <- vals[, colnames(vals) %in% fields, drop = FALSE]
						if (!is.null(code)) vals <- apply(vals, 2, function(x) code[as.integer(x)])
						
						list(N = list(nrow(v)), values = list(vals), fraction = list(v[, "x"]))
					} else {
						list(N = -1, values = NULL, fraction = NULL)
					}
				})
    	})


		#' Extract spatial polygon data for polygons or rectangles.
		#'
		#' @param x Points represented by an object inheriting from \linkS4class{SpatialPolygons}.
		#' @param x Either an object inheriting from \linkS4class{SpatialPolygons} OR resolution(s) of rectangles as a numeric vector of length two or a matrix with two columns.
		#'		If \linkS4class{SpatialPolygons}, then values of \code{data} are extracted per polygon and weighted by area.
		#'		If the latter, then \code{coords} must be provided. \code{x} is the vector or matrix representing the rectangle extents in x- and y-coordinates.
		#'		If a matrix, then rows must match \code{coords}.
		#' @param data A object inheriting from \linkS4class{SpatialPolygons} from which data is extracted.
		#' @param file_path A character string. The directory of the shapefile if \code{data == NULL}.
		#' @param file_shp A character string. The filename of the shapefile if \code{data == NULL}.
		#' @param fields A character vector. If not \code{NULL}, then \code{fields} selects columns of the extracted object.
		#' @param code A vector. If not \code{NULL}, then the extracted data are treated as integer codes of a factor whose levels are encoded by \code{code}.
		#' @param ...
		#'	\describe{
		#'		\item{coords}{Cell centers (corresponding to each resolution of \code{x}) that are represented by a two-column matrix of xy coordinates. Ignored if x is inheriting from \linkS4class{SpatialPolygons}.}
		#'		\item{crs_data}{A \linkS4class{CRS} object indicating the coordinate reference system (CRS) of x and coords. Ignored if x is inheriting from \linkS4class{SpatialPolygons}.}
		#'		\item{probs}{A numeric vector of probabilities with values in \code{[0,1]} at which sample quantiles are returned.}
		#'	}
		#'
		#' @seealso \code{\link[sp]{over}}
		#'
		#' @return A vector or matrix with length/rows corresponding to the elements of \code{x} and available columns requested by \code{fields}. If \code{!is.null(code)}, then the encoded 'factor levels' are returned.
		extract_from_external_shapefile <- compiler::cmpfun(function(x, data = NULL, file_path = NULL, file_shp = NULL, fields = NULL, code = NULL, ...) {
			if (!requireNamespace("rgeos")) stop("'rgeos' is required but not available")
			
			if (is.null(data)) {
				if (!requireNamespace("rgdal")) stop("'rgdal' is required but not available")
				data <- rgdal::readOGR(dsn = file_path, layer = file_shp, verbose = FALSE)
			}
			
			dots <- list(...)
			if (!("probs" %in% names(dots)))
				dots[["probs"]] <- NA
			
			if (!inherits(x, "SpatialPolygons")) {
				if (!("coords" %in% names(dots))) stop("Since 'x' are not 'SpatialPolygons', 'coords' are required")
				coords <- sp::coordinates(dots[["coords"]])
			
				# Convert resolution/rectangles into SpatialPolygons
				if (is.vector(x) && length(x) == 2L && x > 0) {
					x <- matrix(x, ncol = 2)
				}
				stopifnot(is.matrix(x), nrow(x) == 1L || nrow(x) == nrow(coords))
	
				to_halfres <- x / 2
				cxy <- cbind(coords[, 1] - to_halfres[, 1], coords[, 1] + to_halfres[, 1],
							 coords[, 2] - to_halfres[, 2], coords[, 2] + to_halfres[, 2])

				ptemp0 <- lapply(seq_len(nrow(coords)),
									function(i) matrix(c(cxy[i, 1], cxy[i, 3], cxy[i, 1], cxy[i, 4], cxy[i, 2], cxy[i, 4], cxy[i, 2], cxy[i, 3]),
												ncol = 2, byrow = TRUE))
				ptemp1 <- lapply(ptemp0, sp::Polygon)
				ptemp2 <- lapply(seq_along(ptemp1), function(i) sp::Polygons(ptemp1[i], ID = i))
				x <- sp::SpatialPolygons(ptemp2, proj4string = dots[["crs_data"]])
			}
			
			if (!raster::compareCRS(raster::crs(x), raster::crs(data))) {
				x <- sp::spTransform(x, CRS = raster::crs(data))
			}
			
			reagg <- reaggregate_shapefile(x = data, by = x, fields = fields, code = code)
			
			weighted.agg(reagg, probs = dots[["probs"]])
		})
		
		#' Extracts the 'units' argument from a CRS object
		#'
		#' @param CRS A Raster*, Spatial*, CRS, or character object with a coordinate reference system (CRS).
		#' @return A character string or \code{NA}.
		crs_units <- compiler::cmpfun(function(CRS) {
			args_crs <- raster::crs(CRS, asText = TRUE)
			stopifnot(inherits(args_crs, "character") && rgdal::checkCRSArgs(args_crs)[[1]])
			
			args2 <- strsplit(args_crs, split = "+", fixed = TRUE)[[1]]
			units <- trimws(args2[grep("units", args2)])
			if (length(units) > 0) {
				strsplit(units, split = "=", fixed = TRUE)[[1]][2]
			} else NA
		})
		
		#' Aligns 'grid_from' with 'grid_to' for certain cells
		#'
		#' @param grid_from A RasterLayer object.
		#' @param coords A matrix of x and y coordinates, or a SpatialPoints or SpatialPointsDataFrame object indicating which cells of projected 'grid_from' will be used.
		#' @param grid_to A RasterLayer object.
		#' @param crs_to A CRS object or \code{NULL} in which case it will be extracted from \code{grid_to}.
		#'
		#' @return A list with two elements
		#'	\describe{
		#'		\item{x}{A RasterLayer object. Cells values are \code{NA} or 1 if they contain points of \code{coords}.}
		#'		\item{index}{An integer vector. The cell numbers of \code{x} that correspond to \code{coords}.}
		#' }
		align_with_target_grid <- compiler::cmpfun(function(grid_from, coords, grid_to, crs_to = NULL) {
			if (is.null(crs_to)) crs_to <- raster::crs(grid_to)
			
			# Align with data crs
			if (raster::compareCRS(crs_to, raster::crs(grid_from))) {
				x <- grid_from
			} else {
				to_ext <- raster::projectExtent(grid_from, grid_to)
				if (identical(crs_units(to_ext), crs_units(grid_from))) {
					raster::res(to_ext) <- raster::res(grid_from)
				}
				raster::origin(to_ext) <- raster::origin(grid_to)
				x <- raster::projectRaster(grid_from, to = to_ext, method = "ngb")
			}
			
			# locations of simulation runs
			x <- raster::init(x, fun = function(i) rep(NA, i))
			imatch <- raster::cellFromXY(x, coords)
			x[imatch] <- 1
	
			list(x = x, index = imatch)
		})


		#' Calculate resolution of one coordinate system for points in their coordinate system transformed to a target coordinate system 
		#'
		#' @param res_from A numeric vector of length two. The resolution in x and y direction in the coordinate system \code{crs_from}.
		#' @param crs_from A CRS object. The coordinate system of \code{res_from}.
		#' @param sp A SpatialPoints object. Cell center points for which new resolutions will be calculated.
		#' @param crs_sp A CRS object. The coordinate system of \code{sp}.
		#' @param crs_to A CRS object. The coordinate system in which the resulting resolution will be calculated.
		#'
		#' @return A numeric vector of length two (if resolution is constant for each point) or a matrix with two columns for the x- and y-resolutions per row for each point.
		align_with_target_res <- compiler::cmpfun(function(res_from, crs_from, sp, crs_sp, crs_to) {
			if (identical(crs_units(crs_from), crs_units(crs_to))) {
				res_from
			} else {
				sp_from <- if (raster::compareCRS(crs_sp, crs_from)) {
								sp
							} else {
								# transform points to same coordinate system as resolution
								sp::spTransform(sp, CRS = crs_from)
							}
				
				# resolution is constant for every point in crs_from
				coords_from <- sp::coordinates(sp_from)
				from_halfres <- res_from / 2
				
				# cell corners of each point
				#	- lower left cell corner
				cxy0_from <- cbind(coords_from[, 1] - from_halfres[1], coords_from[, 2] - from_halfres[2])
				cxy0_from_sp <- sp::SpatialPoints(coords = cxy0_from, proj4string = crs_from)
				#	- upper right cell corner
				cxy1_from <- cbind(coords_from[, 1] + from_halfres[1], coords_from[, 2] + from_halfres[2])
				cxy1_from_sp <- sp::SpatialPoints(coords = cxy1_from, proj4string = crs_from)
				
				# transform cell corners to target coordinate system
				cxy0_to_sp <- sp::spTransform(cxy0_from_sp, CRS = crs_to)
				cxy1_to_sp <- sp::spTransform(cxy1_from_sp, CRS = crs_to)
				
				# resolution varies with latitude of points in crs_to
				cxy0_to <- sp::coordinates(cxy0_to_sp)
				cxy1_to <- sp::coordinates(cxy1_to_sp)
				
				abs(cxy1_to - cxy0_to)
			}
		})

	}	

}

#--------------------------------------------------------------------------------------------------#
#------Load additional parameters and functions for data from the Lawrence Livermore National Lab and from USGS NEX

if (exinfo$GDODCPUCLLNL || exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA) {
	stopifnot(getCurrentWeatherDataFromDatabase, getScenarioWeatherDataFromDatabase)
	
	dbW_setConnection(dbFilePath=dbWeatherDataFile)
	dbW_iSiteTable <- dbW_getSiteTable()
	dbW_iScenarioTable <- dbW_getScenariosTable()
	dbW_compression_type <- Rsoilwat31:::dbW_compression()
	
	climScen <- data.frame(matrix(unlist(strsplit(temp <- climate.conditions[!grepl(climate.ambient, climate.conditions)], split=".", fixed=TRUE)), ncol=4, byrow=TRUE), stringsAsFactors=FALSE)
	climScen$imap_todbW <- match(temp, table=dbW_iScenarioTable$Scenario, nomatch=0)
	dbW_iScenarioTable[, "Scenario"] <- tolower(dbW_iScenarioTable[, "Scenario"])
	reqGCMs <- unique(climScen[, 4])
	reqRCPs <- unique(climScen[, 3])
	reqRCPsPerGCM <- lapply(reqGCMs, FUN=function(x) unique(climScen[x == climScen[, 4], 3]))
	reqDownscalingsPerGCM <- lapply(reqGCMs, FUN=function(x) unique(climScen[x == climScen[, 4], 1]))
	
	for (i in seq_along(reqGCMs)) {
		dir.create2(file.path(dir.out.temp, reqGCMs[i]), showWarnings=FALSE, recursive=TRUE)
	}


	#---Downscaling/bias-correction functions	
	#Helper functions
	in_GMC_box <- compiler::cmpfun(function(xy, lats, longs, i_use) {
		!i_use & (xy[, 1] >= longs[1] & xy[, 1] <= longs[2]) & (xy[, 2] >= lats[1] & xy[, 2] <= lats[2])
	})

	unique_times <- compiler::cmpfun(function(timeSlices, slice) {
		starts <- na.exclude(timeSlices$Year[timeSlices$Slice == slice & timeSlices$Time == "start"])
		ends <- na.exclude(timeSlices$Year[timeSlices$Slice == slice & timeSlices$Time == "end"])
		temp <- lapply(seq_along(starts), function(x) starts[x]:ends[x])
		temp1 <- vector("integer", length = 0)
		for (it in seq_along(temp)) temp1 <- union(temp1, temp[[it]])
		n <- 1 + length(temp2 <- which(diff(temp1) > 1))
		temp2 <- c(1, 1 + temp2, length(temp1) + 1)
		res <- matrix(NA, nrow = n, ncol = 2)
		for (it in 1:n) res[it, ] <- c(temp1[temp2[it]], temp1[temp2[it+1] - 1])
	
		res
	})

	useSlices <- compiler::cmpfun(function(getYears, timeSlices, run, slice) {
		res <- rep(FALSE, length = nrow(getYears[[slice]]))
		temp <- timeSlices$Year[timeSlices$Run == run & timeSlices$Slice == slice]
		if (!anyNA(temp)) {
			istart <- findInterval(temp[1], getYears[[slice]][, 1], rightmost.closed = FALSE, all.inside = FALSE)
			iend <- findInterval(temp[2], getYears[[slice]][, 2], rightmost.closed = FALSE, all.inside = FALSE)
			res[istart:iend] <- TRUE
		}
		
		res
	})


	#' Error function
	#' @references See among examples of ?Normal
	erf <- compiler::cmpfun(function(x) 2 * pnorm(x * sqrt(2)) - 1)
	
	#' Stretch the values
	#'
	#' Values above the mean of \code{x} are made larger and 
	#' values below the mean are made smaller - each by \code{lambda * dist(x, mean(x))}.
	stretch_values <- compiler::cmpfun(function(x, lambda = 0) {
		(1 + lambda) * x - lambda * mean(x)
	})

	
	#' Additive Precipitation adjustment by a delta value
	#' 
	#' Additive Precipitation adjustment by a delta value
	#'
	#' Provide one of the arguments \code{addDelta} or \code{deltaPerEvent}, but not both.
	#' The value of \code{addDelta} will be evenly split up among \code{ind_events}.
	#'
	#' @param data A numeric vector. Daily values of precipitation.
	#' @param ind_events A logical or integer vector of the same length as \code{data} or \code{NULL}. If logical, then TRUE/FALSE for each element/day of \code{data} whether it precipitates or not. If integer, then the indices of \code{data} on which it precipitates. If \code{NULL}, then it will be calculated as \code{data > 0}.
	#' @param addDelta A numeric value or \code{NULL}. The total amount of precipitation that is to be added/removed from \code{data[ind_events]} together (divided among events), i.e., it requires the same unit as \code{data}.
	#' @param deltaPerEvent A numeric vector of the length equal to \code{sum(ind_events)} or \code{NULL}. The daily amount of precipitation that is to be added/removed from each \code{data[ind_events]}, i.e., it requires the same unit as \code{data}.
	#'
	#' @return A list with two elements
	#'	\describe{
	#'		\item{data}{A copy of \code{data} with adjusted values.}
	#'		\item{PPT_to_remove}{The total amount of precipitation that couldn't be removed from \code{data} due to lack of precipitation.}
	#'	}
	add_delta_to_PPT <- compiler::cmpfun(function(data, ind_events = NULL, addDelta = NULL, deltaPerEvent = NULL) {
		stopifnot(xor(is.null(deltaPerEvent), is.null(addDelta)))
		
		if (is.null(ind_events)) ind_events <- data > 0
		if (!is.null(addDelta)) {
			elems_N <- if (is.logical(ind_events)) sum(ind_events) else length(ind_events)
			deltaPerEvent <- rep(addDelta[1] / elems_N, elems_N)
		}
		StillToSubtract <- 0
		
		if (all(deltaPerEvent > 0)) { # All deltas are additions -> no problem
			data[ind_events] <- data[ind_events] + deltaPerEvent
		} else { # There are subtractions -> check that all adjusted precipitation days > 0
			newRainyValues <- data[ind_events] + deltaPerEvent
			posRainyDays <- newRainyValues >= 0
			
			if (all(posRainyDays)) { # all ok
				data[ind_events] <- newRainyValues
			} else { # Some rainy days would now be negative; this is not ok
				negRainyDays <- !posRainyDays
				data[ind_events][posRainyDays] <- newRainyValues[posRainyDays]
				data[ind_events][negRainyDays] <- 0
				StillToSubtract <- sum(newRainyValues[negRainyDays]) # 'StillToSubtract' is negative
				ppt_avail <- sum(data[ind_events])
				
				if (ppt_avail > 0) {
					# There is precipitation of the already adjusted rainy days from which to subtract
					temp <- Recall(data = data, ind_events = data > 0, addDelta = StillToSubtract)
					data <- temp$data
					StillToSubtract <- temp$PPT_to_remove
				}
			}
		}
		
		list(data = data, PPT_to_remove = StillToSubtract)
	})
	
	#' Adds/removes elements of data
	#'
	#' Adds/removes elements of data until \code{identical(length(data), targetLength)}
	#'
	#' Removal of elements is by random sampling. Addition of elements is by randomly
	#' placing them within the sequence of days of \code{data} and them randomly
	#' assigning values of the original \code{data} with replacement.
	#'
	#' @param data A numeric vector. Daily values of precipitation.
	#' @param targetLength An integer value.
	#'
	#' @return A copy of \code{data} with adjusted length.
	fix_PPTdata_length <- compiler::cmpfun(function(data, targetLength) {
		targetLength <- as.integer(targetLength)
		stopifnot(targetLength >= 0)
		
		Ndiff <- length(data) - targetLength
		absNdiff <- abs(Ndiff)
		
		if (absNdiff > 0) {
			if (Ndiff > 0) {
				# remove random days
				ids <- sample(x = length(data), size = absNdiff, replace = FALSE)
				data <- data[-ids]
			} else {
				# Imputation
				# add random days with randomly sampled precipitation
				ids <- sample(x = targetLength, size = absNdiff, replace = FALSE)
				temp <- rep(0, targetLength)
				temp[-ids] <- data
				temp[ids] <- sample(x = data, size = absNdiff, replace = TRUE)
				data <- temp
			}
		}
		
		data
	})
	
	#' @param data_N An integer value. The number of days of the precipitation record to consider.
	#' @param this_newPPTevent_N An integer value. The number of days which will received 'spill-over' precipitation.
	#' @param sigmaN An integer value. The multiplicator of \code{this_newPPTevent_N} to determine the range of days to consider.
	#' @param this_i_extreme. An integer value. The index indicating for which day in the precipitation record, precipitation is removed and redistributed.
	#' @param this_pptToDistribute. An integer value. The amount of precipitation that was removed from day \code{this_i_extreme} and is now redistributed to other days.
	calc_Days_withLoweredPPT <- compiler::cmpfun(function(data_N, this_newPPTevent_N, sigmaN, this_i_extreme, this_pptToDistribute) {
		this_newPPTevent_N <- max(0L, as.integer(this_newPPTevent_N))
		
		#Randomly select days within ± sigmaN days from a normal distribution		
		temp <- (-sigmaN * this_newPPTevent_N):(sigmaN * this_newPPTevent_N)
		xt <- temp[this_i_extreme + temp > 0]
		this_xt <- this_i_extreme + xt
		xt <- xt[1 <= this_xt & this_xt <= data_N] #do not select days from previous or next year
		probs <- dnorm(x = xt, mean = 0, sd = this_newPPTevent_N)
		probs[which.max(probs)] <- 0
		temp <- sample(x = xt, size = this_newPPTevent_N, replace = FALSE, prob = probs)
		dayDelta <- temp[.Internal(order(na.last = TRUE, decreasing = FALSE, abs(temp)))]
		
		#Distribute PPT to add among the selected days with a linear decay function
		newDays <- this_i_extreme + dayDelta
		temp <- 1 / abs(dayDelta)
		newPPT <- this_pptToDistribute * temp / sum(temp)
		
		list(newDays = newDays, newPPT = newPPT)
	})

	#' Check that data are within range of normal distribution
	#'
	#' @param data A numeric vector. Daily values of temperature.
	#' @param sigmaN An integer value. A multiplicator of \code{sd}.
	test_sigmaNormal <- compiler::cmpfun(function(data, sigmaN = 6) {
		md <- mean(data)
		sdd <- sd(data) * sigmaN
		stopifnot(data < md + sdd, data > md - sdd)
	})

	
	#' Check that data are within range of an approximated gamma distribution
	#'
	#' @param data A numeric vector. Daily values of precipitation.
	#' @param sigmaN An integer value. A multiplicator of \code{sd}.
	#' @references Choi, S. C., and R. Wette. 1969. Maximum Likelihood Estimation of the Parameters of the Gamma Distribution and Their Bias. Technometrics 11:683-690.
	#' @references http://en.wikipedia.org/wiki/Gamma_distribution#Maximum_likelihood_estimation
	test_sigmaGamma <- compiler::cmpfun(function(data, sigmaN = 6) {
		tempD <- data[data > 0]
		
		if (length(tempD) >= 2 && sd(tempD) > 0) {
			tempM <- mean(tempD)
			temp <- log(tempM) - mean(log(tempD))
			# Approximate shape and scale instead of very slow call: g <- MASS::fitdistr(data, "gamma")
			gshape <- (3 - temp + sqrt((temp - 3)^2 + 24 * temp)) / (12 * temp)
			gscale <- tempM / gshape
			stopifnot(data < qgamma(erf(sigmaN / sqrt(2)), shape = gshape, scale = gscale))
		}
	})
	
	
	#' @param data A numeric vector. Daily values of precipitation.
	#' @param dailyPPTceiling A numeric value. The maximum value of daily precipitation. Values above this limit will be removed and redistributed to other days.
	#' @param do_checks A logical value. See details.
	#' @param sigmaN An integer value. A multiplicator of \code{sd} for data checks.
	#' @param mfact A numeric value. See details.
	#' @details IF \code{TRUE} and any daily precipitation is equal or larger than \code{mfact * dailyPPTceiling}, then the code will error out.
	controlExtremePPTevents <- compiler::cmpfun(function(data, dailyPPTceiling, sigmaN, do_checks = FALSE, mfact = 10) {
		if (do_checks) stopifnot(data < mfact * dailyPPTceiling) #something went wrong, e.g., GCM data is off; (10 / 1.5 * dailyPPTceiling) -> dailyPPTtoExtremeToBeReal  #if more than 1000% of observed value then assume that something went wrong and error out
		
		data_N <- length(data)
		i_extreme <- which(data > dailyPPTceiling)
		irep <- 0
		
		while (length(i_extreme) > 0 && irep < 30) {
			# limit calls: if too many wet days, then not possible to distribute all the water!
			newValues <- dailyPPTceiling * runif(n = length(i_extreme), min = 0.9, max = 1)
			pptToDistribute <- data[i_extreme] - newValues
			data[i_extreme] <- newValues
			newPPTevent_N <- ceiling(pptToDistribute / dailyPPTceiling)
			stopifnot(sum(newPPTevent_N) <= data_N) 	# more days with dailyPPTceiling precipitation would be necessary than are available
			
			for (i in seq_along(i_extreme)) {
				newPPTevents <- calc_Days_withLoweredPPT(
					data_N = data_N, this_newPPTevent_N = newPPTevent_N[i], sigmaN = sigmaN,
					this_i_extreme = i_extreme[i], this_pptToDistribute = pptToDistribute[i])
						
				data[newPPTevents$newDays] <- data[newPPTevents$newDays] + newPPTevents$newPPT
			}
			
			# prepare for next iteration in case a day with previous ppt got too much ppt
			i_extreme <- which(data > dailyPPTceiling)
			irep <- irep + 1
		}
		
		if (do_checks) test_sigmaGamma(data = data, sigmaN)
		
		data
	})

	#' Add/multiply deltas to historic daily data to generate future daily SoilWat-formatted weather.
	#' Used by \code{downscale.raw}, \code{downscale.delta}, and \code{downscale.deltahybrid}
	applyDeltas <- compiler::cmpfun(function(obs.hist.daily, obs.hist.monthly, delta_ts, ppt_fun, sigmaN = 6, do_checks=FALSE) {
		dailyPPTceiling <- 1.5 * max(sapply(obs.hist.daily, FUN=function(obs) max(obs@data[,4]))) #Hamlet et al. 2010: "an arbitrary ceiling of 150% of the observed maximum precipitation value for each cell is also imposed by “spreading out” very large daily precipitation values into one or more adjacent days"

		res <- try(lapply(obs.hist.daily, FUN=function(obs) {
					month <- as.POSIXlt(paste(obs@year, obs@data[, "DOY"], sep="-"), format="%Y-%j")$mon + 1
					ydelta <- delta_ts[delta_ts[, "Year"] == obs@year, -(1:2)]
					tmax <- obs@data[, "Tmax_C"] + ydelta[month, "Tmax_C"]

					if (do_checks) test_sigmaNormal(data=tmax, sigmaN)

					tmin <- obs@data[, "Tmin_C"] + ydelta[month, "Tmin_C"]
					if (do_checks) test_sigmaNormal(data=tmin, sigmaN)

					ppt_data <- unlist(lapply(1:12, FUN=function(m) {
														im_month <- month == m
														m_ydelta <- ydelta[m, 3]
														m_data <- obs@data[im_month, "PPT_cm"]
														if (ppt_fun[m] == "*") {# multiply ppt
															res <- m_data * m_ydelta
														} else if (m_ydelta == 0) { #add ppt here and below: nothing to add
															res <- m_data
														} else if (any(i_rainyDays <- m_data > 0)) { #there are rainy days in the historic record: add to those
															res <- add_delta_to_PPT(data=m_data, ind_events=i_rainyDays, addDelta=m_ydelta)$data
														} else { #there are no rainy days in the historic record
															if (m_ydelta > 0) { #we need rainy days in the historic record to add precipitation
																if (any(i_rainyMYears <- obs.hist.monthly[obs.hist.monthly[, "Month"] == m, "PPT_cm"] > 0)) { #sample from the same historic month in an other with rainy days instead
																	#Locate data of same month in other year
																	i_newYear <- which(i_rainyMYears)[which.min(abs(obs.hist.monthly[obs.hist.monthly[, "Month"] == m, "PPT_cm"][i_rainyMYears] - m_ydelta))]
																	newMonth <- as.POSIXlt(paste((newObs <- obs.hist.daily[i_newYear][[1]])@year, newObs@data[, "DOY"], sep="-"), format="%Y-%j")$mon + 1
																	newMonthData <- newObs@data[, "PPT_cm"][newMonth == m]
																	#Adjust data
																	newMonthData <- fix_PPTdata_length(data=newMonthData, targetLength=sum(im_month)) #adjust number of days in case we got a leap year February issue
																	res <- add_delta_to_PPT(newMonthData, newMonthData > 0, addDelta = m_ydelta)$data
																} else if (any(i_rainyMonth <- obs.hist.monthly[, "PPT_cm"] > 0)) { #no rainy day for this month in historic record: locate rainy days in any months from other years
																	#Locate data of any month in any year
																	i_newMYear <- which(i_rainyMonth)[which.min(abs(obs.hist.monthly[i_rainyMonth, "PPT_cm"] - m_ydelta))]
																	i_newYear <- which(obs.hist.monthly[i_newMYear, "Year"] == sort(unique(obs.hist.monthly[, "Year"])))
																	newMonth <- as.POSIXlt(paste((newObs <- obs.hist.daily[i_newYear][[1]])@year, newObs@data[, "DOY"], sep="-"), format="%Y-%j")$mon + 1
																	newMonthData <- newObs@data[, "PPT_cm"][newMonth == obs.hist.monthly[i_newMYear, "Month"]]
																	#Adjust data
																	newMonthData <- fix_PPTdata_length(data=newMonthData, targetLength=sum(im_month)) #adjust number of days in case we got a month with a different number of days
																	res <- add_delta_to_PPT(newMonthData, newMonthData > 0, addDelta = m_ydelta)$data
																} else {
																	stop(paste("no rainy day in historic record, but requested for the future prediction"))
																}
															} else {#there is no rain in the historic record, so we cannot remove any
																res <- rep(0, length(m_data))
															}
														}
														return(res)
													}))
					
					ppt <- controlExtremePPTevents(data=ppt_data, dailyPPTceiling, do_checks=do_checks, sigmaN = sigmaN)			

					new("swWeatherData", data=round(data.matrix(cbind(obs@data[, "DOY"], tmax, tmin, ppt), rownames.force=FALSE), 2), year=obs@year)
				}), silent=TRUE)

		return(res)
	})

	

	#' Add/multiply deltas to historic daily precipitation to generate future daily precipitation without checks
	#' 
	#' @param m An integer vector. Each element corresponds to a day (i.e., length(m) is 365 or 366 days) and the values are the number of the month.
	#' @param data A numeric vector. Precipitation of each day.
	#' @param ydelta A numeric vector. Delta values for each day. If computed deltas are monthly, then they must be repeated for each day before passed as argument to this function.
	#' @param add_days A logical vector. \code{TRUE} for each day for which \code{ydelta} is applied additively.
	#' @param mult_days A logical vector. \code{TRUE} for each day for which \code{ydelta} is applied multiplicatively.
	#' @return A copy of \code{data} with adjusted values.
	applyPPTdelta_simple <- compiler::cmpfun(function(m, data, ydelta, add_days, mult_days) {
		ppt <- rep(0, length(data))
		ievents <- data > 0

		# additive delta
		if (any(add_days)) {
			# Spread monthly 'delta' amount of PPT to each daily precip event
			events_per_month <- tapply(as.integer(ievents), m, sum)[m]
			itemp <- add_days & ievents
			ppt[itemp] <- data[itemp] + ydelta[itemp] / events_per_month[itemp]
			# Simple correction for negative precipitation that can arise from subtractive deltas
			negppt <- ppt[itemp] < 0
			if (any(negppt)) ppt[itemp][negppt] <- 0
		}
		# multiplicative delta
		if (any(mult_days)) {
			itemp <- mult_days & ievents
			ppt[itemp] <- data[itemp] * ydelta[itemp]
		}

		ppt
	})

	#' Add/multiply deltas to historic daily precipitation to generate future daily precipitation with checks
	#'
	#' @inheritParams applyPPTdelta_simple
	#' @inheritParams downscale
	#' @return A list with two elements
	#'	\describe{
	#'		\item{data}{A copy of \code{data} with adjusted values.}
	#'		\item{PPT_to_remove}{The total amount of precipitation that couldn't be removed from \code{data} due to lack of precipitation.}
	#'	}
	applyPPTdelta_detailed <- compiler::cmpfun(function(m, data, ydelta, add_days, mult_days, daily, monthly) {
		ppt <- rep(0, length(data))
		PPT_to_remove <- 0
		
		# multiplicative delta
		if (any(mult_days)) {
			ppt[mult_days] <- data[mult_days] * ydelta[mult_days]
		}

		# additive delta
		if (any(add_days) && sum(abs(ydelta[add_days])) > 0) {
			ievents <- data > 0
			ievents_add <- ievents & add_days
			
			if (any(ievents_add)) {
				# there is precipitation among the days with additive delta: add to/subtract from those
				eventsN_add_per_month <- tapply(as.integer(ievents_add), m, sum)[m]
				temp <- add_delta_to_PPT(data = data[add_days],
									ind_events = ievents_add[add_days],
									deltaPerEvent = (ydelta / eventsN_add_per_month)[ievents_add])
				ppt[add_days] <- temp[["data"]]
				
				if (temp[["PPT_to_remove"]] < 0) {
					# there was not enough precipitation among the additive days; attempt to remove precipitation from all days
					temp <- add_delta_to_PPT(data = ppt,
										ind_events = ievents,
										addDelta = temp[["PPT_to_remove"]])
					ppt <- temp[["data"]]
					PPT_to_remove <- PPT_to_remove + temp[["PPT_to_remove"]]
				}
			} else {
				# there are no precip days in the data with additive delta
				idelta_pos <- ydelta > 0 & ievents_add
				idelta_neg <- ydelta < 0 & ievents_add

				if (any(idelta_pos)) {
					# there are positive deltas: sample days with precipitation events from the historic record (to preserve precipitation event distribution) and adjust the amounts
					ipos_months <- m[idelta_pos]
					
					for (im in unique(ipos_months)) {
						this_month_im <- monthly[, "Month"] == im
						month_precip <- monthly[, "PPT_cm"] > 0 
						this_month_precip <- month_precip & this_month_im
						precip_target <- (temp <- ydelta[idelta_pos][ipos_months == im])[1] / length(temp)

						if (any(this_month_precip)) {
							# locate data from the same historic month 'im' from a different year with the most similar monthly PPT
							itemp <- this_month_precip
						} else {
							# this month 'im' has no precipitation in all years
							if (any(month_precip)) {
								# locate data from any month from any year with the most similar monthly PPT
								itemp <- month_precip
							} else {
								stop(paste("Historic record has no days with precipitation, but requested for the future."))
							}
						}
						
						i_newYear <- monthly[itemp, "Year"][which.min(abs(monthly[itemp, "PPT_cm"] - precip_target))]
						newMonthData <- daily[[as.character(i_newYear)]]@data[m == im, "PPT_cm"]
						
						# Adjust data for this month
						these_days <- m == im
						newMonthData <- fix_PPTdata_length(data = newMonthData, targetLength = sum(these_days)) #adjust number of days in case we got a leap year February issue
						temp <- add_delta_to_PPT(data = newMonthData, ind_events = newMonthData > 0,
									addDelta = precip_target - sum(newMonthData))
						ppt[these_days] <- temp[["data"]]
						PPT_to_remove <- PPT_to_remove + temp[["PPT_to_remove"]]
					}
				}
				
				if (any(idelta_neg)) {
					# there are negative deltas and no precipitation among additive days: attempt to remove from all days otherwise return and attempt to remove from all years
					StillToSubtract <- sum(ydelta[idelta_neg]) # 'StillToSubtract' is negative

					# attempt to remove precipitation from all days
					temp <- add_delta_to_PPT(data = ppt,
										ind_events = ievents,
										addDelta = StillToSubtract)
					ppt <- temp[["data"]]
					PPT_to_remove <- PPT_to_remove + temp[["PPT_to_remove"]]
				}
			}
		}
		
		list(data = ppt, PPT_to_remove = PPT_to_remove)
	})
	

	applyDelta_oneYear <- compiler::cmpfun(function(obs, delta_ts, ppt_fun, daily, monthly, ppt_type = c("simple", "detailed"), dailyPPTceiling, sigmaN, do_checks) {
		ppt_type <- match.arg(ppt_type)

		month <- 1 + as.POSIXlt(seq(ISOdate(obs@year, 1, 1), ISOdate(obs@year, 12, 31), by = "day"))$mon
		ydeltas <- delta_ts[delta_ts[, "Year"] == obs@year, -(1:2)]
		add_days <- ppt_fun[month] == "+"
		mult_days <- !add_days
		PPT_to_remove <- 0
		
		tmax <- obs@data[, "Tmax_C"] + ydeltas[month, "Tmax_C"]
		if (do_checks) test_sigmaNormal(data=tmax, sigmaN)

		tmin <- obs@data[, "Tmin_C"] + ydeltas[month, "Tmin_C"]
		if (do_checks) test_sigmaNormal(data=tmin, sigmaN)
		
		if (ppt_type == "simple") {
			ppt <- applyPPTdelta_simple(m = month,
						data = obs@data[, "PPT_cm"],
						ydelta = ydeltas[month, "PPT_cm"],
						add_days = add_days, mult_days = mult_days)
						
		} else if (ppt_type == "detailed") {
			temp <- applyPPTdelta_detailed(m = month,
						data = obs@data[, "PPT_cm"],
						ydelta = ydeltas[month, "PPT_cm"],
						add_days = add_days, mult_days = mult_days,
						daily = daily, monthly = monthly)
			ppt <- temp[["data"]]
			PPT_to_remove <- temp[["PPT_to_remove"]]
			
			if (dailyPPTceiling > 0)
				ppt <- controlExtremePPTevents(data = ppt, 
							do_checks = do_checks,
							dailyPPTceiling = dailyPPTceiling,
							sigmaN = sigmaN)			
		}
	

		sw <- new("swWeatherData",
				data = round(data.matrix(cbind(obs@data[, "DOY"], tmax, tmin, ppt),
										rownames.force = FALSE),
							2),
				year = obs@year)
		list(sw = sw, PPT_to_remove = PPT_to_remove)
	})

	
	applyDeltas2 <- compiler::cmpfun(function(daily, monthly, years, delta_ts, ppt_fun, ppt_type = c("simple", "detailed"), dailyPPTceiling, sigmaN, do_checks = FALSE) {
		ppt_type <- match.arg(ppt_type)
		# daily_months <- 1 + as.POSIXlt(seq(ISOdate(years[1], 1, 1), ISOdate(years[length(years)], 12, 31), by = "day"))$mon

		sw_list <- list()
		totalPPT_to_remove <- 0
		
		for (i in seq_along(daily)) {
			temp <- applyDelta_oneYear(obs = daily[[i]],
						delta_ts = delta_ts, ppt_fun = ppt_fun, daily = daily, monthly = monthly,
						ppt_type = ppt_type, dailyPPTceiling = dailyPPTceiling, sigmaN = sigmaN, do_checks = do_checks)
			sw_list[[i]] <- temp[["sw"]]
			totalPPT_to_remove <- totalPPT_to_remove + temp[["PPT_to_remove"]]
		}
		
		if (totalPPT_to_remove < 0) {
			# Some years didn't have sufficient precipitation for the removal of the requested delta precipitation
			# Here, crude approach to remove this additional quantity by spreading it across all years
			
			daily2 <- dbW_weatherData_to_dataframe(sw_list)
			totalPPT <- sum(daily2[, "PPT_cm"])
			
			if (totalPPT > abs(totalPPT_to_remove)) {
				daily2[, "PPT_cm"] <- daily2[, "PPT_cm"] * (1 - abs(totalPPT_to_remove) / totalPPT)
			} else {
				warning(paste("Total site precipitation should be reduced on average by a further", round((abs(totalPPT_to_remove) - totalPPT) / length(daily), 2), "cm / year"))
				daily2[, "PPT_cm"] <- 0
			}
			
			sw_list <- dbW_dataframe_to_weatherData(daily2, years)
		}
	  	names(sw_list) <- years
		
		sw_list
	})


	
	#' Downscale and temporal disaggregation
	#' 
	#' @details Units are [degree Celsius] for temperature and [cm / day] or [cm / month], respectively, for precipitation
	#' 
	#' @param obs.hist.daily A list. Each element corresponds to one year of simstartyr:endyris and is an object of class \linkS4class{swWeatherData}.
	#' @param daily A list. Each element corresponds to one year of simstartyr:endyris and is an object of class \linkS4class{swWeatherData}.
	#' @param obs.hist.monthly A numeric matrix. Monthly time-series of observed weather calculated from \code{obs.hist.daily} for the years simstartyr:endyr.
	#' @param monthly A numeric matrix. Monthly time-series of observed weather calculated from \code{daily} for the years simstartyr:endyr.
	#' @param scen.hist.monthly A numeric matrix. Monthly time-series of scenario weather during the historic time period DScur_startyr:DScur_endyr
	#' @param scen.fut.monthly A numeric matrix. Monthly time-series of scenario weather during the projected time period DSfut_startyr:DSfut_endyr
	#' @param downscaling.options A named list.
	#' @param do_checks A logical value. If \code{TRUE} perform several sanity checks on the data.
	downscale <- function(obs.hist.daily, obs.hist.monthly, scen.fut.monthly, downscaling.options, do_checks = TRUE) {}

	

	#' Time periods for downscaling functions
	#' @inheritParams downscale
	downscale.periods <- compiler::cmpfun(function(obs.hist.daily, obs.hist.monthly, scen.hist.monthly = NULL, scen.fut.monthly = NULL, years = NULL, DScur_startyear = NULL, DScur_endyear = NULL, DSfut_startyear = NULL, DSfut_endyear = NULL) {
		# Time periods
		#	- historic observed period: simstartyr:endyr
		dyears <- sapply(obs.hist.daily, function(obs) obs@year)
		if (is.null(years)) years <- dyears
		startyear <- years[1]
		endyear <- years[length(years)]
		iuse_obs_hist_d <- dyears >= startyear & dyears <= endyear
		iuse_obs_hist_m <- obs.hist.monthly[, 1] >= startyear & obs.hist.monthly[, 1] <= endyear
		stopifnot(sum(iuse_obs_hist_m) == (endyear - startyear + 1) * 12)

		#	- historic training period: DScur_startyear:DScur_endyear
		if (!is.null(scen.hist.monthly)) {
			if (is.null(DScur_startyear)) DScur_startyear <- scen.hist.monthly[1, 1]
			if (is.null(DScur_endyear)) DScur_endyear <- scen.hist.monthly[nrow(scen.hist.monthly), 1]
			iuse_scen_hist_m <- scen.hist.monthly[, 1] >= DScur_startyear & scen.hist.monthly[, 1] <= DScur_endyear
			if (!(sum(iuse_scen_hist_m) == (DScur_endyear - DScur_startyear + 1) * 12)) {
				warning("downscale.periods: resulting record of 'scen.hist.monthly' covers only the years ", paste(range(scen.hist.monthly[iuse_scen_hist_m, 1]), collapse = "-"), " instead of the requested ", DScur_startyear, "-", DScur_endyear, immediate. = TRUE)
			}
		} else {
			DScur_startyear <- DScur_endyear <- iuse_scen_hist_m <- NULL
		}
		#	- future training period: DSfut_startyear:DSfut_endyear
		if (!is.null(scen.fut.monthly)) {
			if (is.null(DSfut_startyear)) DSfut_startyear <- scen.fut.monthly[1, 1]
			if (is.null(DSfut_endyear)) DSfut_endyear <- scen.fut.monthly[nrow(scen.fut.monthly), 1]
			iuse_scen_fut_m <- scen.fut.monthly[, 1] >= DSfut_startyear & scen.fut.monthly[, 1] <= DSfut_endyear
			if (!(sum(iuse_scen_fut_m) == (DSfut_endyear - DSfut_startyear + 1) * 12)) {
				warning("downscale.periods: resulting record of 'scen.fut.monthly' covers only the years ", paste(range(scen.fut.monthly[iuse_scen_fut_m, 1]), collapse = "-"), " instead of the requested ", DSfut_startyear, "-", DSfut_endyear, immediate. = TRUE)
			}
		} else {
			DSfut_startyear <- DSfut_endyear <- iuse_scen_fut_m <- NULL
		}

	
		# Return
		list(years = years, startyear = startyear, endyear = endyear,
			DScur_startyear = DScur_startyear, DScur_endyear = DScur_endyear,
			DSfut_startyear = DSfut_startyear, DSfut_endyear = DSfut_endyear,
			iuse_obs_hist_d = iuse_obs_hist_d, iuse_obs_hist_m = iuse_obs_hist_m,
			iuse_scen_hist_m = iuse_scen_hist_m, iuse_scen_fut_m = iuse_scen_fut_m)
	})

	#' Downscale with the 'direct approach'
	#' 
	#' See 'direct' approach in Lenderink et al. (2007)
	#'
	#' @inheritParams downscale
	#'
	#' @references Lenderink, G., A. Buishand, and W. van Deursen. 2007. Estimates of future discharges of the river Rhine using two scenario methodologies: direct versus delta approach. Hydrology and Earth System Sciences 11:1145-1159.
	#' @export
	downscale.raw <- compiler::cmpfun(function(obs.hist.daily, obs.hist.monthly, scen.fut.monthly, years = NULL, DScur_startyear = NULL, DScur_endyear = NULL, DSfut_startyear = NULL, DSfut_endyear = NULL, downscaling.options = list(applyPPT_type = "detailed", sigmaN = 6, PPTratioCutoff = 10), dailyPPTceiling, do_checks=TRUE) {
		# Time periods
		tp <- downscale.periods(obs.hist.daily, obs.hist.monthly, scen.hist.monthly = NULL, scen.fut.monthly, years, DScur_startyear, DScur_endyear, DSfut_startyear, DSfut_endyear)
		if (any(!tp$iuse_obs_hist_d)) obs.hist.daily <- obs.hist.daily[tp$iuse_obs_hist_d]
		if (any(!tp$iuse_obs_hist_m)) obs.hist.monthly <- obs.hist.monthly[tp$iuse_obs_hist_m, ]
		if (any(!tp$iuse_scen_fut_m)) scen.fut.monthly <- scen.fut.monthly[tp$iuse_scen_fut_m, ]
	
		# 1. Calculate mean monthly values in historic and future scenario values
		scen.fut.mean_tmax <- tapply(scen.fut.monthly[, "tmax"], INDEX = scen.fut.monthly[, "month"], mean, na.rm = TRUE)
		scen.fut.mean_tmin <- tapply(scen.fut.monthly[, "tmin"], INDEX = scen.fut.monthly[, "month"], mean, na.rm = TRUE)
		scen.fut.mean_ppt <- tapply(scen.fut.monthly[, "prcp"], INDEX = scen.fut.monthly[, "month"], sum, na.rm = TRUE)
		
		obs.hist.mean_tmax <- tapply(obs.hist.monthly[, "Tmax_C"], INDEX = obs.hist.monthly[, "Month"], mean, na.rm = TRUE)
		obs.hist.mean_tmin <- tapply(obs.hist.monthly[, "Tmin_C"], INDEX = obs.hist.monthly[, "Month"], mean, na.rm = TRUE)
		obs.hist.mean_ppt <- tapply(obs.hist.monthly[, "PPT_cm"], INDEX = obs.hist.monthly[, "Month"], sum, na.rm = TRUE)
		
		# 2. Calculate deltas between observed historic and future mean scenario values
				#	- Additive approach (Anandhi et al. 2011): Temp, close-to-zero PPT, small or very large PPT ratios
				#	- Multiplicative approach (Wang et al. 2014): PPT otherwise
		delta_ts <- matrix(NA, ncol=5, nrow=nrow(obs.hist.monthly), dimnames=list(NULL, c("Year", "Month", "Tmax_C", "Tmin_C", "PPT_cm")))
		delta_ts[, 1:2] <- obs.hist.monthly[, 1:2]
		ppt_fun <- rep("*", 12)

		# Deltas of monthly means
		delta_ts[, "Tmax_C"] <- scen.fut.mean_tmax - obs.hist.mean_tmax
		delta_ts[, "Tmin_C"] <- scen.fut.mean_tmin - obs.hist.mean_tmin
		delta_ppts <- scen.fut.mean_ppt / obs.hist.mean_ppt
		temp_add <- delta_ppts < 1 / (10 * downscaling.options[["PPTratioCutoff"]]) | delta_ppts > downscaling.options[["PPTratioCutoff"]]
		if (any(temp_add)) {
			ppt_fun[temp_add] <- "+"
			delta_ppts[temp_add] <- scen.fut.mean_ppt - obs.hist.mean_ppt
		}
		delta_ts[, "PPT_cm"] <- delta_ppts
		
		
		# 3. Apply deltas to historic daily weather
		try(applyDeltas2(daily = obs.hist.daily, monthly = obs.hist.monthly, 
				years = tp$years, delta_ts = delta_ts, ppt_fun = ppt_fun,
				ppt_type = downscaling.options[["applyPPT_type"]], dailyPPTceiling = dailyPPTceiling, sigmaN = downscaling.options[["sigmaN"]], do_checks = do_checks), silent = TRUE)
	})
	
	#' Downscale with the 'delta approach'
	#'
	#' @inheritParams downscale
	#'
	#' @references Hay, L. E., R. L. Wilby, and G. H. Leavesley. 2000. A comparison of delta change and downscaled gcm scenarios for three mountainous basins in the United States. Journal of the American Water Resources Association 36:387-397.
	#' @references Hamlet, A. F., E. P. Salathé, and P. Carrasco. 2010. Statistical downscaling techniques for global climate model simulations of temperature and precipitation with application to water resources planning studies. Chapter 4. Final Report for the Columbia Basin Climate Change Scenarios Project. Climate Impacts Group, Center for Science in the Earth System, Joint Institute for the Study of the Atmosphere and Ocean, University of Washington, Seattle, WA.
	#' @export
	downscale.delta <- compiler::cmpfun(function(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly, years = NULL, DScur_startyear = NULL, DScur_endyear = NULL, DSfut_startyear = NULL, DSfut_endyear = NULL, downscaling.options = list(applyPPT_type = "detailed", sigmaN = 6, PPTratioCutoff = 10), dailyPPTceiling, do_checks = TRUE) {
		# Time periods
		tp <- downscale.periods(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly, years, DScur_startyear, DScur_endyear, DSfut_startyear, DSfut_endyear)
		if (any(!tp$iuse_obs_hist_d)) obs.hist.daily <- obs.hist.daily[tp$iuse_obs_hist_d]
		if (any(!tp$iuse_obs_hist_m)) obs.hist.monthly <- obs.hist.monthly[tp$iuse_obs_hist_m, ]
		if (any(!tp$iuse_scen_hist_m)) scen.hist.monthly <- scen.hist.monthly[tp$iuse_scen_hist_m, ]
		if (any(!tp$iuse_scen_fut_m)) scen.fut.monthly <- scen.fut.monthly[tp$iuse_scen_fut_m, ]
		
		# 1. Calculate mean monthly values in historic and future scenario values
		scen.fut.mean_tmax <- tapply(scen.fut.monthly[, "tmax"], INDEX = scen.fut.monthly[, "month"], mean, na.rm = TRUE)
		scen.fut.mean_tmin <- tapply(scen.fut.monthly[, "tmin"], INDEX = scen.fut.monthly[, "month"], mean, na.rm = TRUE)
		scen.fut.mean_ppt <- tapply(scen.fut.monthly[, "prcp"], INDEX = scen.fut.monthly[, "month"], sum, na.rm = TRUE)

		scen.hist.mean_tmax <- tapply(scen.hist.monthly[, "tmax"], INDEX = scen.hist.monthly[, "month"], mean, na.rm = TRUE)
		scen.hist.mean_tmin <- tapply(scen.hist.monthly[, "tmin"], INDEX = scen.hist.monthly[, "month"], mean, na.rm = TRUE)
		scen.hist.mean_ppt <- tapply(scen.hist.monthly[, "prcp"], INDEX = scen.hist.monthly[, "month"], sum, na.rm = TRUE)

		
		# 2. Calculate deltas between historic and future mean scenario values
				#	- Additive approach (Anandhi et al. 2011): Temp, close-to-zero PPT, small or very large PPT ratios
				#	- Multiplicative approach (Wang et al. 2014): PPT otherwise
		delta_ts <- matrix(NA, ncol=5, nrow=nrow(obs.hist.monthly), dimnames=list(NULL, c("Year", "Month", "Tmax_C", "Tmin_C", "PPT_cm")))
		delta_ts[, 1:2] <- obs.hist.monthly[, 1:2]
		ppt_fun <- rep("*", 12)
		
		# Deltas of monthly means
		delta_ts[, "Tmax_C"] <- scen.fut.mean_tmax - scen.hist.mean_tmax
		delta_ts[, "Tmin_C"] <- scen.fut.mean_tmin - scen.hist.mean_tmin
		delta_ppts <- scen.fut.mean_ppt / scen.hist.mean_ppt
		temp_add <- delta_ppts < 1 / (10 * downscaling.options[["PPTratioCutoff"]]) | delta_ppts > downscaling.options[["PPTratioCutoff"]]

		if (any(temp_add)) {
			ppt_fun[temp_add] <- "+"
			delta_ppts[temp_add] <- scen.fut.mean_ppt - scen.hist.mean_ppt
		}
		delta_ts[, "PPT_cm"] <- delta_ppts
		
		
		# 3. Apply deltas to historic daily weather
		try(applyDeltas2(daily = obs.hist.daily, monthly = obs.hist.monthly, 
				years = tp$years, delta_ts = delta_ts, ppt_fun = ppt_fun,
				ppt_type = downscaling.options[["applyPPT_type"]], dailyPPTceiling = dailyPPTceiling, sigmaN = downscaling.options[["sigmaN"]], do_checks = do_checks), silent = TRUE)
	})
	
	#' Downscale with the 'delta-hybrid approach' old version (prior to May 2016)
	#' 
	#' Hybrid-delta downscaling developed by Hamlet et al. 2010 and Tohver et al. 2014.
	#' Applied, e.g., by Dickerson-Lange et al. 2014
	#'
	#' @inheritParams downscale
	#'
	#' @references Hamlet, A. F., E. P. Salathé, and P. Carrasco. 2010. Statistical downscaling techniques for global climate model simulations of temperature and precipitation with application to water resources planning studies. Chapter 4. Final Report for the Columbia Basin Climate Change Scenarios Project. Climate Impacts Group, Center for Science in the Earth System, Joint Institute for the Study of the Atmosphere and Ocean, University of Washington, Seattle, WA.
	#' @references Tohver, I.M., Hamlet, A.F. & Lee, S.-Y. (2014) Impacts of 21st-Century Climate Change on Hydrologic Extremes in the Pacific Northwest Region of North America. Journal of the American Water Resources Association, 50, 1461-1476.
	#' @references Anandhi, A., A. Frei, D. C. Pierson, E. M. Schneiderman, M. S. Zion, D. Lounsbury, and A. H. Matonse. 2011. Examination of change factor methodologies for climate change impact assessment. Water Resources Research 47:W03501.
	#' @references Dickerson-Lange, S. E., and R. Mitchell. 2014. Modeling the effects of climate change projections on streamflow in the Nooksack River basin, Northwest Washington. Hydrological Processes:doi: 10.1002/hyp.10012.
	#' @references Wang, L., and W. Chen. 2014. Equiratio cumulative distribution function matching as an improvement to the equidistant approach in bias correction of precipitation. Atmospheric Science Letters 15:1-6.
	#' @export
	downscale.deltahybrid <- compiler::cmpfun(function(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly, years = NULL, DScur_startyear = NULL, DScur_endyear = NULL, DSfut_startyear = NULL, DSfut_endyear = NULL, downscaling.options = list(sigmaN = 6, PPTratioCutoff = 10), do_checks=TRUE) {
		#Functions
		eCDF.Cunnane <- function(x) {
			na_N <- sum(is.na(x))
			x <- sort(x, na.last=NA)
			if (na_N > 0) {#if there are NAs in the data, add them in the middle assuming missing values represent median conditions
				i_center <- ceiling(length(x)/2)
				x <- c(x[1:i_center], rep(NA, na_N), x[(i_center+1):length(x)])
			}
			n <- length(x)
			q <- (1:n - 0.4) / (n + 0.2) #Cunnane (1978)
			f <- splinefun(x=q, y=x, method="monoH.FC", ties=mean) #'hyman' produces too extreme large values
			return(list(x=x, q=q, fun=f))
		}

		# Time periods
		tp <- downscale.periods(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly, years, DScur_startyear, DScur_endyear, DSfut_startyear, DSfut_endyear)
		if (any(!tp$iuse_obs_hist_d)) obs.hist.daily <- obs.hist.daily[tp$iuse_obs_hist_d]
		if (any(!tp$iuse_obs_hist_m)) obs.hist.monthly <- obs.hist.monthly[tp$iuse_obs_hist_m, ]
		if (any(!tp$iuse_scen_hist_m)) scen.hist.monthly <- scen.hist.monthly[tp$iuse_scen_hist_m, ]
		if (any(!tp$iuse_scen_fut_m)) scen.fut.monthly <- scen.fut.monthly[tp$iuse_scen_fut_m, ]

 		#Delta time series values									
		delta_ts <- matrix(NA, ncol=5, nrow=nrow(obs.hist.monthly), dimnames=list(NULL, c("Year", "Month", "Tmax_C", "Tmin_C", "PPT_cm")))
		delta_ts[, 1:2] <- obs.hist.monthly[, 1:2]
		ppt_fun <- rep("*", 12)
		for (iv in 1:3) {
			for (m in 1:12) {
				# 1. Calculate eCDF of historic weather and of scenario using Cunnane plotting position (Cunnane 1978) following Hamlet et al. 2010
				#		- interpolation and extrapolation by splines (Dickerson-Lange et al. 2014) instead of standard anomalies (Hamlet et al. 2010)
				obs.hist.x <- obs.hist.monthly[rep(1:12, times=nrow(obs.hist.monthly) / 12) == m, 2 + iv]
				scen.hist.x <- scen.hist.monthly[rep(1:12, times=nrow(scen.hist.monthly) / 12) == m, 2 + iv]
				scen.fut.x <- scen.fut.monthly[rep(1:12, times=nrow(scen.fut.monthly) / 12) == m, 2 + iv]
				
				#NA values are assumed to represent median conditions
				if (any(i_na <- is.na(obs.hist.x))) obs.hist.x[i_na] <- median(obs.hist.x, na.rm=TRUE)
				if (any(i_na <- is.na(scen.hist.x))) scen.hist.x[i_na] <- median(scen.hist.x, na.rm=TRUE)
				if (any(i_na <- is.na(scen.fut.x))) scen.fut.x[i_na] <- median(scen.fut.x, na.rm=TRUE)
				
				#eCDFs
				obs.hist.ecdf <- eCDF.Cunnane(obs.hist.x)
				scen.hist.ecdf <- eCDF.Cunnane(scen.hist.x)
				scen.fut.ecdf <- eCDF.Cunnane(scen.fut.x)
				
				# 2. Adjust future scenario with quantile-based deltas from historic comparison for future scenario values with linear extrapolation
				#	- Additive approach (Anandhi et al. 2011): Temp, close-to-zero PPT, small or very large PPT ratios
				#	- Multiplicative approach (Wang et al. 2014): PPT otherwise
				scHistToFut <- scen.hist.ecdf$fun(scen.fut.ecdf$q, extrapol="linear")
				scHistToFutRatio <- obs.hist.ecdf$fun(scen.fut.ecdf$q, extrapol="linear") / scHistToFut
				if (any(iv <= 2, any(scHistToFut < 1/(10*downscaling.options[["PPTratioCutoff"]])), any(scHistToFutRatio > downscaling.options[["PPTratioCutoff"]]), any(scHistToFutRatio < 1/downscaling.options[["PPTratioCutoff"]]))) {
					scen.fut.xadj <- scen.fut.x + obs.hist.ecdf$fun(scen.fut.ecdf$q, extrapol="linear") - scHistToFut
					if (all(iv == 3, sum(temp0 <- (scen.fut.xadj < 0)) > 0)) scen.fut.xadj[temp0] <- 0
				} else {
					scen.fut.xadj <- scen.fut.x * scHistToFutRatio
				}
				stopifnot(is.finite(scen.fut.xadj))
				if (do_checks) {
					if (iv <= 2) test_sigmaNormal(data=scen.fut.xadj, downscaling.options[["sigmaN"]])
					if (iv == 3) test_sigmaGamma(data=scen.fut.xadj, downscaling.options[["sigmaN"]])
				}

				# 3. Calculate eCDF of future adjusted scenario
				scen.fut2.ecdf <- eCDF.Cunnane(scen.fut.xadj)
				
				# 5. Quantile map observed historic to adjusted future scenario
				#	- Additive approach (Anandhi et al. 2011): Temp, close-to-zero PPT, small or very large PPT ratios
				#	- Multiplicative approach (Wang et al. 2014): PPT otherwise
				scHistToHist <- obs.hist.ecdf$fun(obs.hist.ecdf$q, extrapol="linear")
				scHistToFutRatio <- scen.fut2.ecdf$fun(obs.hist.ecdf$q, extrapol="linear") / scHistToHist
				if (any(iv <= 2, any(scHistToHist < 1/(10*downscaling.options[["PPTratioCutoff"]])), any(scHistToFutRatio > downscaling.options[["PPTratioCutoff"]]), any(scHistToFutRatio < 1/downscaling.options[["PPTratioCutoff"]]))) {
					mapFut <- scen.fut2.ecdf$fun(obs.hist.ecdf$q, extrapol="linear") - scHistToHist
					if (iv == 3) ppt_fun[m] <- "+"
				} else {
					mapFut <- scHistToFutRatio
					stopifnot(all(!is.infinite(mapFut)), all(!is.nan(mapFut))) #if (sum(temp <- is.nan(mapFut)) > 0) mapFut[temp] <- 0
				}
				delta_ts[delta_ts[, "Month"] == m, 2 + iv] <- mapFut[rank(obs.hist.x, ties.method="random")]
			}
		}
		
		# 6. Apply deltas to historic daily weather
		applyDeltas(obs.hist.daily, obs.hist.monthly, delta_ts, ppt_fun, downscaling.options[["sigmaN"]], do_checks=do_checks)
	})
   
	#------------------------
	doQmapQUANT.default_drs <- compiler::cmpfun(function(x, fobj, type = c("linear", "tricub"), linear_extrapolation=c("Boe", "Thermessl2012CC.QMv1b", "none"), spline_method=c("monoH.FC", "fmm", "natural"), monthly_extremes=NULL, correctSplineFun_type=c("fail", "none", "attempt"), ...) {
	  # Note: differs from call to call if jitter correction is used
	  
	  type <- match.arg(type)
	  linear_extrapolation <- match.arg(linear_extrapolation)
	  spline_method <- match.arg(spline_method)
	  correctSplineFun_type <- match.arg(correctSplineFun_type)
	  
	  wet <- if (!is.null(fobj$wet.day)) {
	    x >= fobj$wet.day
	  } else {
	    rep(TRUE, length(x))
	  }
	  out <- rep(NA, length.out = length(x))
	  
	  if (type == "linear") {
	    out[wet] <- approx(x = fobj$par$modq[, 1], y = fobj$par$fitq[, 1], xout = x[wet], method = "linear", rule = 2, ties = mean)$y
	    if (!(linear_extrapolation == "none")) {
	      # "same extrapolation as Boe et al. (2007), but neglecting the three highest/lowest correction terms" Thermessl et al. 2011 Climatic Change
	      qid <- switch(linear_extrapolation, Boe=0, Thermessl2012CC.QMv1b=3)
	      nq <- nrow(fobj$par$modq)
	      largex <- x > fobj$par$modq[nq, 1] + tol
	      if (any(largex)) {
	        max.delta <- fobj$par$modq[nq - qid, 1] - fobj$par$fitq[nq - qid, 1]
	        out[largex] <- x[largex] - max.delta
	      }
	      smallx <- x < fobj$par$modq[1, 1] - tol
	      if (any(smallx)) {
	        min.delta <- fobj$par$modq[1 + qid, 1] - fobj$par$fitq[1 + qid, 1]
	        out[smallx] <- x[smallx] - min.delta
	      }
	    }
	  } else if (type == "tricub") {
	    sfun <- splinefun(x = fobj$par$modq[, 1], y = fobj$par$fitq[, 1], method = spline_method) #only "monoH.FC" would be appropriate here because we would want a monotone function if possible
	    temp <- sfun(x[wet])
	    
	    #There seem to be at least two causes for abnormally high values from sfun()
	    #	1) extrapolation error
	    #	2) huge oscillations
	    #		2a) arising from non-monotone splines ('fmm' and 'natural')
	    #		2b) arising from numerical instabilities in the exact monotonicity for 'monoH.FC'
	    
	    if (!is.null(monthly_extremes) && !(correctSplineFun_type == "none")) {
	      # version previous to 20150705 didn't catch several bad cases, e.g., ix = 180099
	      # to prevent huge oscillation in 'fmm' and 'natural', we need to bound values between some small and some not-too large number
	      # apparently 'monoH.FC' does also show huge oscillations, e.g., ix=82529 because of numerical instabilities in the exact monotonicity in fobj$par$modq[, 1]
			icount <- 1
			while ((itemp <- sum((temp < monthly_extremes[1]) | (temp > monthly_extremes[2]))) > 0 && icount < 10) {
			  if (correctSplineFun_type == "fail") stop("Out-of-range splinefun values and 'correctSplineFun_type' set to fail")
			  sfun <- splinefun(x=jitter(fobj$par$modq[, 1]), y=jitter(fobj$par$fitq[, 1]), method=spline_method)
			  temp <- sfun(x[wet])
			  icount <- icount + 1
			}
			if (itemp > 0) stop("Jitter failed to fix out-of-range splinefun values")
	    }
	    
	    out[wet] <- temp
	  }
	  out[!wet] <- 0
	  if (!is.null(fobj$wet.day))  out[out < 0] <- 0
	  
	  out
	})
	
	doQmapQUANT_drs <- compiler::cmpfun(function(x, fobj, type=c("linear_Boe", "linear_Thermessl2012CC.QMv1b", "linear_none", "tricub_fmm", "tricub_monoH.FC", "tricub_natural", "normal_anomalies"), montly_obs_base=NULL, monthly_extremes=NULL, correctSplineFun_type=c("fail", "none", "attempt"), ...) {
		correctSplineFun_type <- match.arg(correctSplineFun_type)
		type <- match.arg(type)
		temp <- strsplit(type, "_", fixed=TRUE)[[1]]
		type <- temp[1]
		type_mod <- temp[2]

		if (type == "linear") {
			out <- try(doQmapQUANT.default_drs(x, fobj, type="linear", linear_extrapolation=type_mod, monthly_extremes=monthly_extremes, correctSplineFun_type=correctSplineFun_type, ...), silent=TRUE)
		} else if (type == "tricub") {
			out <- try(doQmapQUANT.default_drs(x, fobj, type="tricub", spline_method=type_mod, monthly_extremes=monthly_extremes, correctSplineFun_type=correctSplineFun_type, ...), silent=TRUE)
		} else if (type == "normal") {
			# Tohver, I. M., A. F. Hamlet, and S.-Y. Lee. 2014. Impacts of 21st-Century Climate Change on Hydrologic Extremes in the Pacific Northwest Region of North America. Journal of the American Water Resources Association 50:1461-1476.
			# Appendix A, p. 6: "... values that are outside the observed quantile map (e.g. in the early parts of the 20th century) are interpolated using standard anomalies (i.e. number of standard deviations from the mean) calculated for the observed data and GCM data. Although this approach ostensibly assumes a normal distribution, it was found during testing to be much more stable than attempts to use more sophisticated approaches. In particular, the use of Extreme Value Type I or Generalized Extreme Value distributions for extending the tail of the probability distributions were both found to be highly unstable in practice and introduced unacceptable daily extremes in isolated grid cells. These errors occur because of irregularities in the shapes of the CDFs for observed and GCM data, which relates in part to the relatively small sample size used to construct the monthly CDFs (i.e. n = 30)."

			out <- doQmapQUANT.default_drs(x, fobj, type="linear", linear_extrapolation="Boe", monthly_extremes=monthly_extremes, correctSplineFun_type=correctSplineFun_type, ...)

			target_range <- c(-Inf, fobj$par$modq[1, 1] -  tol, max(fobj$par$modq[, 1]) + tol, Inf) # -Inf, smallest observed value, largest observed value, Inf
			out_of_range <- !(findInterval(x, target_range) == 2)

			if (any(out_of_range)) {
				tscore_x <- (x[out_of_range] - mean(montly_obs_base)) / sd(montly_obs_base)
				out[out_of_range] <- mean(out[!out_of_range]) + sd(out[!out_of_range]) * tscore_x 
			}
		}

		out
	})

	
	#------------------------
	
	#' Downscale with the 'delta-hybrid approach' new version (post to May 2016)
	#' 
	#' Hybrid-delta downscaling developed by Hamlet et al. 2010 and Tohver et al. 2014.
	#' Applied, e.g., by Dickerson-Lange et al. 2014.
	#' Quantile mapping performed by functions modified from Gudmundsson et al. 2012.
	#'
	#' @inheritParams downscale
	#'
	#' @references Hamlet, A. F., E. P. Salathé, and P. Carrasco. 2010. Statistical downscaling techniques for global climate model simulations of temperature and precipitation with application to water resources planning studies. Chapter 4. Final Report for the Columbia Basin Climate Change Scenarios Project. Climate Impacts Group, Center for Science in the Earth System, Joint Institute for the Study of the Atmosphere and Ocean, University of Washington, Seattle, WA.
	#' @references Tohver, I.M., Hamlet, A.F. & Lee, S.-Y. (2014) Impacts of 21st-Century Climate Change on Hydrologic Extremes in the Pacific Northwest Region of North America. Journal of the American Water Resources Association, 50, 1461-1476.
	#' @references Anandhi, A., A. Frei, D. C. Pierson, E. M. Schneiderman, M. S. Zion, D. Lounsbury, and A. H. Matonse. 2011. Examination of change factor methodologies for climate change impact assessment. Water Resources Research 47:W03501.
	#' @references Dickerson-Lange, S. E., and R. Mitchell. 2014. Modeling the effects of climate change projections on streamflow in the Nooksack River basin, Northwest Washington. Hydrological Processes:doi: 10.1002/hyp.10012.
	#' @references Wang, L., and W. Chen. 2014. Equiratio cumulative distribution function matching as an improvement to the equidistant approach in bias correction of precipitation. Atmospheric Science Letters 15:1-6.
	#' @references Gudmundsson, L., Bremnes, J.B., Haugen, J.E. & Engen-Skaugen, T. (2012). Technical Note: Downscaling RCM precipitation to the station scale using statistical transformations - a comparison of methods. Hydrol Earth Syst Sci, 16, 3383-3390.
	#' @export
	downscale.deltahybrid3mod <- compiler::cmpfun(function(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly,
	                                      deltaFuture_yr, years = NULL, 
	                                      DScur_startyear = NULL, DScur_endyear = NULL,
	                                      DSfut_startyear = NULL, DSfut_endyear = NULL,
	                                      downscaling.options = list(extrapol_type = "linear_Thermessl2012CC.QMv1b",
	                                      		applyPPT_type = "detailed",
	                                      		sigmaN = 6,
												PPTratioCutoff = 10,
												correctSplineFun_type = "attempt"),
										  dailyPPTceiling, monthly_extremes,
	                                      do_checks = TRUE) {

		stopifnot(requireNamespace("qmap"))
		qstep <- 0.01
		nboot <- 1

		# Time periods
		tp <- downscale.periods(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly, years, DScur_startyear, DScur_endyear, DSfut_startyear, DSfut_endyear)
		if (any(!tp$iuse_obs_hist_d)) obs.hist.daily <- obs.hist.daily[tp$iuse_obs_hist_d]
		if (any(!tp$iuse_obs_hist_m)) obs.hist.monthly <- obs.hist.monthly[tp$iuse_obs_hist_m, ]
		if (any(!tp$iuse_scen_hist_m)) scen.hist.monthly <- scen.hist.monthly[tp$iuse_scen_hist_m, ]
		if (any(!tp$iuse_scen_fut_m)) scen.fut.monthly <- scen.fut.monthly[tp$iuse_scen_fut_m, ]

		# Data objects
		sbc.hist.monthly <- matrix(NA, ncol=5, nrow=nrow(scen.hist.monthly), dimnames=list(NULL, colnames(obs.hist.monthly)))
		sbc.hist.monthly[, 1:2] <- scen.hist.monthly[, 1:2]
		sbc.fut.monthly <- matrix(NA, ncol=5, nrow=nrow(scen.fut.monthly), dimnames=list(NULL, colnames(obs.hist.monthly)))
		sbc.fut.monthly[, 1:2] <- scen.fut.monthly[, 1:2]
		#	future simulation years = delta + simstartyr:endyr
		hd.fut.monthly <- delta_ts <- matrix(NA, ncol=5, nrow=nrow(obs.hist.monthly), dimnames=list(NULL, colnames(obs.hist.monthly)))
		hd.fut.monthly[, 1:2] <- delta_ts[, 1:2] <- obs.hist.monthly[, 1:2]
		hd.fut.monthly[, 1] <- hd.fut.monthly[, 1] + deltaFuture_yr

		#------STEPS 1-4 based on the appendix of Tohver et al. 2014
		for (iv in 1:3) {	# for each variable separately: Tmax, Tmin, PPT
			# NAs in scenario data: impute with median conditions (TODO(drs): implement a more sophisticated imputation scheme; this one biases variation downwards)
			if (anyNA(scen.hist.monthly[, 2 + iv])) {
				id_nas <- is.na(scen.hist.monthly[, 2 + iv])
				scen.hist.monthly[id_nas, 2 + iv] <- median(scen.hist.monthly[, 2 + iv], na.rm=TRUE)
			}
			if (anyNA(scen.fut.monthly[, 2 + iv])) {
				id_nas <- is.na(scen.fut.monthly[, 2 + iv])
				scen.fut.monthly[id_nas, 2 + iv] <- median(scen.fut.monthly[, 2 + iv], na.rm=TRUE)
			}
	    
			#---STEP 1: Statistical bias correction of GCM data
			# 1st part of this step is NOT carried out here because our GCM data is already BCSD downscaled: "first aggregating the gridded T and P observations to the GCM grid scale (at the time of this writing typically about 200km resolution)"

			# fit quantile map based on training data of same historic time period
			qm_fit <- qmap::fitQmapQUANT.default(obs=obs.hist.monthly[, 2 + iv], mod=scen.hist.monthly[, 2 + iv], qstep=qstep, nboot=nboot, wet.day=FALSE)
	    
			# 2nd part: bias correcting historic data ("then using quantile mapping techniques to remove the systematic bias in the GCM simulations relative to the observed probability distributions")
			temp <- try(doQmapQUANT_drs(x=scen.hist.monthly[, 2 + iv], fobj=qm_fit, type=downscaling.options[["extrapol_type"]], montly_obs_base=obs.hist.monthly[, 2 + iv], monthly_extremes=monthly_extremes[[iv]], correctSplineFun_type=downscaling.options[["correctSplineFun_type"]]), silent = TRUE)
			if (inherits(temp, "try-error")) return(temp)
			sbc.hist.monthly[, 2 + iv] <- temp
	    
			# 3rd part: bias correcting future data ("the same quantile map between simulations and observations is used to transform the future simulations from the GCM")
			temp <- try(doQmapQUANT_drs(x=scen.fut.monthly[, 2 + iv], fobj=qm_fit, type=downscaling.options[["extrapol_type"]], montly_obs_base=obs.hist.monthly[, 2 + iv], monthly_extremes=monthly_extremes[[iv]], correctSplineFun_type=downscaling.options[["correctSplineFun_type"]]), silent = TRUE)
			if (inherits(temp, "try-error")) return(temp)
			sbc.fut.monthly[, 2 + iv] <- temp


			#---STEP 2: Spatial downscaling
			# 	- "the monthly T and P values at the GCM grid scale are interpolated to the fine scale grid"
			# 	-> not done here because spatial aggregation (step 1, 1st part) not carried out

			for (im in 1:12) { # for each month separately
				#---STEP 3: Remapping the Historical Record to Interpolated GCM data
				id_sim_months <- obs.hist.monthly[, "Month"] == im	#identical(obs.hist.monthly[, 2], hd.fut.monthly[, 2])

				qm_fitm <- qmap::fitQmapQUANT.default(obs=sbc.fut.monthly[sbc.fut.monthly[, 2] == im, 2 + iv], mod=obs.hist.monthly[id_sim_months, 2 + iv], qstep=qstep, nboot=nboot, wet.day=FALSE)
				temp <- try(doQmapQUANT_drs(x=obs.hist.monthly[id_sim_months, 2 + iv], fobj=qm_fitm, type=downscaling.options[["extrapol_type"]], montly_obs_base=obs.hist.monthly[, 2 + iv], monthly_extremes=monthly_extremes[[iv]], correctSplineFun_type=downscaling.options[["correctSplineFun_type"]]), silent = TRUE)
				if (inherits(temp, "try-error")) return(temp)
				hd.fut.monthly[id_sim_months, 2 + iv] <- temp
			}
		}

		#---STEP 4: Daily Time Step Disaggregation of Monthly Data
		delta_ts[, c("Tmax_C", "Tmin_C")] <- hd.fut.monthly[, c("Tmax_C", "Tmin_C")] - obs.hist.monthly[, c("Tmax_C", "Tmin_C")] # equation 8

		ppt_fun <- rep("*", 12)
		delta_ppts <- hd.fut.monthly[, "PPT_cm"] / obs.hist.monthly[, "PPT_cm"] # equation 7
		temp_add <- is.infinite(delta_ppts) | is.nan(delta_ppts) | delta_ppts > downscaling.options[["PPTratioCutoff"]] | delta_ppts < 1 / downscaling.options[["PPTratioCutoff"]]
		if (any(temp_add)) {
			ids_m <- unique(delta_ts[temp_add, "Month"])
			ppt_fun[ids_m] <- "+"
			temp_m <- delta_ts[, "Month"] %in% ids_m # all calendar month for which at least one instance qualifies for additive PPT
			delta_ppts[temp_m] <- hd.fut.monthly[temp_m, "PPT_cm"] - obs.hist.monthly[temp_m, "PPT_cm"]
		}
		delta_ts[, "PPT_cm"] <- delta_ppts
      
	    # Apply deltas to historic daily weather
	    # Note: PPT differs from call to call to applyDeltas() because of controlExtremePPTevents (if dailyPPTceiling > 0)
		try(applyDeltas2(daily = obs.hist.daily, monthly = obs.hist.monthly,
	    						years = tp$years, delta_ts, ppt_fun, ppt_type = downscaling.options[["applyPPT_type"]], dailyPPTceiling, sigmaN = downscaling.options[["sigmaN"]], do_checks = do_checks), silent = FALSE)
	})



	#-------DB access functions
	if (exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA) {	
	
		mmPerSecond_to_cmPerMonth <- compiler::cmpfun(function(prcp_mmPerSecond, dpm) {
			prcp_mmPerSecond * dpm * 8640	# 24 * 60 * 60 / 10 == 8640
		})

		get.request <- compiler::cmpfun(function(service, request, i, variable, scen, gcm, lon, lat, startyear, endyear, dir.out.temp, useRCurl, saveNEXtempfiles) {
			if (useRCurl && !saveNEXtempfiles) {
				success <- try(getURL(request, .opts=list(timeout=5*60, connecttimeout=60)), silent=TRUE)
				if (!inherits(success, "try-error")) {
					if (isTRUE(grepl("Not Found", success, ignore.case=TRUE))) {
						class(success) <- "try-error"
					} else {
						if (service == "ncss") {
							ftemp <- textConnection(success)
						} else if (service == "opendap") {
							ftemp <- textConnection((temp <- strsplit(success, split="\n\n", fixed=TRUE))[[1]][3])
							ttemp <- as.POSIXlt("1950-01-01") + 86400 * as.numeric(scan(text=sub("\n", ",", temp[[1]][4], fixed=TRUE), what="character", sep=",", quiet=TRUE)[-1])
						}
						success <- 0
					}
				}
			} else {
				if (service == "opendap") stop("Curl must be present to access NEX-DCP30 data via thredds/dodsC (opendap)")
				ftemp <- file.path(dir.out.temp, paste0("NEX_", gcm, "_", scen, "_", variable, "_", round(lat, 5), "&", round(lon, 5), ".csv"))
				success <- try(download.file(url=request, destfile=ftemp, quiet=TRUE), silent=TRUE)
			}
	
			yearsN <- endyear - startyear + 1
			dat <- rep(NA, times=12*yearsN)
			if (!inherits(success, "try-error") && success == 0) {
				if (service == "ncss") {
					temp <- read.csv(ftemp, colClasses=c("POSIXct", "NULL", "NULL", "numeric")) #colnames = Time, Lat, Long, Variable
					vtemp <- temp[, 2]
					ttemp <- as.POSIXlt(temp[, 1])
				} else if (service == "opendap") {
					vtemp <- read.csv(ftemp, colClasses=c("NULL", "numeric"), header=FALSE)[-1, ] #columns = Index, Variable
				}
				if (!saveNEXtempfiles && !useRCurl && file.exists(ftemp)) unlink(ftemp)
				if (length(vtemp) < 12*yearsN) { #some GCMs only have values up to Nov 2099
					tempYearMonth <- paste(ttemp$year + 1900, ttemp$mo + 1, sep="_")
					targetYearMonth <- paste(rep(startyear:endyear, each=12), rep(1:12, times=yearsN), sep="_")
					iavail <- match(targetYearMonth, tempYearMonth, nomatch = 0)
					dat[iaval > 0] <- vtemp[iavail]
				} else {
					dat <- vtemp
				}
			} else {
				stop(paste(i, "th extraction of NEX at", Sys.time(), "for", gcm, scen, "at", lon, lat, ": not successful"))
			}
		
			dat
		})

	
		get.DBvariable <- compiler::cmpfun(function(i, variable, scen, gcm, lon, lat, bbox, startyear, endyear, dir.out.temp, useRCurl, saveNEXtempfiles) {
			gcmrun <- "r1i1p1"
			#1st attempt: TRHEDDS ncss/netCDF subsetting service
			request <- paste0(paste("http://dataserver.nccs.nasa.gov", "thredds/ncss/grid/bypass/NEX-DCP30/bcsd", scen, gcmrun,
								paste0(gcm, "_", variable, ".ncml"), sep="/"), "?var=", paste0(gcm, "_", variable), 
								"&latitude=", lat, "&longitude=", ifelse(lon > 180, lon - 360, lon),
								paste0("&time_start=", startyear, "-01-01T00%3A00%3A00Z&time_end=", endyear, "-12-31T23%3A59%3A59Z&timeStride=1"),
								"&accept=csv")
			dat <- get.request(service="ncss", request, i, variable, scen, gcm, lon, lat, startyear, endyear, dir.out.temp, useRCurl, saveNEXtempfiles)
			if (inherits(dat, "try-error") || any(dat > 1e5 | dat < -1e5, na.rm=TRUE)) { #thredds/ncss/ returns for some GCMs/RCPs/locations unrealistic large values, e.g., 9.969210e+36 and sometimes 2.670153e+42 for pr, tasmin, and tasmax for the month of May in every fifth year (2071, 2076, ...): bug report to NASA NCCS Support Team on June 2, 2014 - confirmed on June 8, 2014 by Yingshuo Shen (issue=48932)
				#2nd attempt: TRHEDDS opendap/dodsC
				lat.index <- round((lat - bbox$lat[1]) / 0.0083333333, 0)
				lon.index <- round((lon - bbox$lon[1]) / 0.0083333333, 0)
				if (startyear < 2006 && scen == "historical") {
					index.time.start <- (startyear - 1950) * 12
					index.time.end <- (endyear + 1 - 1950) * 12 - 1
				} else {
					index.time.start <- (startyear - 2006) * 12
					index.time.end <- (endyear + 1 - 2006) * 12 - 1
				}
				request <- paste0(paste("http://dataserver.nccs.nasa.gov", "thredds/dodsC/bypass/NEX-DCP30/bcsd", scen, gcmrun,
								paste0(gcm, "_", variable, ".ncml.ascii"), sep="/"),
								"?lat[", lat.index, "],lon[", lon.index, "],",
								gcm, "_", variable, "[", index.time.start, ":1:", index.time.end, "][", lat.index, "][", lon.index, "]")

				dat <- get.request(service="opendap", request, i, variable, scen, gcm, lon, lat, startyear, endyear, dir.out.temp, useRCurl, saveNEXtempfiles)
				stopifnot(!inherits(dat, "try-error"), all(dat < 1e5 & dat > -1e5, na.rm=TRUE))
			}
		
			dat
		})
		
		#' @return A list of one data.frame object with 5 columns and names of
		#' "year", "month", "tmax", "tmin", and "prcp". Each row is one day.
		#' Units are [degree Celsius] for temperature and [cm / day] and [cm / month], respectively, for precipitation.
		get_GCMdata <- compiler::cmpfun(function(i, ts_mons, dpm, gcm, scen, lon, lat, startyear, endyear, varTags, ...) {
			dots <- list(...) # bbox, dir.out.temp, useRCurl, saveNEXtempfiles
			
			clim <- vector("list", length=3)
			names(clim) <- varTags
			
			for (iv in seq_along(varTags)) {
				#Extract data
				clim[[iv]] <- get.DBvariable(i, variable=varTags[iv], scen=scen, gcm=gcm, lon=lon, lat=lat, bbox = dots[["bbox"]], startyear=startyear, endyear=endyear, dir.out.temp = dots[["dir.out.temp"]], useRCurl = dots[["useRCurl"]], saveNEXtempfiles = dots[["saveNEXtempfiles"]])
				#Adjust units
				if (varTags[iv] == "pr") {#convert kg/m2/s -> cm/month
					clim[[iv]] <- mmPerSecond_to_cmPerMonth(prcp_mmPerSecond=clim[[iv]], dpm)
				} else if (grepl("tas", varTags[iv])) {	#convert K -> C
					clim[[iv]] <- clim[[iv]] - 273.15
				}
			}
		
			#Monthly weather time-series
			list(cbind(year=ts_mons$year + 1900, month=ts_mons$mon + 1, tmax = clim[["tasmax"]], tmin = clim[["tasmin"]], prcp = clim[["pr"]]))
		})
	}		

	if (exinfo$GDODCPUCLLNL) {
		whereNearest <- compiler::cmpfun(function(val, matrix) {
			#this returns the index of the closest value in the matrix to the passed in value.
			which.min(abs(matrix - val))
		})

		get.SpatialIndices <- compiler::cmpfun(function(filename, lon, lat) {
			nc <- nc_open(filename=filename, write=FALSE, readunlim=TRUE, verbose=FALSE)
		
			#Get latitudes/longitudes from the netCDF files...; they are the same for each CMIP x extent
			#	- these are used to get the correct indices in the whereNearest function
			lats <- nc$dim$lat$vals
			lons <- nc$dim$lon$vals
			#close the netCDF file
			nc_close(nc)
		
			if (any(lons > 180)) lons <- ifelse(lons > 180, lons - 360, lons)
			#Calculate the spatial indices
			ncg <- NULL
			ncg$ix <- whereNearest(val=lon, matrix=lons)
			ncg$iy <- whereNearest(val=lat, matrix=lats)

			ncg
		})

		get.TimeIndices <- compiler::cmpfun(function(filename, startyear, endyear) {
			nc <- nc_open(filename=filename, write=FALSE, readunlim=TRUE, verbose=FALSE)

			utemp <- nc$dim$time$units
			tvals <- nc$dim$time$vals
			nc_close(nc)
		
			N <- length(tvals)
			temp <- lapply(strsplit(utemp, split=" ", fixed=TRUE)[[1]], function(x) as.Date(x, format="%Y-%m-%d"))
			baseYear <- temp[sapply(temp, function(x) !is.na(x))][[1]]
			stopifnot(length(baseYear) == 1)

			firstDate <- as.POSIXlt(baseYear + tvals[1])
			lastDate <- as.POSIXlt(baseYear + tvals[N])
		
			startYear <- firstDate$year + 1900
			startMonth <- firstDate$mon + 1
			endYear <- lastDate$year + 1900
			endMonth <- lastDate$mon + 1
		
			stopifnot(startYear <= startyear || (startMonth == 1 && startYear == startyear)) 	#we only extract full years and require data from the startyear on
			timeStartIndex <- (startyear - startYear) * 12 + 2 - startMonth #we extract beginning with January of startyear
		
			#account for missing months: assume all are at the end; e.g., precipitation of 'HadGEM2-ES' has values only until Nov 2099 instead Dec 2100
			timeCount_should <- (endyear - startyear + 1) * 12 #timeCount must include a count at timeStartIndex; to extract two values at 1:2, have timeStartIndex=1 and timeCount=2
			N_should <- timeStartIndex + timeCount_should - 1
			if (N >= N_should) {
				timeCount <- timeCount_should
				addMissingMonthAtEnd <- 0
			} else {
				timeCount <- N - timeStartIndex + 1
				addMissingMonthAtEnd <- N_should - N
			}
		
			list(timeStartIndex = timeStartIndex,
				timeCount = timeCount,
				addMissingMonthAtEnd = addMissingMonthAtEnd)
		})
	
		do_ncvar_get <- compiler::cmpfun(function(nc, nc_perm, variable, ncg, nct) {
			index <- which("time" == nc_perm)

			if (index == 3L) { #if file is in order of (lat, lon, time)
				ncvar_get(nc, variable, start=c(ncg$ix, ncg$iy, nct$timeStartIndex), count=c(1, 1, nct$timeCount))
			} else if (index == 1L) { #if file is optimized for time series extraction and permutated to order (time, lat, lon)
				ncvar_get(nc, variable, start=c(nct$timeStartIndex, ncg$ix, ncg$iy), count=c(nct$timeCount, 1, 1))
			} else {
				stop("do_ncvar_get: dimension 'time' must be either in first or third place, but is instead at ", index)
			}
		})
	
		mmPerDay_to_cmPerMonth <- compiler::cmpfun(function(prcp_mmPerDay, dpm) {
			prcp_mmPerDay / 10 * dpm
		})
	
		get.DBvariable <- compiler::cmpfun(function(filepath, variable, unit, ncg, nct, lon, lat, startyear, endyear) {
			# the 'raster' package (version <= '2.5.2') cannot handle non-equally spaced cells
			nc <- nc_open(filename = filepath, write = FALSE, readunlim = TRUE, verbose = FALSE)
			stopifnot(grepl(unit, nc$var[[variable]]$units, fixed = TRUE))
		
			# getting the values from the netCDF files...
			nc_perm <- sapply(nc$var[[variable]]$dim, function(x) x$name)
			res <- try(do_ncvar_get(nc, nc_perm, variable, ncg, nct), silent = TRUE)
			if (inherits(res, "try-error")) { #in case of 'HadGEM2-ES x RCP45' where pr and tasmax/tasmin have different timings
				ncg <- get.SpatialIndices(filename = filepath, lon, lat)
				nct <- get.TimeIndices(filename = filepath, startyear, endyear)
				res <- try(do_ncvar_get(nc, nc_perm, variable, ncg, nct), silent = TRUE)
			}
			nc_close(nc) #close the netCDF file
		
			#adjust for missing months
			if (nct$addMissingMonthAtEnd > 0) res <- c(res, rep(NA, times = nct$addMissingMonthAtEnd))
			
			#no data: most likely cell location is not terrestrial
			if (all(is.na(res))) stop("get.DBvariable: all data are NAs; cell location (", round(lon, 5), ", ", round(lat, 5), ") is likely not terrestrial")
			
			res
		})

	
		#' @return A list of one data.frame object with 5 columns and names of
		#' "year", "month", "tmax", "tmin", and "prcp". Each row is one day.
		#' Units are [degree Celsius] for temperature and [cm / day] and [cm / month], respectively, for precipitation.
		get_GCMdata <- compiler::cmpfun(function(i, ts_mons, dpm, gcm, scen, lon, lat, startyear, endyear, varTags, ...) {
			dots <- list(...) # fileVarTags, ncFiles, ncg, nct

			#Get precipitation data
			temp1 <- grepl(dots[["fileVarTags"]]["prcp"], dots[["ncFiles"]])
			if (any(temp1)) {
				prcp <- get.DBvariable(filepath = dots[["ncFiles"]][temp1][1], variable = varTags["prcp"], unit = "mm/d", ncg = dots[["ncg"]], nct = dots[["ncg"]], lon = lon, lat = lat, startyear = startyear, endyear = endyear)
				prcp <- mmPerDay_to_cmPerMonth(prcp, dpm)
			} else {
				stop("No suitable netCDF file with precipitation data found for ", i, gcm, scen, varTags["prcp"])
			}
			#Get temperature data
			temp3 <- grepl(dots[["fileVarTags"]]["tmin"], dots[["ncFiles"]])
			temp4 <- grepl(dots[["fileVarTags"]]["tmax"], dots[["ncFiles"]])
			if (any(temp3) && any(temp4)) {
				tmin <- get.DBvariable(filepath = dots[["ncFiles"]][temp3][1], variable = varTags["tmin"], unit = "C", ncg = dots[["ncg"]], nct = dots[["ncg"]], lon = lon, lat = lat, startyear = startyear, endyear = endyear)
				tmax <- get.DBvariable(filepath = dots[["ncFiles"]][temp4][1], variable = varTags["tmax"], unit = "C", ncg = dots[["ncg"]], nct = dots[["ncg"]], lon = lon, lat = lat, startyear = startyear, endyear = endyear)
			} else {
				temp2 <- grepl(dots[["fileVarTags"]]["tmean"], dots[["ncFiles"]])
				if (any(temp2)) {
					tmean <- get.DBvariable(filepath = dots[["ncFiles"]][temp2][1], variable = varTags["tmean"], unit = "C", ncg = dots[["ncg"]], nct = dots[["ncg"]], lon = lon, lat = lat, startyear = startyear, endyear = endyear)
					tmin <- tmax <- tmean
				} else {
					stop("No suitable netCDF file with temperature data found for ", i, gcm, scen, varTags["prcp"])
				}
			}
		
			list(cbind(year = ts_mons$year + 1900, month = ts_mons$mon + 1, tmax = tmax, tmin = tmin, prcp = prcp))
		})
	}
	
	#----Extraction function
	calc.ScenarioWeather <- compiler::cmpfun(function(i, GCM_source, is_GDODCPUCLLNL, is_NEX, gcmFiles, reqGCMs, reqRCPsPerGCM, reqDownscalingsPerGCM, climate.ambient, bbox, locations, dbW_iSiteTable, compression_type, varTags, fileVarTags, getYears, assocYears, future_yrs, simstartyr, endyr, DScur_startyr, DScur_endyr, downscaling.options, dir.out.temp, be.quiet, useRCurl, saveNEXtempfiles) { 
		#Identify index for site and scenario
		ig <- (i - 1) %% length(reqGCMs) + 1
		il <- (i - 1) %/% length(reqGCMs) + 1

		gcm <- reqGCMs[ig]
		rcps <- reqRCPsPerGCM[[ig]]
		downs <- reqDownscalingsPerGCM[[ig]]
		lon <- locations[il, "X_WGS84"]
		lat <- locations[il, "Y_WGS84"]
		site_id <- locations[il, "site_id"]
#		site_id <- dbW_iSiteTable[dbW_iSiteTable[, "Label"] == locations[il, "WeatherFolder"], "Site_id"]		
		ncFiles_gcm <- if (is_GDODCPUCLLNL) gcmFiles[grepl(paste0("_", as.character(gcm), "_"), gcmFiles)] else NULL
		if (!be.quiet) print(paste(i, "th extraction of '", GCM_source, "' at", Sys.time(), "for", gcm, "(", paste(rcps, collapse=", "), ") at", lon, lat))
		
		# Data Bounding Box
		if (!(lat >= bbox$lat[1] && lat <= bbox$lat[2] && lon >= bbox$lon[1] && lon <= bbox$lon[2])) {
			stop(paste(i, "th extraction of '", GCM_source, "' at", Sys.time(), "at", lon, lat, ": outside of bounding data box"))
		}

		#Scenario monthly weather time-series: Get GCM data for each scenario and time slice
		scen.monthly <- matrix(data=vector("list", length=(getYears$n_first+getYears$n_second)*(1 + length(rcps))), ncol=getYears$n_first+getYears$n_second, dimnames=list(c("Current", as.character(rcps)), c(paste0("first", 1:getYears$n_first), paste0("second", 1:getYears$n_second))))
		#First slice ('historical'): 1950-2005
		args_extract1 <- list(i = i, gcm = gcm, scen = "historical", lon = lon, lat = lat, varTags = varTags)
		if (is_GDODCPUCLLNL) {
			ncFiles <- ncFiles_gcm[grepl(args_extract1[["scen"]], ncFiles_gcm)]
			ncg <- get.SpatialIndices(filename = ncFiles[1], lon = lon, lat = lat)
		}
		if (is_GDODCPUCLLNL) {
			args_extract1 <- c(args_extract1, fileVarTags = list(fileVarTags), ncFiles = list(ncFiles), ncg = list(ncg))
		} else if (is_NEX) {
			args_extract1 <- c(args_extract1, bbox = list(bbox), dir.out.temp = dir.out.temp, useRCurl = useRCurl, saveNEXtempfiles = saveNEXtempfiles)
		}
		for (it in 1:getYears$n_first) {
			args_first <- c(args_extract1, 
								ts_mons = list(getYears$first_dates[[it]]),
								dpm = list(getYears$first_dpm[[it]]),
								startyear = getYears$first[it, 1],
								endyear = getYears$first[it, 2])
			if (is_GDODCPUCLLNL) {
				# Time index: differs among variables from the same GCMxRCP: in only once case: HadGEM2-ES x RCP45
				args_first <- c(args_first, nct = list(get.TimeIndices(filename = ncFiles[1], startyear = getYears$first[it, 1], endyear = getYears$first[it, 2])))
			}
			scen.monthly[1, it] <- do.call(get_GCMdata, args = args_first)
		}
		
		#Second slice ('future scenarios'): 2006-2099
		for (it in 1:getYears$n_second) {
			args_extract2 <- c(args_extract1, 
								ts_mons = list(getYears$second_dates[[it]]),
								dpm = list(getYears$second_dpm[[it]]),
								startyear = getYears$second[it, 1],
								endyear = getYears$second[it, 2])
			# Time index: differs among variables from the same GCMxRCP: in only once case: HadGEM2-ES x RCP45
			if (is_GDODCPUCLLNL) {
				args_extract2[["nct"]] <- get.TimeIndices(filename=ncFiles_gcm[grep(as.character(rcps)[1], ncFiles_gcm)[1]], startyear=getYears$second[it, 1], endyear=getYears$second[it, 2])
			}
			for (isc in 2:nrow(scen.monthly)) { 
				args_second <- args_extract2
				args_second[["scen"]] <- as.character(rcps[isc - 1])
				if (is_GDODCPUCLLNL) {
					args_second[["ncFiles"]] <- ncFiles_gcm[grepl(args_second[["scen"]], ncFiles_gcm)]
				}
				scen.monthly[isc, getYears$n_first + it] <- do.call(get_GCMdata, args = args_second)
			}
		}

	
		#Observed historic daily weather from weather database
		obs.hist.daily <- Rsoilwat31::dbW_getWeatherData(Site_id=site_id, startYear=simstartyr, endYear=endyr, Scenario=climate.ambient)
		if (obs.hist.daily[[1]]@year < 1950) { #TODO(drs): I don't know where the hard coded value of 1950 comes from; it doesn't make sense to me
			print("Note: subsetting years 'obs.hist.daily' because 'simstartyr < 1950'")
			start_yr <- obs.hist.daily[[length(obs.hist.daily)]]@year - 1950
			obs.hist.daily <- obs.hist.daily[(length(obs.hist.daily)-start_yr):length(obs.hist.daily)]
		}

		sim_years <- as.integer(names(obs.hist.daily))
		obs.hist.monthly <- dbW_weatherData_to_monthly(dailySW = obs.hist.daily)

		#Hamlet et al. 2010: "an arbitrary ceiling of 150% of the observed maximum precipitation value for each cell is also imposed by ???spreading out??? very large daily precipitation values into one or more adjacent days"
		dailyPPTceiling <- downscaling.options[["daily_ppt_limit"]] * max(unlist(lapply(obs.hist.daily, function(obs) max(obs@data[, "PPT_cm"]))))
		#Monthly extremes are used to cut the most extreme spline oscillations; these limits are ad hoc; monthly temperature extremes based on expanded daily extremes
		temp <- stretch_values(x = range(sapply(obs.hist.daily, function(obs) obs@data[, c("Tmax_C", "Tmin_C")])), lambda = downscaling.options[["monthly_limit"]])
		monthly_extremes <- list(Tmax = temp, Tmin = temp, PPT = c(0, downscaling.options[["monthly_limit"]] * max(aggregate(obs.hist.monthly[, "PPT_cm"], by=list(obs.hist.monthly[, 1]), FUN=sum)$x)))

	
		wdataOut <- list()
		for (ir in seq_along(rcps)) { #Apply downscaling for each RCP
			#Put historical data together
			#NOTE: both scen.hist.monthly and scen.fut.monthly may have NAs because some GCMs do not provide data for the last month of a time slice (e.g. December 2005 may be NA)
			if (!all(downs == "raw")) {
				scen.hist.monthly <- NULL
				for (itt in which(assocYears[["historical"]]$first)) scen.hist.monthly <- rbind(scen.hist.monthly, scen.monthly[1, itt][[1]])
				for (itt in which(assocYears[["historical"]]$second)) scen.hist.monthly <- rbind(scen.hist.monthly, scen.monthly[1 + ir, getYears$n_first + itt][[1]])
			}
		
			types <- list()
			for (it in 1:nrow(future_yrs)) {
				tag <- paste0(rownames(future_yrs)[it], ".", rcps[ir])
			
				#Put future data together
				scen.fut.monthly <- NULL
				for (itt in which(assocYears[[tag]]$first)) scen.fut.monthly <- rbind(scen.fut.monthly, scen.monthly[1, itt][[1]])
				for (itt in which(assocYears[[tag]]$second)) scen.fut.monthly <- rbind(scen.fut.monthly, scen.monthly[1 + ir, getYears$n_first + itt][[1]])
				
				# Comment: The variables are expected to cover the following time periods
				# 'obs.hist.daily' = simstartyr:endyr
				# 'obs.hist.monthly' = simstartyr:endyr
				# 'scen.hist.monthly' = DScur_startyr:DScur_endyr
				# 'scen.fut.monthly' = DSfut_startyr:DSfut_endyr
				# 'scen.fut.daily' will cover: delta + simstartyr:endyr
				# Units are [degree Celsius] for temperature and [cm / day] and [cm / month], respectively, for precipitation
				
				#Apply downscaling					
				if ("raw" %in% downs) {
					scenario_id <- dbW_iScenarioTable[dbW_iScenarioTable[, "Scenario"] == tolower(paste("raw", tag, gcm, sep=".")), "id"]
					scen.fut.daily <- downscale.raw(obs.hist.daily, obs.hist.monthly, scen.fut.monthly,
											years = sim_years,
											DScur_startyear = DScur_startyr, DScur_endyear = DScur_endyr,
											DSfut_startyear = future_yrs[it, "DSfut_startyr"], DSfut_endyear = future_yrs[it, "DSfut_endyr"],
											downscaling.options = downscaling.options,
											dailyPPTceiling = dailyPPTceiling,
											do_checks = TRUE)
					
					if (inherits(scen.fut.daily, "try-error")) {#raw unsuccessful, replace with raw without checks
						scen.fut.daily <- downscale.raw(obs.hist.daily, obs.hist.monthly, scen.fut.monthly,
												years = sim_years,
												DScur_startyear = DScur_startyr, DScur_endyear = DScur_endyr,
												DSfut_startyear = future_yrs[it, "DSfut_startyr"], DSfut_endyear = future_yrs[it, "DSfut_endyr"],
												downscaling.options = downscaling.options,
												dailyPPTceiling = dailyPPTceiling,
												do_checks = FALSE)
						stopifnot(!inherits(scen.fut.daily, "try-error"))
						print(paste0(i, ", site_id = ", site_id, ", scenario_id = ", scenario_id, ", ", tolower(paste(tag, gcm, sep=".")), ", timeslice = ", rownames(future_yrs)[it], ": raw method: checks turned off for monthly->daily"))
					}
					data_blob <- dbW_weatherData_to_blob(scen.fut.daily, compression_type)
					years <- as.integer(names(scen.fut.daily))
					types[[length(types)+1]] <- list(Site_id=site_id, Scenario_id=scenario_id, StartYear=years[1], EndYear=years[length(years)], weatherData=data_blob)
				}
				
				if ("delta" %in% downs) {
					scenario_id <- dbW_iScenarioTable[dbW_iScenarioTable[, "Scenario"] == tolower(paste("delta", tag, gcm, sep=".")), "id"]
					scen.fut.daily <- downscale.delta(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly,
											years = sim_years,
											DScur_startyear = DScur_startyr, DScur_endyear = DScur_endyr,
											DSfut_startyear = future_yrs[it, "DSfut_startyr"], DSfut_endyear = future_yrs[it, "DSfut_endyr"],
											downscaling.options = downscaling.options,
											dailyPPTceiling = dailyPPTceiling,
											do_checks = TRUE)
					if (inherits(scen.fut.daily, "try-error")) {#delta unsuccessful, replace with delta without checks
						scen.fut.daily <- downscale.delta(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly,
												years = sim_years,
												DScur_startyear = DScur_startyr, DScur_endyear = DScur_endyr,
												DSfut_startyear = future_yrs[it, "DSfut_startyr"], DSfut_endyear = future_yrs[it, "DSfut_endyr"],
												downscaling.options = downscaling.options,
												dailyPPTceiling = dailyPPTceiling,
												do_checks = FALSE)
						stopifnot(!inherits(scen.fut.daily, "try-error"))
						print(paste0(i, ", site_id = ", site_id, ", scenario_id = ", scenario_id, ", ", tolower(paste(tag, gcm, sep=".")), ", timeslice = ", rownames(future_yrs)[it], ": delta method: checks turned off for monthly->daily"))
					}
					data_blob <- dbW_weatherData_to_blob(scen.fut.daily, compression_type)
					years <- as.integer(names(scen.fut.daily))
					types[[length(types)+1]] <- list(Site_id=site_id, Scenario_id=scenario_id, StartYear=years[1], EndYear=years[length(years)], weatherData=data_blob)
				}
				
				if ("hybrid-delta" %in% downs) {
					scenario_id <- dbW_iScenarioTable[dbW_iScenarioTable[, "Scenario"] == tolower(paste("hybrid-delta", tag, gcm, sep=".")), "id"]
					scen.fut.daily <- downscale.deltahybrid(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly,
											years = sim_years,
											DScur_startyear = DScur_startyr, DScur_endyear = DScur_endyr,
											DSfut_startyear = future_yrs[it, "DSfut_startyr"], DSfut_endyear = future_yrs[it, "DSfut_endyr"],
											downscaling.options = downscaling.options,
											do_checks = TRUE)
					if (inherits(scen.fut.daily, "try-error")) {#delta-hybrid unsuccessful, replace with delta method
						scen.fut.daily <- downscale.delta(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly,
												years = sim_years,
												DScur_startyear = DScur_startyr, DScur_endyear = DScur_endyr,
												DSfut_startyear = future_yrs[it, "DSfut_startyr"], DSfut_endyear = future_yrs[it, "DSfut_endyr"],
												downscaling.options = downscaling.options,
												dailyPPTceiling = dailyPPTceiling,
												do_checks = FALSE)
						stopifnot(!inherits(scen.fut.daily, "try-error"))
						print(paste0(i, ", site_id = ", site_id, ", scenario_id = ", scenario_id, ", ", tolower(paste(tag, gcm, sep=".")), ", timeslice = ", rownames(future_yrs)[it], ": delta-hybrid replaced by delta method for monthly->daily"))
					}
					data_blob <- dbW_weatherData_to_blob(scen.fut.daily, compression_type)
					years <- as.integer(names(scen.fut.daily))
					types[[length(types)+1]] <- list(Site_id=site_id, Scenario_id=scenario_id, StartYear=years[1], EndYear=years[length(years)], weatherData=data_blob)
				}
				
				if ("hybrid-delta-3mod" %in% downs) {
					scenario_id <- dbW_iScenarioTable[dbW_iScenarioTable[, "Scenario"] == tolower(paste("hybrid-delta-3mod", tag, gcm, sep=".")), "id"]                          

					scen.fut.daily <- downscale.deltahybrid3mod(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly,
											deltaFuture_yr = future_yrs[it, "delta"], years = sim_years,
											DScur_startyear = DScur_startyr, DScur_endyear = DScur_endyr,
											DSfut_startyear = future_yrs[it, "DSfut_startyr"], DSfut_endyear = future_yrs[it, "DSfut_endyr"],
											downscaling.options = downscaling.options,
											dailyPPTceiling = dailyPPTceiling,
											monthly_extremes = monthly_extremes,
											do_checks = TRUE)
					
					if (inherits(scen.fut.daily, "try-error")) {#delta-hybrid-3mod unsuccessful, replace with delta method
						scen.fut.daily <- downscale.delta(obs.hist.daily, obs.hist.monthly, scen.hist.monthly, scen.fut.monthly,
												years = sim_years,
												DScur_startyear = DScur_startyr, DScur_endyear = DScur_endyr,
												DSfut_startyear = future_yrs[it, "DSfut_startyr"], DSfut_endyear = future_yrs[it, "DSfut_endyr"],
												downscaling.options = downscaling.options,
												dailyPPTceiling = dailyPPTceiling,
												do_checks = FALSE)
						stopifnot(!inherits(scen.fut.daily, "try-error"))
						print(paste0(i, ", site_id = ", site_id, ", scenario_id = ", scenario_id, ", ", tolower(paste(tag, gcm, sep=".")), ", timeslice = ", rownames(future_yrs)[it], ": delta-hybrid-3mod replaced by delta method for monthly->daily"))
					}
					data_blob <- dbW_weatherData_to_blob(scen.fut.daily, compression_type)
					years <- as.integer(names(scen.fut.daily))
					types[[length(types)+1]] <- list(Site_id=site_id, Scenario_id=scenario_id, StartYear=years[1], EndYear=years[length(years)], weatherData=data_blob)
				}
			}
			wdataOut[[ir]] <- types
		}
		saveRDS(wdataOut, file=file.path(dir.out.temp, gcm, paste(GCM_source, "_", i, ".rds", sep="")))
		res <- i
	
		res
	})

	#' Make daily weather for a scenario
	#'
	#' A wrapper function for \code{calc.ScenarioWeather} with error control.
	#'
	#' @inheritParams calc.ScenarioWeather
	try.ScenarioWeather <- compiler::cmpfun(function(i, GCM_source, is_GDODCPUCLLNL, is_NEX, gcmFiles, reqGCMs, reqRCPsPerGCM, reqDownscalingsPerGCM, climate.ambient, bbox, locations, dbW_iSiteTable, compression_type, varTags, fileVarTags, getYears, assocYears, future_yrs, simstartyr, endyr, DScur_startyr, DScur_endyr, downscaling.options, dir.out.temp, be.quiet, useRCurl, saveNEXtempfiles) {
		temp <- try(calc.ScenarioWeather(i = i,
						GCM_source = GCM_source, is_GDODCPUCLLNL = is_GDODCPUCLLNL, is_NEX = is_NEX,
						gcmFiles = gcmFiles,
						reqGCMs = reqGCMs, reqRCPsPerGCM = reqRCPsPerGCM, reqDownscalingsPerGCM = reqDownscalingsPerGCM,
						climate.ambient = climate.ambient,
						bbox = bbox,
						locations = locations,
						dbW_iSiteTable = dbW_iSiteTable,
						compression_type = compression_type,
						varTags = varTags, fileVarTags = fileVarTags,
						getYears = getYears, assocYears = assocYears, future_yrs = future_yrs,
						simstartyr = simstartyr, endyr = endyr,
						DScur_startyr = DScur_startyr, DScur_endyr = DScur_endyr,
						downscaling.options = downscaling.options,
						dir.out.temp = dir.out.temp,
						be.quiet = be.quiet,
						useRCurl = useRCurl, saveNEXtempfiles = saveNEXtempfiles),
					silent = FALSE)
		
		if (inherits(temp, "try-error")) {
			print(paste(Sys.time(), temp))
			save(i, temp, file = file.path(dir.out.temp, paste0("failed_", i, ".RData")))
			res <- NULL
		} else {
			res <- i
		}

		res
	})

	#' Organizes the calls (in parallel) which obtain specified scenario weather for the weather database from one of the available GCM sources
	#'
	#' This function assumes that a whole bunch of global variables exist and contain appropriate values.
	tryToGet_ClimDB <- compiler::cmpfun(function(is_ToDo, list.export, GCM_source, is_GDODCPUCLLNL, is_NEX, gcmFiles, reqGCMs, reqRCPsPerGCM, reqDownscalingsPerGCM, bbox, locations, varTags, fileVarTags, getYears, assocYears, saveNEXtempfiles) {
		#requests is_ToDo: fastest if nc file is
		#	- DONE: permutated to (lat, lon, time) instead (time, lat, lon)
		#	- TODO: many sites are extracted from one nc-read instead of one site per nc-read (see benchmarking_GDODCPUCLLNL_extractions.R)
		#TODO: create chunks for is_ToDo of size sites_per_chunk_N that use the same access to a nc file and distribute among workersN

		if (parallel_runs && parallel_init) {
			is_ToDo <- sample(x=is_ToDo, size=length(is_ToDo)) #attempt to prevent reading from same .nc at the same time

			# extract the GCM data depending on parallel backend
			if (identical(parallel_backend, "mpi")) {
				exportObjects(list.export)
				if (is_NEX && useRCurl && !saveNEXtempfiles)
					Rmpi::mpi.bcast.cmd(library("RCurl", quietly=TRUE))
				if (is_GDODCPUCLLNL)
					Rmpi::mpi.bcast.cmd(library("ncdf4", quietly=TRUE))
				Rmpi::mpi.bcast.cmd(library("Rsoilwat31", quietly=TRUE))
				Rmpi::mpi.bcast.cmd(Rsoilwat31::dbW_setConnection(dbFilePath=dbWeatherDataFile))
			
				i_Done <- Rmpi::mpi.applyLB(x = is_ToDo, fun = try.ScenarioWeather,
						GCM_source = GCM_source, is_GDODCPUCLLNL = is_GDODCPUCLLNL, is_NEX = is_NEX,
						gcmFiles = gcmFiles,
						reqGCMs = reqGCMs, reqRCPsPerGCM = reqRCPsPerGCM, reqDownscalingsPerGCM = reqDownscalingsPerGCM,
						climate.ambient = climate.ambient,
						bbox = bbox,
						locations = locations,
						dbW_iSiteTable = dbW_iSiteTable,
						compression_type = dbW_compression_type,
						varTags = varTags, fileVarTags = fileVarTags,
						getYears = getYears, assocYears = assocYears, future_yrs = future_yrs,
						simstartyr = simstartyr, endyr = endyr,
						DScur_startyr = DScur_startyr, DScur_endyr = DScur_endyr,
						downscaling.options = downscaling.options,
						dir.out.temp = dir.out.temp,
						be.quiet = be.quiet,
						useRCurl = useRCurl, saveNEXtempfiles = saveNEXtempfiles)
			
				Rmpi::mpi.bcast.cmd(Rsoilwat31::dbW_disconnectConnection())
				Rmpi::mpi.bcast.cmd(rm(list=ls()))
				Rmpi::mpi.bcast.cmd(gc())
				
			} else if (identical(parallel_backend, "snow")) {
				snow::clusterExport(cl, list.export, envir=parent.frame())
				if (is_NEX && useRCurl && !saveNEXtempfiles)
					snow::clusterEvalQ(cl, library("RCurl", quietly = TRUE))
				if (is_GDODCPUCLLNL)
					snow::clusterEvalQ(cl, library("ncdf4", quietly=TRUE))
				snow::clusterEvalQ(cl, library("Rsoilwat31", quietly=TRUE))
				snow::clusterEvalQ(cl, Rsoilwat31::dbW_setConnection(dbFilePath=dbWeatherDataFile))
			
				i_Done <- snow::clusterApplyLB(cl, x = is_ToDo, fun = try.ScenarioWeather,
						GCM_source = GCM_source, is_GDODCPUCLLNL = is_GDODCPUCLLNL, is_NEX = is_NEX,
						gcmFiles = gcmFiles,
						reqGCMs = reqGCMs, reqRCPsPerGCM = reqRCPsPerGCM, reqDownscalingsPerGCM = reqDownscalingsPerGCM,
						climate.ambient = climate.ambient,
						bbox = bbox,
						locations = locations,
						dbW_iSiteTable = dbW_iSiteTable,
						compression_type = dbW_compression_type,
						varTags = varTags, fileVarTags = fileVarTags,
						getYears = getYears, assocYears = assocYears, future_yrs = future_yrs,
						simstartyr = simstartyr, endyr = endyr,
						DScur_startyr = DScur_startyr, DScur_endyr = DScur_endyr,
						downscaling.options = downscaling.options,
						dir.out.temp = dir.out.temp,
						be.quiet = be.quiet,
						useRCurl = useRCurl, saveNEXtempfiles = saveNEXtempfiles)
			
				snow::clusterEvalQ(cl, Rsoilwat31::dbW_disconnectConnection())
				snow::clusterEvalQ(cl, rm(list=ls()))
				snow::clusterEvalQ(cl, gc())
				
			} else if (identical(parallel_backend, "multicore")) {
				packages.export <- "Rsoilwat31"
				if (is_NEX && useRCurl && !saveNEXtempfiles)
					packages.export <- c(packages.export, "RCurl")
				if (is_GDODCPUCLLNL)
					packages.export <- c(packages.export, "ncdf4")
				i_Done <- foreach(i=is_ToDo, .combine="c", .errorhandling="remove", .inorder=FALSE, .export=list.export, .packages=packages.export) %dopar% {
					Rsoilwat31::dbW_setConnection(dbFilePath=dbWeatherDataFile)
					temp <- try.ScenarioWeather(i,
						GCM_source = GCM_source, is_GDODCPUCLLNL = is_GDODCPUCLLNL, is_NEX = is_NEX,
						gcmFiles = gcmFiles,
						reqGCMs = reqGCMs, reqRCPsPerGCM = reqRCPsPerGCM, reqDownscalingsPerGCM = reqDownscalingsPerGCM,
						climate.ambient = climate.ambient,
						bbox = bbox,
						locations = locations,
						dbW_iSiteTable = dbW_iSiteTable,
						compression_type = dbW_compression_type,
						varTags = varTags, fileVarTags = fileVarTags,
						getYears = getYears, assocYears = assocYears, future_yrs = future_yrs,
						simstartyr = simstartyr, endyr = endyr,
						DScur_startyr = DScur_startyr, DScur_endyr = DScur_endyr,
						downscaling.options = downscaling.options,
						dir.out.temp = dir.out.temp,
						be.quiet = be.quiet,
						useRCurl = useRCurl, saveNEXtempfiles = saveNEXtempfiles)
					Rsoilwat31::dbW_disconnectConnection()
					return(temp)
				}
			} else {
				i_Done <- NULL
			}
			
		} else {
			Rsoilwat31::dbW_setConnection(dbFilePath=dbWeatherDataFile)
			i_Done <- foreach(i=is_ToDo, .combine="c", .errorhandling="remove", .inorder=FALSE) %do% try.ScenarioWeather(i,
						GCM_source = GCM_source, is_GDODCPUCLLNL = is_GDODCPUCLLNL, is_NEX = is_NEX,
						gcmFiles = gcmFiles,
						reqGCMs = reqGCMs, reqRCPsPerGCM = reqRCPsPerGCM, reqDownscalingsPerGCM = reqDownscalingsPerGCM,
						climate.ambient = climate.ambient,
						bbox = bbox,
						locations = locations,
						dbW_iSiteTable = dbW_iSiteTable,
						compression_type = dbW_compression_type,
						varTags = varTags, fileVarTags = fileVarTags,
						getYears = getYears, assocYears = assocYears, future_yrs = future_yrs,
						simstartyr = simstartyr, endyr = endyr,
						DScur_startyr = DScur_startyr, DScur_endyr = DScur_endyr,
						downscaling.options = downscaling.options,
						dir.out.temp = dir.out.temp,
						be.quiet = be.quiet,
						useRCurl = useRCurl, saveNEXtempfiles = saveNEXtempfiles)
			Rsoilwat31::dbW_disconnectConnection()
		}
	

		if (!be.quiet) print(paste("Started adding temporary files into database '", GCM_source, "' at", Sys.time()))
		Rsoilwat31::dbW_setConnection(dbFilePath=dbWeatherDataFile)
		temp.files <- list.files(path=dir.out.temp, pattern=GCM_source, recursive=TRUE, include.dirs=FALSE, no..=TRUE)
		if (length(temp.files) > 0) {
			for (k in seq_along(temp.files)) {
				ftemp <- file.path(dir.out.temp, temp.files[k])
				wdataOut <- readRDS(file = ftemp)
				
				for (j in seq_along(wdataOut)) {
					for (l in seq_along(wdataOut[[j]])) {
						res <- try(Rsoilwat31:::dbW_addWeatherDataNoCheck(
									Site_id = 		wdataOut[[j]][[l]]$Site_id,
									Scenario_id =	wdataOut[[j]][[l]]$Scenario_id,
									StartYear = 	wdataOut[[j]][[l]]$StartYear,
									EndYear = 		wdataOut[[j]][[l]]$EndYear,
									weather_blob = 	wdataOut[[j]][[l]]$weatherData),
								silent=TRUE)
						if (inherits(res, "try-error")) {
							if (!be.quiet) print(paste("Adding downscaled data for Site_id", wdataOut[[j]][[l]]$Site_id, "scenario", wdataOut[[j]][[l]]$Scenario_id, "was unsuccessful:", temp))
							break
						}
					}
					if (inherits(res, "try-error")) break
				}
				if (!inherits(res, "try-error")) unlink(ftemp)
			}
		}
		Rsoilwat31::dbW_disconnectConnection()
	
		sort(unlist(i_Done))
	})


}


#--------------------------------------------------------------------------------------------------#
#------EXTRACT CLIMATE CHANGE DATA------
# Allow for multiple data sources
#	- among sites but not multiple sources per site (for that you need a new row in the MasterInput spreadsheet)
#	- NEX USA data has highest priority, then LLNL USA products, then global LLNL products

if (exinfo$GDODCPUCLLNL || exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA) {

	xy <- with(SWRunInformation[runIDs_sites,], data.frame(X_WGS84, Y_WGS84))
	i_use <- rep(FALSE, times = runsN_sites)

	if (extract_determine_database == "SWRunInformation" && "GCM_sources" %in% colnames(SWRunInformation)) {
		sites_GCM_source <- SWRunInformation$GCM_sources[runIDs_sites]

	} else if (extract_determine_database == "order" || !("GCM_sources" %in% colnames(SWRunInformation))) {
		sites_GCM_source <- rep(NA, times = runsN_sites)

		# determine which data product to use for each site based on bounding boxes of datasets
		if (exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_NEX_USA) {
			i_use <- in_GMC_box(xy = xy, lats=c(24.0625, 49.9375), longs=c(-125.02083333, -66.47916667), i_use=i_use)
			sites_GCM_source[i_use] <- "CMIP5_BCSD_NEX_USA"
		}
		if (exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_USA) {
			i_use <- in_GMC_box(xy = xy, lats=c(25.125, 52.875), longs=c(-124.625, -67), i_use=i_use)
			sites_GCM_source[i_use] <- "CMIP3_BCSD_GDODCPUCLLNL_USA"
		}
		if (exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_USA) {
			i_use <- in_GMC_box(xy = xy, lats=c(25.125, 52.875), longs=c(-124.625, -67), i_use=i_use)
			sites_GCM_source[i_use] <- "CMIP5_BCSD_GDODCPUCLLNL_USA"
		}
		if (exinfo$ExtractClimateChangeScenarios_CMIP3_BCSD_GDODCPUCLLNL_Global) {
			i_use <- in_GMC_box(xy = xy, lats=c(-55.25-0.25, 83.25+0.25), longs=c(-179.75-0.25, 179.75+0.25), i_use=i_use)
			sites_GCM_source[i_use] <- "CMIP3_BCSD_GDODCPUCLLNL_Global"
		}
		if (exinfo$ExtractClimateChangeScenarios_CMIP5_BCSD_GDODCPUCLLNL_Global) {
			i_use <- in_GMC_box(xy = xy, lats=c(-55.25-0.25, 83.25+0.25), longs=c(-179.75-0.25, 179.75+0.25), i_use=i_use)
			sites_GCM_source[i_use] <- "CMIP5_BCSD_GDODCPUCLLNL_Global"
		}
	
		#write data to datafile.SWRunInformation
		SWRunInformation$GCM_sources[runIDs_sites] <- as.character(sites_GCM_source)
		write.csv(SWRunInformation, file=file.path(dir.in, datafile.SWRunInformation), row.names=FALSE)
		unlink(file.path(dir.in, datafile.SWRWinputs_preprocessed))

	} else {
		stop(paste("Value of 'extract_determine_database'", extract_determine_database, "not implemented"))
	}

	if (anyNA(sites_GCM_source)) print("No climate change data available for ", sum(is.na(sites_GCM_source)), " sites")

	#access climate change data
	get_climatechange_data <- compiler::cmpfun(function(GCM_source, is_GDODCPUCLLNL, is_NEX, do_SWRun_sites, include_YN_climscen) {
	#	GCM_source <- if (GCM_source == "CMIP5_BCSD_NEX_USA") {
	#					"CMIP5-BCSD-NEX-USA"
	#				} else if (grepl("CMIP3_BCSD_GDODCPUCLLNL", GCM_source)) {
	#					"CMIP3-BCSD-GDO-DCP-UC-LLNL"
	#				} else if (grepl("CMIP5_BCSD_GDODCPUCLLNL", GCM_source)) {
	#					"CMIP5-BCSD-GDO-DCP-UC-LLNL"
	#				} else {
	#					stop("No dataset for climate change conditions set")
	#				}
		if (!be.quiet) print(paste0("Started '", GCM_source, "' at ", Sys.time()))
	
		#Global flags
		repeatExtractionLoops_maxN <- 3
		#Specific flags
		if (is_GDODCPUCLLNL) {
			##gdo-dcp.ucllnl.org/downscaled_cmip_projections
			dir.ex.dat <- file.path(dir.ex.fut, "GDO_DCP_UCLLNL_DownscaledClimateData")
			if (GCM_source == "CMIP3_BCSD_GDODCPUCLLNL_USA") dir.ex.dat <- file.path(dir.ex.dat, "CMIP3_BCSD", "CONUS_0.125degree")
			if (GCM_source == "CMIP3_BCSD_GDODCPUCLLNL_Global") dir.ex.dat <- file.path(dir.ex.dat, "CMIP3_BCSD", "Global_0.5degree_MaurerEd")
			if (GCM_source == "CMIP5_BCSD_GDODCPUCLLNL_USA") dir.ex.dat <- file.path(dir.ex.dat, "CMIP5_BCSD", "CONUS_0.125degree_r1i1p1")
			if (GCM_source == "CMIP5_BCSD_GDODCPUCLLNL_Global") dir.ex.dat <- file.path(dir.ex.dat, "CMIP5_BCSD", "Global_0.5degree_r1i1p1")
		
			#CMIP3 Global and USA
			#	- obs: 1950 Jan to 1999 Dec
			#	- SRES: 1950 Jan to 2099 Dec
			#	- all same time + spatial coordinates
			#CMIP5 Global and USA
			#	- historical: 1950 Jan to 2005 Dec (except: HadGEM2-CC and HadGEM2-ES, to 2005 Nov)
			#	- RCPs:
			#		- in general: 2006 Jan to 2099 Dec or 2100 Dec
			#		- HadGEM2-CC and HadGEM2-ES: 2005 Dec to 2099 Dec
			#		- RCP45 & Globus: HadGEM2-ES: 2005 Dec to 2099 Nov
			#		- RCP45: HadCM2 and MIROC4h: 2006 Jan to 2035 Dec
			#		- no RCP85: GISS-E2-H-CC, GISS-E2-R-CC
			#	=> ignore missing Dec value; ignore 2005 Dec value if that is the start
			#	- all same spatial coordinates
		
			scenariosDB <- list.dirs(dir.ex.dat, full.names=FALSE, recursive=FALSE)
			if (any((temp <- sapply(scenariosDB, FUN=function(x) length(list.files(file.path(dir.ex.dat, x))))) == 0)) scenariosDB <- scenariosDB[temp > 0]
		
			gcmsDB <- unique(unlist(sapply(scenariosDB, FUN=function(x) sapply(strsplit(list.files(file.path(dir.ex.dat, x)), split="_", fixed=TRUE), FUN=function(x) x[5]))))	
			gcmFiles <- list.files(dir.ex.dat, recursive=TRUE, full.names=TRUE)
		
			print_int <- 100
			saveNEXtempfiles <- FALSE
		}
		if (is_NEX) {
			##https://portal.nccs.nasa.gov/portal_home/published/NEX.html
			opt <- options("timeout")
			options(timeout=5*60)

			dir.ex.dat <- NULL
			if (useRCurl) {
				stopifnot(RCurl::url.exists("https://portal.nccs.nasa.gov/portal_home/published/NEX.html")) #check whether we are online
				saveNEXtempfiles <- FALSE
			} else {
				saveNEXtempfiles <- TRUE
			}

			gcmsDB <- c("inmcm4", "bcc-csm1-1", "bcc-csm1-1-m", "NorESM1-M", "MRI-CGCM3", "MPI-ESM-MR", "MPI-ESM-LR", "MIROC5", "MIROC-ESM", "MIROC-ESM-CHEM", "IPSL-CM5B-LR", "IPSL-CM5A-MR", "IPSL-CM5A-LR", "HadGEM2-ES", "HadGEM2-CC", "HadGEM2-AO", "GISS-E2-R", "GFDL-ESM2M", "GFDL-ESM2G", "GFDL-CM3", "FIO-ESM", "FGOALS-g2", "CanESM2", "CSIRO-Mk3-6-0", "CNRM-CM5", "CMCC-CM", "CESM1-CAM5", "CESM1-BGC", "CCSM4", "BNU-ESM", "ACCESS1-0")
			scenariosDB <- c("historical", "rcp26", "rcp45", "rcp60", "rcp85")
			gcmFiles <- NULL
			
			print_int <- 1
		}
	
		#Force dataset specific lower/uper case for GCMs and RCPs
		reqGCMs <- gcmsDB[match(tolower(reqGCMs), tolower(gcmsDB), nomatch=NA)]
		reqRCPs <- scenariosDB[match(tolower(reqRCPs), tolower(scenariosDB), nomatch=NA)]
		reqRCPsPerGCM <- lapply(reqRCPsPerGCM, FUN=function(r) scenariosDB[match(tolower(r), tolower(scenariosDB), nomatch=NA)])
	
		#Tests that all requested conditions will be extracted
		stopifnot(length(reqGCMs) > 0, all(!is.na(reqGCMs)))
		stopifnot(length(reqRCPs) > 0, all(!is.na(reqRCPs)), any(grepl("historical", scenariosDB, ignore.case=TRUE)))

		#put requests together
		locations <- SWRunInformation[do_SWRun_sites, c("X_WGS84", "Y_WGS84", "site_id", "WeatherFolder")]	#locations of simulation runs
		requestN <- length(reqGCMs) * nrow(locations)
		if (!be.quiet) print(paste("'", GCM_source, "' will run", requestN, "times"))
	
		#bounding box
		bbox <- data.frame(matrix(NA, nrow=2, ncol=2, dimnames=list(NULL, c("lat", "lon"))))
		if (is_NEX) {
			bbox$lat <- c(24.0625, 49.9375)
			bbox$lon <- c(-125.02083333, -66.47916667)
		}
		if (GCM_source == "CMIP3_BCSD_GDODCPUCLLNL_USA" || GCM_source == "CMIP5_BCSD_GDODCPUCLLNL_USA") {
			bbox$lat <- c(25.125, 52.875)
			bbox$lon <- c(-124.625, -67)
		}
		if (GCM_source == "CMIP3_BCSD_GDODCPUCLLNL_Global" || GCM_source == "CMIP5_BCSD_GDODCPUCLLNL_Global") {
			bbox$lat <- c(-55.25-0.25, 83.25+0.25)
			bbox$lon <- c(-179.75-0.25, 179.75+0.25)
		}
	
		#time box
		tbox <- data.frame(matrix(NA, nrow=2, ncol=2, dimnames=list(c("start", "end"), c("first", "second"))))
		if (is_NEX || GCM_source == "CMIP5_BCSD_GDODCPUCLLNL_USA" || GCM_source == "CMIP5_BCSD_GDODCPUCLLNL_Global") {
			tbox$first <- c(1950, 2005)
			tbox$second <- c(2006, 2099)
		}
		if (GCM_source == "CMIP3_BCSD_GDODCPUCLLNL_USA" || GCM_source == "CMIP3_BCSD_GDODCPUCLLNL_Global") {
			tbox$first <- c(1950, 1999)
			tbox$second <- c(2000, 2099)
		}
		#timing: time slices: data is organized into 'historical' runs 1950-2005 (="first") and future 'rcp' runs 2006-2099 (="second")
		timeSlices <- data.frame(matrix(NA, ncol=4, nrow = 4 + 4 * nrow(future_yrs), dimnames = list(NULL, c("Run", "Slice", "Time", "Year"))))
		timeSlices[, 1:3] <- expand.grid(c("start", "end"), c("first", "second"), c("historical", rownames(future_yrs)))[, 3:1]
		#historic conditions for downscaling
		timeSlices[1, 4] <- max(tbox$first[1], DScur_startyr)
		timeSlices[2, 4] <- min(tbox$first[2], DScur_endyr)
		if (DScur_endyr > tbox$first[2]) {
			timeSlices[3, 4] <- tbox$second[1]
			timeSlices[4, 4] <- min(tbox$second[2], DScur_endyr)
		}
		#future conditions for downscaling
		for (it in 1:nrow(future_yrs)) {
			timeSlices[3 + 4*it, 4] <- max(tbox$second[1], future_yrs[it, "DSfut_startyr"])
			timeSlices[4 + 4*it, 4] <- min(tbox$second[2], future_yrs[it, "DSfut_endyr"])	#limits timeSlices to 2099
			if (DScur_startyr < 1950) { #TODO(drs): I don't know where the hard coded value of 1950 comes from; it doesn't make sense to me
			  print("Note: adjustment to 'timeSlices' because 'DScur_startyr < 1950'")
			  timeSlices[4 + 4*it, 4] <- min(timeSlices[4 + 4*it, 4], timeSlices[4 + 3*it, 4]+(timeSlices[4,4]-timeSlices[1,4]))
			}
			if (future_yrs[it, "DSfut_startyr"] < tbox$second[1]) {
				timeSlices[1 + 4*it, 4] <- max(tbox$first[1], future_yrs[it, "DSfut_startyr"])
				timeSlices[2 + 4*it, 4] <- tbox$second[1]
			}
		}
		#get unique time slices
		temp1 <- unique_times(timeSlices, slice = "first")
		temp2 <- unique_times(timeSlices, slice = "second")
		getYears <- list(n_first = nrow(temp1), first = temp1, n_second = nrow(temp2), second = temp2)
		
		#Monthly time-series
		getYears$first_dates <- lapply(1:getYears$n_first, FUN=function(it) as.POSIXlt(seq(from=as.POSIXlt(paste0(getYears$first[it, 1], "-01-01")), to=as.POSIXlt(paste0(getYears$first[it, 2], "-12-31")), by="1 month")))
		getYears$second_dates <- lapply(1:getYears$n_second, FUN=function(it) as.POSIXlt(seq(from=as.POSIXlt(paste0(getYears$second[it, 1], "-01-01")), to=as.POSIXlt(paste0(getYears$second[it, 2], "-12-31")), by="1 month")))
		#Days per month
		getYears$first_dpm <- lapply(1:getYears$n_first, FUN=function(it) rle(as.POSIXlt(seq(from=as.POSIXlt(paste0(getYears$first[it, 1], "-01-01")), to=as.POSIXlt(paste0(getYears$first[it, 2], "-12-31")), by="1 day"))$mon)$lengths)
		getYears$second_dpm <- lapply(1:getYears$n_second, FUN=function(it) rle(as.POSIXlt(seq(from=as.POSIXlt(paste0(getYears$second[it, 1], "-01-01")), to=as.POSIXlt(paste0(getYears$second[it, 2], "-12-31")), by="1 day"))$mon)$lengths)	

		#Logical on how to select from getYears
		assocYears <- vector("list", length = 1 + length(reqRCPs) * nrow(future_yrs))
		names_assocYears <- c("historical", paste0(rownames(future_yrs), ".", rep(reqRCPs, each = nrow(future_yrs))))
		for (it in seq_along(assocYears)) {
			temp <- strsplit(names_assocYears[it], ".", fixed=TRUE)[[1]][[1]]
			assocYears[[it]] <- list(first = useSlices(getYears, timeSlices, run = temp, slice = "first"),
									second = useSlices(getYears, timeSlices, run = temp, slice = "second"))
		}
		names(assocYears) <- names_assocYears
		
		print(paste("Future scenario data will be extracted for a time period spanning ", timeSlices[7,4], "through",  max(na.omit(timeSlices[,4]))))
		#Variable tags
		if (is_NEX) {
			varTags <- c(prcp = "pr", tmin = "tasmin", tmax = "tasmax") #units c("kg/m2/s", "K", "K") --> SoilWat required units c("cm/day", "C", "C")
			fileVarTags <- NULL
		}
		if (GCM_source == "CMIP3_BCSD_GDODCPUCLLNL_USA" || GCM_source == "CMIP3_BCSD_GDODCPUCLLNL_Global") {
			varTags <- c(prcp = "Prcp", tmean = "Tavg", tmin = "Tmin", tmax = "Tmax")
			fileVarTags <- paste("monthly", varTags, sep=".")
			names(fileVarTags) <- names(varTags)
		}
		if (GCM_source == "CMIP5_BCSD_GDODCPUCLLNL_USA" || GCM_source == "CMIP5_BCSD_GDODCPUCLLNL_Global") {
			varTags <- c(prcp = "pr", tmean = "tas", tmin = "tasmin", tmax = "tasmax")
			fileVarTags <- paste0("_", varTags, "_")
			names(fileVarTags) <- names(varTags)
		}

	
		#objects that need exporting to workers
		list.export <- c("dir.out.temp", "GCM_source", "is_GDODCPUCLLNL", "is_NEX", "reqGCMs", "reqRCPsPerGCM", "reqDownscalingsPerGCM", "locations", "climScen", "varTags", "be.quiet", "getYears", "assocYears", "future_yrs", "DScur_startyr", "DScur_endyr", "simstartyr", "endyr", "dbWeatherDataFile", "climate.ambient", "dbW_iSiteTable", "dbW_iScenarioTable", "dbW_compression_type", "bbox", "print_int",
				"calc.ScenarioWeather", "get_GCMdata", "get.DBvariable",
				"downscale.raw", "downscale.delta", "downscale.deltahybrid", "downscale.deltahybrid3mod", "downscale.periods",
				"in_GMC_box", "unique_times", "useSlices", "erf", "add_delta_to_PPT", "fix_PPTdata_length", "calc_Days_withLoweredPPT", "controlExtremePPTevents", "test_sigmaNormal", "test_sigmaGamma", "stretch_values", 
				"applyDeltas", "applyPPTdelta_simple", "applyPPTdelta_detailed", "applyDelta_oneYear", "applyDeltas2",
				"doQmapQUANT.default_drs", "tol", "doQmapQUANT_drs", "applyDeltas2", "downscaling.options")
		if (is_NEX) {
			list.export <- c(list.export, "saveNEXtempfiles", "useRCurl",  
				"mmPerSecond_to_cmPerMonth", "get.request")
		}
		if (is_GDODCPUCLLNL) {
			list.export <- c(list.export, "fileVarTags", "gcmFiles",  
				"mmPerDay_to_cmPerMonth", "whereNearest", "get.TimeIndices", "get.SpatialIndices", "do_ncvar_get")
		}


		#Repeat call to get climate data for all requests until complete
		repeatN <- 0
		i_AllToDo <- seq_len(requestN)
		i_Done <- NULL
	
		logFile <- file.path(dir.out, paste0("extractionsDone_", GCM_source, ".rds"))
		if (file.exists(logFile)) {
			i_Done <- sort(unique(c(i_Done, readRDS(file=logFile))))
		}
		temp.files <- list.files(path=dir.out.temp, pattern=GCM_source, recursive=TRUE, include.dirs=FALSE, no..=TRUE)
		if (length(temp.files) > 0) {
			i_Done <- sort(unique(c(i_Done, as.integer(unlist(strsplit(unlist(strsplit(temp.files, split="_", fixed=TRUE))[c(FALSE, TRUE)], split=".", fixed=TRUE))[c(TRUE, FALSE)]))))
		}
		
		while (repeatExtractionLoops_maxN > repeatN && length(i_ToDo <- if (length(i_Done) > 0) i_AllToDo[-i_Done] else i_AllToDo) > 0) {
			repeatN <- repeatN + 1
			if (!be.quiet) print(paste("'", GCM_source, "' will run the", repeatN, ". time to extract", length(i_ToDo), "requests" ))
			
			out <- tryToGet_ClimDB(is_ToDo = i_ToDo,
				list.export = list.export,
				GCM_source, is_GDODCPUCLLNL, is_NEX, gcmFiles, reqGCMs, reqRCPsPerGCM, reqDownscalingsPerGCM, bbox, locations, varTags, fileVarTags, getYears, assocYears, saveNEXtempfiles)
			
			i_Done <- sort(unique(c(i_Done, out)))
			saveRDS(i_Done, file = logFile)
		}
		rm(i_ToDo, logFile)

		# Determine progress
		if (length(i_Done) > 0) {
			if (!be.quiet) print(paste(GCM_source, "was extracted for n =", length(i_Done), "out of", length(i_AllToDo), "downscaling requests"))

			ils_done <- unique((i_Done - 1) %/% length(reqGCMs) + 1)
			include_YN_climscen[do_SWRun_sites][ils_done] <- 1
			
			i_ToDo <- i_AllToDo[-i_Done]
		} else {
			i_ToDo <- i_AllToDo
		}
		
		#Clean up: report unfinished locations, etc.
		if (length(i_ToDo) > 0) {
			print(paste(length(i_ToDo), "sites didn't extract climate scenario information by '", GCM_source, "'"))
			ils_notdone <- unique((i_ToDo - 1) %/% length(reqGCMs) + 1)
			failedLocations_DB <- locations[ils_notdone, ]
			
			include_YN_updateFailed <- include_YN
			include_YN_updateFailed[do_SWRun_sites][ils_notdone] <- 0
			save(failedLocations_DB, include_YN_updateFailed, file=file.path(dir.in, paste0("ClimDB_failedLocations_", GCM_source, ".RData")))

			rm(failedLocations_DB, include_YN_updateFailed, ils_notdone)
		}
	
		if (!be.quiet) print(paste("Finished '", GCM_source, "' at", Sys.time()))
	
		rm(locations, requestN, i_Done, i_ToDo, i_AllToDo, varTags, timeSlices, getYears, assocYears, gcmsDB, scenariosDB)

		include_YN_climscen
	})
	
	# keep track of successful/unsuccessful climate scenarios
	include_YN_climscen <- rep(0, runsN_master)
	
	# loop through data sources
	for (isource in sort(unique(sites_GCM_source))) {
		include_YN_climscen <- get_climatechange_data(GCM_source = isource,
								is_GDODCPUCLLNL = grepl("BCSD_GDODCPUCLLNL", isource),
								is_NEX = isource == "CMIP5_BCSD_NEX_USA",
								do_SWRun_sites = runIDs_sites[sites_GCM_source == isource],
								include_YN_climscen = include_YN_climscen)
	}

	SWRunInformation$Include_YN_ClimateScenarioSources <- include_YN_climscen
	write.csv(SWRunInformation, file=file.path(dir.in, datafile.SWRunInformation), row.names=FALSE)
	unlink(file.path(dir.in, datafile.SWRWinputs_preprocessed))

	rm(sites_GCM_source, xy, i_use, include_YN_climscen)
}


#-CMIP3_ClimateWizardEnsembles does not allow for multiple sources
if (	exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_Global || exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_USA) {
	
	if (!be.quiet) print(paste("Started 'ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles' at", Sys.time()))
	
	list.scenarios.datafile <- climate.conditions[!grepl(climate.ambient, climate.conditions)]
	if (length(list.scenarios.datafile) > 0) { #extracts only information requested in the 'datafile.SWRunInformation'
		
		if (exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_Global) {
			#Maurer EP, Adam JC, Wood AW (2009) Climate model based consensus on the hydrologic impacts of climate change to the Rio Lempa basin of Central America. Hydrology and Earth System Sciences, 13, 183-194.
			#accessed via climatewizard.org on July 10, 2012
			dir.ex.dat <- file.path(dir.external, "ExtractClimateChangeScenarios", "ClimateWizard_CMIP3", "Global")
		}
		if (exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_USA) {
			#Maurer, E. P., L. Brekke, T. Pruitt, and P. B. Duffy. 2007. Fine-resolution climate projections enhance regional climate change impact studies. Eos Transactions AGU 88:504.
			#accessed via climatewizard.org
			dir.ex.dat <- file.path(dir.external, "ExtractClimateChangeScenarios", "ClimateWizard_CMIP3", "USA")
		}
		
		list.scenarios.external <- basename(list.dirs2(path=dir.ex.dat, full.names=FALSE, recursive=FALSE))
		
		if (all(list.scenarios.datafile %in% list.scenarios.external)) {
			#locations of simulation runs
			locations <- SpatialPoints(coords=with(SWRunInformation, data.frame(X_WGS84, Y_WGS84)), proj4string=CRS("+proj=longlat +datum=WGS84"))
			
			for (sc in seq_along(list.scenarios.datafile)) {
				dir.ex.dat.sc <- file.path(dir.ex.dat, list.scenarios.datafile[sc])
				temp <- basename(list.dirs2(path=dir.ex.dat.sc, full.names=FALSE, recursive=FALSE))
				if (exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_Global) {
					dir.ex.dat.sc.ppt <- file.path(dir.ex.dat.sc, temp[grepl(pattern="Precipitation_Value", x=temp)])
					dir.ex.dat.sc.temp <- file.path(dir.ex.dat.sc, temp[grepl(pattern="Tmean_Value", x=temp)])
				}		
				if (exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_USA) {
					dir.ex.dat.sc.ppt <- file.path(dir.ex.dat.sc, temp[grepl(pattern="Precipitation_Change", x=temp)])
					dir.ex.dat.sc.temp <- file.path(dir.ex.dat.sc, temp[grepl(pattern="Tmean_Change", x=temp)])
				}		
				list.temp.asc <- list.files(dir.ex.dat.sc.temp, pattern=".asc")
				list.ppt.asc <- list.files(dir.ex.dat.sc.ppt, pattern=".asc")
				
				#extract data
				get.month <- function(path, grid) {
					g <- raster(file.path(path, grid))
					locations.CoordG <- spTransform(locations, CRS=CRS(proj4string(g)))	#transform points to grid-coords
					vals <- extract(g, locations.CoordG)
				}
				sc.temp <- sapply(st_mo, FUN=function(m) get.month(path=dir.ex.dat.sc.temp, grid=list.temp.asc[grepl(pattern=paste("_", m, "_", sep=""), x=list.temp.asc)]))
				sc.ppt <- sapply(st_mo, FUN=function(m) get.month(path=dir.ex.dat.sc.ppt, grid=list.ppt.asc[grepl(pattern=paste("_", m, "_", sep=""), x=list.temp.asc)]))
				
				if (exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_Global) {
					#temp value in C
					#ppt value in mm
					#add data to sw_input_climscen and set the use flags
					sw_input_climscen_values_use[i.temp <- match(paste("PPTmm_m", st_mo, "_sc", formatC(sc, width=2,format="d", flag="0"), sep=""), colnames(sw_input_climscen_values_use))] <- 1
					sw_input_climscen_values[, i.temp] <- sc.ppt
					sw_input_climscen_values_use[i.temp <- match(paste("TempC_m", st_mo, "_sc", formatC(sc, width=2,format="d", flag="0"), sep=""), colnames(sw_input_climscen_values_use))] <- 1
					sw_input_climscen_values[, i.temp] <- sc.temp
				}
				if (exinfo$ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles_USA) {
					sc.temp <- sc.temp * 5/9	#temp addand in C
					sc.ppt <- 1 + sc.ppt/100	#ppt change as factor
					#add data to sw_input_climscen and set the use flags
					sw_input_climscen_use[i.temp <- match(paste("PPTfactor_m", st_mo, "_sc", formatC(sc, width=2,format="d", flag="0"), sep=""), colnames(sw_input_climscen_use))] <- 1
					sw_input_climscen[, i.temp] <- sc.ppt
					sw_input_climscen_use[i.temp <- match(paste("deltaTempC_m", st_mo, "_sc", formatC(sc, width=2,format="d", flag="0"), sep=""), colnames(sw_input_climscen_use))] <- 1
					sw_input_climscen[, i.temp] <- sc.temp
				}
			}
			
			res <- nrow(sw_input_climscen_values[, i.temp]) - sum(complete.cases(sw_input_climscen_values[, i.temp]))
			if (res > 0) print(paste(res, "sites didn't extract climate scenario information by 'ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles'"))
			
			#write data to datafile.climatescenarios_values
			tempdat <- rbind(sw_input_climscen_values_use, sw_input_climscen_values)
			write.csv(tempdat, file=file.path(dir.sw.dat, datafile.climatescenarios_values), row.names=FALSE)
			unlink(file.path(dir.in, datafile.SWRWinputs_preprocessed))
			
			rm(list.scenarios.datafile, list.scenarios.external, tempdat, sc.temp, sc.ppt, res, locations)
		} else {
			print("Not all scenarios requested in 'datafile.SWRunInformation' are available in with 'ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles'")
		}
	}
	if (!be.quiet) print(paste("Finished 'ExtractClimateChangeScenarios_CMIP3_ClimateWizardEnsembles' at", Sys.time()))
}

#------END CLIMATE CHANGE DATA------


#--------------------------------------------------------------------------------------------------#
#------EXTRACT SOIL CHARACTERISTICS------
if (exinfo$ExtractSoilDataFromCONUSSOILFromSTATSGO_USA || exinfo$ExtractSoilDataFromISRICWISEv12_Global) {
	#allow for multiple sources
	if (extract_determine_database == "SWRunInformation" && "SoilTexture_source" %in% colnames(SWRunInformation)) {
		sites_externalsoils_source <- SWRunInformation$SoilTexture_source[runIDs_sites]
	} else if (extract_determine_database == "order" || !("SoilTexture_source" %in% colnames(SWRunInformation))) {
		sites_externalsoils_source <- rep(NA, times = runsN_sites)
	} else {
		stop(paste("Value of 'extract_determine_database'", extract_determine_database, "not implemented"))
	}
	
	if (exinfo$ExtractSoilDataFromCONUSSOILFromSTATSGO_USA) {
		if (!be.quiet) print(paste("Started 'ExtractSoilDataFromCONUSSOILFromSTATSGO_USA' at", Sys.time()))
		#Miller, D. A. and R. A. White. 1998. A conterminous United States multilayer soil characteristics dataset for regional climate and hydrology modeling. Earth Interactions 2:1-26.
		#CONUS-SOIL: rasterized and controlled STATSGO data; information for 11 soil layers available
		# Note(drs): it appears that NODATA values are recorded as 0
		
		do_extract <- is.na(sites_externalsoils_source) |
						sites_externalsoils_source == "CONUSSOILFromSTATSGO_USA"

		if (continueAfterAbort) {
			do_extract <- do_extract & (
								is.na(sw_input_soillayers[runIDs_sites, "SoilDepth_cm"]) |
								has_nodata(sw_input_soils[runIDs_sites, ], "Matricd_L") |
								has_nodata(sw_input_soils[runIDs_sites, ], "GravelContent_L") |
								has_nodata(sw_input_soils[runIDs_sites, ], "Sand_L") |
								has_nodata(sw_input_soils[runIDs_sites, ], "Clay_L"))
		}
				
		if (any(do_extract)) {
			if (!be.quiet) print(paste("'ExtractSoilDataFromCONUSSOILFromSTATSGO_USA' will be extracted for n =", sum(do_extract), "sites"))

			dir.ex.conus <- file.path(dir.ex.soil, "CONUSSoil", "output", "albers")
			stopifnot(file.exists(dir.ex.conus))
			
			ldepth <- c(5, 10, 20, 30, 40, 60, 80, 100, 150, 200, 250)	#in cm
			nl <- length(ldepth)

			# output container
			soil_data <- array(NA, dim=c(sum(do_extract), nl, 5), dimnames=list(NULL, paste0("L", seq_len(nl)), c("bedrock", "matricd", "rockvol", "sand", "clay")))
			g <- raster::brick(file.path(dir.ex.conus, "bd.tif"))
			crs_data <- raster::crs(g)

			#locations of simulation runs
			sites_conus <- run_sites[do_extract, ]
			# Align with data crs
			if (!raster::compareCRS(crs_sites, crs_data)) {
				sites_conus <- sp::spTransform(sites_conus, CRS = crs_data)	#transform points to grid-coords
			}

			if (sim_cells_or_points == "point") {
				cell_res_conus <- NULL
				args_extract <- list(x = sites_conus)
				
			} else if (sim_cells_or_points == "cell") {
				cell_res_conus <- align_with_target_res(res_from = sim_res, crs_from = sim_crs,
					sp = run_sites[do_extract, ], crs_sp = crs_sites, crs_to = crs_data)
				args_extract <- list(x = cell_res_conus, coords = sites_conus, method = "block")			
			}

			#---extract data
			# bulk density -> matric density
			cond30 <- compiler::cmpfun(function(v) ifelse(is.na(v) | v < 30, NA, v))
			ftemp <- file.path(dir.ex.conus, "bd_cond30.tif")
			if (file.exists(ftemp)) {
				g <- raster::brick(ftemp)
			} else {
				# bulk density of less than 0.3 g / cm3 should be treated as no soil
				g <- raster::calc(g, fun = cond30, filename = ftemp)
			}
			soil_data[, , "matricd"] <- round(do.call("extract_from_external_raster", args = c(args_extract, data = list(g)))) / 100	
			print("NOTE: soil density values extracted from CONUS-soil (gridded STATSGO) may be too low!")	
				
			# Convert bulk density to matric density
			#	eqn. 20 from Saxton et al. 2006: bulkd <- matricd * (1 - rockvol) + rockvol * 2.65
			# This appears to be matric density, Miller et al. 1998 has labelled this as bulk. However, eq. 20 (Saxton et al. 2006) would give negative values if we assumed it to be bulk density
			#matricd <- ifelse(abs(1 - rockvol) > sqrt(.Machine$double.eps), (bulkd - rockvol * 2.65) / (1 - rockvol), 0)

			# soil depth
			cond0 <- compiler::cmpfun(function(v) ifelse(!is.na(v) & v > 0, v, NA))
			ftemp <- file.path(dir.ex.conus, "rockdepm_cond0.tif")
			if (file.exists(ftemp)) {
				g <- raster::raster(ftemp)
			} else {
				# rockdepth of 0 cm should be treated as no soil
				g <- raster::calc(raster::raster(file.path(dir.ex.conus, "rockdepm.tif")), fun = cond0, filename = ftemp)
			}
			rockdep_cm <- round(do.call("extract_from_external_raster", args = c(args_extract, data = list(g)))) #depth in cm >< bedrock from datafile.bedrock, but seems to make more sense?
			
			# rock volume
			g <- raster::brick(file.path(dir.ex.conus, "rockvol.tif")) #New with v31: rockvol -> gravel vol%
			rockvol <- do.call("extract_from_external_raster", args = c(args_extract, data = list(g))) / 100
			# eq. 7 of Miller et al. 1998
			rockvol <- round(pmax(pmin(rockvol, 1), 0), 2) # volume fraction of bulk=total soil
			soil_data[, , "rockvol"] <- ifelse(is.finite(rockvol), rockvol, NA)
			
			# adjust soil depth by layers with 100% rock volume
			solid_rock_nl <- apply(soil_data[, , "rockvol"] >= 1 - toln, 1, sum, na.rm = TRUE)
			solid_rock_nl <- 1 + nl - solid_rock_nl
			solid_rock_cm <- c(0, ldepth)[solid_rock_nl]
			
			soil_data[, 1, "bedrock"] <- pmin(rockdep_cm, solid_rock_cm) # in most cases == rockdep_cm
			lys <- 1:max(findInterval(soil_data[, 1, "bedrock"], ldepth), na.rm=TRUE)
			
			# sand, silt, and clay
			ftemp <- file.path(dir.ex.conus, "sand_cond0.tif")
			if (file.exists(ftemp)) {
				g <- raster::brick(ftemp)
			} else {
				# sand of 0 cm should be treated as no soil
				g <- raster::calc(raster::brick(file.path(dir.ex.conus, "sand.tif")), fun = cond0, filename = ftemp)
			}
			sand <- do.call("extract_from_external_raster", args = c(args_extract, data = list(g))) / 100

			ftemp <- file.path(dir.ex.conus, "clay_cond0.tif")
			if (file.exists(ftemp)) {
				g <- raster::brick(ftemp)
			} else {
				# clay of 0 cm should be treated as no soil
				g <- raster::calc(raster::brick(file.path(dir.ex.conus, "clay.tif")), fun = cond0, filename = ftemp)
			}
			clay <- do.call("extract_from_external_raster", args = c(args_extract, data = list(g))) / 100

			ftemp <- file.path(dir.ex.conus, "silt_cond0.tif")
			if (file.exists(ftemp)) {
				g <- raster::brick(ftemp)
			} else {
				# silt of 0 cm should be treated as no soil
				g <- raster::calc(raster::brick(file.path(dir.ex.conus, "silt.tif")), fun = cond0, filename = ftemp)
			}
			silt <- do.call("extract_from_external_raster", args = c(args_extract, data = list(g))) / 100

			if (FALSE) {#visualize in interactive sessions
				temp <- sand
				cats <- addNA(cut(temp[, 1], breaks=seq(0, to=max(1, max(temp, na.rm=TRUE)), length.out=nl)))
				cols <- c(head(rainbow(n=nlevels(cats)), n=-1), "gray")
				plot(run_sites, pch=15, cex=0.5, col=cols[cats])
				legend(x="bottomleft", legend=sQuote(levels(cats)), pch=19, col=cols)
				if (require("maps")) map("state", add=TRUE)
			}

			#Normalize to 0-1
			total_matric <- sand + clay + silt # values between 0.99 and 1.01 (of the matric component)
			sand <- round(sand / total_matric, 2) # mass fraction of matric component
			soil_data[, , "sand"] <- ifelse(is.finite(sand), sand, NA)
			clay <- round(clay / total_matric, 2) # mass fraction of matric component
			soil_data[, , "clay"] <- ifelse(is.finite(clay), clay, NA)

			# Determine successful extractions
			i_good <- complete.cases(soil_data[, 1, ]) #length(i_good) == sum(do_extract)
			sites_externalsoils_source[which(do_extract)[!i_good]] <- NA
		
			if (any(i_good)) {
				i_Done <- rep(FALSE, times = runsN_sites) #length(i_Done) == length(runIDs_sites) == runsN_sites
				i_Done[which(do_extract)[i_good]] <- TRUE #sum(i_Done) == sum(i_good)

				sites_externalsoils_source[i_Done] <- "CONUSSOILFromSTATSGO_USA"
						
				#set and save soil layer structure
				sw_input_soillayers[runIDs_sites[i_Done], "SoilDepth_cm"] <- soil_data[i_good, 1, "bedrock"]
				i.temp <- grep("depth_L", colnames(sw_input_soillayers))
				sw_input_soillayers[runIDs_sites[i_Done], i.temp[lys]] <- matrix(data=rep(ldepth[lys], times=sum(i_good)), ncol=length(lys), byrow=TRUE)
				write.csv(sw_input_soillayers, file=file.path(dir.in, datafile.soillayers), row.names=FALSE)
				unlink(file.path(dir.in, datafile.SWRWinputs_preprocessed))

				#set and save soil texture
				#add data to sw_input_soils and set the use flags
				i.temp <- grep("Matricd_L", names(sw_input_soils_use))
				sw_input_soils[runIDs_sites[i_Done], i.temp[lys]] <- soil_data[i_good, lys, "matricd"]
				sw_input_soils_use[i.temp][lys] <- 1
				i.temp <- grep("GravelContent_L", names(sw_input_soils_use))
				sw_input_soils[runIDs_sites[i_Done], i.temp[lys]] <- soil_data[i_good, lys, "rockvol"]
				sw_input_soils_use[i.temp][lys] <- 1
				i.temp <- grep("Sand_L", names(sw_input_soils_use))
				sw_input_soils[runIDs_sites[i_Done], i.temp[lys]] <- soil_data[i_good, lys, "sand"]
				sw_input_soils_use[i.temp][lys] <- 1
				i.temp <- grep("Clay_L", names(sw_input_soils_use))
				sw_input_soils[runIDs_sites[i_Done], i.temp[lys]] <- soil_data[i_good, lys, "clay"]
				sw_input_soils_use[i.temp][lys] <- 1

				#write data to datafile.soils
				tempdat <- rbind(sw_input_soils_use, sw_input_soils)
				write.csv(tempdat, file=file.path(dir.sw.dat, datafile.soils), row.names=FALSE)
				unlink(file.path(dir.in, datafile.SWRWinputs_preprocessed))
		
				rm(tempdat, i.temp, i_Done)
			}

			if (!be.quiet) print(paste("'ExtractSoilDataFromCONUSSOILFromSTATSGO_USA' was extracted for n =", sum(i_good), "out of", sum(do_extract), "sites"))
			rm(lys, total_matric, rockvol, sand, clay, silt, g, sites_conus, cell_res_conus, soil_data, cond0, cond30, i_good, solid_rock_nl, solid_rock_cm)
		}
		
		if (!be.quiet) print(paste("Finished 'ExtractSoilDataFromCONUSSOILFromSTATSGO_USA' at", Sys.time()))
	}

	if (exinfo$ExtractSoilDataFromISRICWISEv12_Global) {
		if (!be.quiet) print(paste("Started 'ExtractSoilDataFromISRICWISEv12_Global' at", Sys.time()))
		#Batjes, N. H. 2012. ISRIC-WISE derived soil properties on a 5 by 5 arc-minutes global grid (ver. 1.2). Report 2012/01 (with data set, available at www.isric.org). ISRIC-World Soil Information, Wageningen, The Netherlands.
		#http://www.isric.org/data/isric-wise-derived-soil-properties-5-5-arc-minutes-global-grid-version-12
		#cells with no soil values have SUID=c(0=Water, 6997=Water, 6694=Rock, or 6998=Glacier)

		do_extract <- is.na(sites_externalsoils_source) |
						sites_externalsoils_source == "ISRICWISEv12_Global"

		if (continueAfterAbort) {
			do_extract <- do_extract & (
								is.na(sw_input_soillayers[runIDs_sites, "SoilDepth_cm"]) |
								has_nodata(sw_input_soils[runIDs_sites, ], "Matricd_L") |
								has_nodata(sw_input_soils[runIDs_sites, ], "GravelContent_L") |
								has_nodata(sw_input_soils[runIDs_sites, ], "Sand_L") |
								has_nodata(sw_input_soils[runIDs_sites, ], "Clay_L"))
		}		

		if (any(do_extract)) {
			if (!be.quiet) print(paste("'ExtractSoilDataFromISRICWISEv12_Global' will be extracted for n =", sum(do_extract), "sites"))
		
			layer_N <- 5	#WISE contains five soil layers for each prid
			layer_Nsim <- 6	#WISE contains five soil layers for each prid; I added one layer to account for lithosols (Ix), which have a soildepth of 10 cm; for all other soil types, my layers 0-10 cm and 10-20 cm contain the same wise information
			layer_TopDep <- c(0, 10, 20, 40, 60, 80)	#in cm
			layer_BotDep <- c(10, 20, 40, 60, 80)	#in cm
	
			dir.ex.dat <- file.path(dir.ex.soil, "wise5by5min_v1b")
			stopifnot(file.exists(dir.ex.dat), require(raster), require(sp), require(rgdal))
	
			#run_sites_wise of simulation runs
			run_sites_wise <- run_sites[do_extract, ]
			is_ToDo <- seq_along(run_sites_wise)
		
			#---extract data
			grid_wise <- raster::raster(x=file.path(dir.ex.dat, "Grid", "smw5by5min"))

			#- List all the wise cells that are covered by the grid cell or point location
			if (sim_cells_or_points == "point") {
				cell_res_wise <- NULL
				suids <- raster::extract(grid_wise, run_sites_wise)
				sim_cells_SUIDs <- data.frame(i = is_ToDo, SUIDs_N = 1, SUID = suids, fraction = 1)
			
			} else if (sim_cells_or_points == "cell") {
				cell_res_wise <- align_with_target_res(res_from = sim_res, crs_from = sim_crs,
					sp = run_sites_wise, crs_sp = crs_sites, crs_to = raster::crs(grid_wise))
				
				#' A wrapper for \code{reaggregate_raster} design to work with raster data from ISRIC-WISE
				#' 
				#' @param i An integer value. The index to select a location from among \code{sp_sites} and the corresponding resolution \code{res}.
				#' @param res A numeric vector of length two or a matrix with two columns. The x- and y-extent of the rectangle(s) for which to extract values.
				#' @param grid A \linkS4class{RasterLayer} object with one layer. The raster from which values are extracted.
				#' @param sp_sites A \linkS4class{SpatialPoints} object. This object is used to extract the coordinates of the i-th location.
				#'
				#' @seealso \code{\link{reaggregate_raster}}
				#'
				#' @return A list with four elements
				#'	\describe{
				#'		\item{i}{An integer value. The location index.}
				#'		\item{SUIDs_N}{An integer vector. The number of unique values within the rectangle of \code{x}.}
				#'		\item{SUID}{A numeric vectors. The sorted unique values.}
				#'		\item{fraction}{A numeric vector. The relative areas covered by \code{values}.}
				#'	}
				extract_SUIDs <- compiler::cmpfun(function(i, res = c(0, 0), grid, sp_sites) {
					# raster::nlayers(grid_wise) == 1
					out <- try(reaggregate_raster(x = grid,
								coord = sp::coordinates(sp_sites[i, ]),
								to_res = if (is.null(dim(res))) res else res[i, ],
								with_weights = TRUE,
								method = "block"), silent = TRUE)
					
					if (inherits(out, "try-error")) {
						if (print.debug) print(out)
						list(i = i, SUIDs_N = -1, SUID = NULL, fraction = NULL)
					} else {
						list(i = i, SUIDs_N = out[[1]][["N"]][[1]],
									SUID = out[[1]][["values"]][[1]],
									fraction = out[[1]][["fraction"]][[1]])
					}
				})

				if (parallel_runs && parallel_init) {
					#objects that need exporting to slaves
					list.export <- c("grid_wise", "run_sites_wise", "cell_res_wise", "reaggregate_raster", "extract_blocks", "add_weights", "print.debug")

					#call the simulations depending on parallel backend
					if (identical(parallel_backend, "mpi")) {
						exportObjects(list.export)
						Rmpi::mpi.bcast.cmd(library(raster, quietly=TRUE))
				
						sim_cells_SUIDs <- Rmpi::mpi.applyLB(x=is_ToDo, fun=extract_SUIDs, res = cell_res_wise, grid = grid_wise, sp_sites = run_sites_wise)
						sim_cells_SUIDs <- do.call(rbind, sim_cells_SUIDs)
				
						Rmpi::mpi.bcast.cmd(rm(list=ls()))
						Rmpi::mpi.bcast.cmd(gc())
					} else if (identical(parallel_backend, "snow")) {
						snow::clusterExport(cl, list.export, envir=parent.frame())
						snow::clusterEvalQ(cl, library(raster, quietly = TRUE))
				
						sim_cells_SUIDs <- snow::clusterApplyLB(cl, x=is_ToDo, fun=extract_SUIDs, res = cell_res_wise, grid = grid_wise, sp_sites = run_sites_wise)
						sim_cells_SUIDs <- do.call(rbind, sim_cells_SUIDs)
				
						snow::clusterEvalQ(cl, rm(list=ls()))
						snow::clusterEvalQ(cl, gc())
					} else if (identical(parallel_backend, "multicore")) {
						packages.export <- "raster"
						sim_cells_SUIDs <- foreach(i=is_ToDo, .combine="rbind", .errorhandling="remove", .inorder=FALSE, .export=list.export, .packages=packages.export) %dopar%
							extract_SUIDs(i, res = cell_res_wise, grid = grid_wise, sp_sites = run_sites_wise)
					} else {
						sim_cells_SUIDs <- NULL
					}
				} else {
					sim_cells_SUIDs <- foreach(i=is_ToDo, .combine="rbind", .errorhandling="remove", .inorder=FALSE) %do%
						extract_SUIDs(i, res = cell_res_wise, grid = grid_wise, sp_sites = run_sites_wise)
				}
			}
			rm(grid_wise)

			sim_cells_SUIDs <- sim_cells_SUIDs[order(unlist(sim_cells_SUIDs[,"i"])),]

			#- Calculate simulation cell wide weighted values based on each PRID weighted by SUID.fraction x PRIP.PROP
			dat_wise <- read.csv(file=file.path(dir.ex.dat, "WISEsummaryFile.csv"))
		
			get_prids <- compiler::cmpfun(function(suid) {
				soils <- dat_wise[dat_wise$SUID == suid, ]
				frac <- unique(soils[, c("PROP", "PRID")])
				depth <- aggregate(soils$BotDep, by = list(soils$PRID), FUN = max)
				idepth <- depth[match(frac$PRID, depth[, 1]), 2]
				
				list(PRIDs_N = nrow(soils) / layer_N,
					 PRID = frac$PRID,
					 fraction = frac$PROP / 100,
					 soildepth = ifelse(idepth > 0, idepth, NA),
					 soildat = soils)
			})

			get_SoilDatValuesForLayer <- compiler::cmpfun(function(dat, soildat_rows, frac) {
				sum(soildat_rows * frac, dat, na.rm = TRUE) #weighted mean = sum of values x weights
			})

			template_simulationSoils <- rep(NA, times = 2 + 4 * layer_Nsim)
			names(template_simulationSoils) <- c("i", "soildepth", paste0(rep(c("bulk", "sand", "clay", "cfrag"), times=layer_Nsim), "_L", rep(1:layer_Nsim, each=4)))
			template_simulationSoils["soildepth"] <- 0
		
			#cells with no soil values have SUID=c(0=Water, 6997=Water, 6694=Rock, or 6998=Glacier)
			calc_weightedMeanForSimulationCell <- compiler::cmpfun(function(i, i_sim_cells_SUIDs, simulationSoils, layer_N, layer_Nsim, layer_TopDep) {
				#Init
				simulationSoils["i"] <- i
				simulation_frac <- 0	#fraction of how much this simulation cell is covered with suids and prids that have a soildepth > 0 cm
				simulation_layer_frac <- rep(0, times = layer_Nsim) #fraction of each soil layer covering this simulation cell
				PRIDs_N <- 0
				PRIDs <- PRIDs_frac <- NULL
				
				#Do calculations if any soils in this simulation cell
				if (i_sim_cells_SUIDs$SUIDs_N > 0) {
					this_simCell <- c(i_sim_cells_SUIDs, soils = list(t(sapply(i_sim_cells_SUIDs$SUID, FUN = get_prids))))
					
					for (is in seq_len(this_simCell$SUIDs_N)) {	#loop through the suids within this simulation cell; each suid may be composed of several prids
						prids_frac <- this_simCell$soils[is,]$fraction * this_simCell$fraction[is]	#vector of the fractions of each prid in relation to the simulation cell
						PRIDs_frac <- c(PRIDs_frac, prids_frac)
						simulation_frac <- simulation_frac + sum(ifelse(!is.na(this_simCell$soils[is,]$soildepth), prids_frac, 0))
						simulationSoils["soildepth"] <- simulationSoils["soildepth"] + sum(this_simCell$soils[is,]$soildepth * prids_frac, na.rm = TRUE)
						
						if (!all(is.na(this_simCell$soils[is,]$soildepth))) for (ils in seq_len(layer_Nsim)) {
							lwise <- if (ils == 1) 1 else {ils - 1}	# I split wise soil layer 0-20 cm into two layers, 0-10 and 10-20 cm, to account for lithosols
							layer.there <- this_simCell$soils[is,]$soildepth > layer_TopDep[ils]	#checks if for each prid, there soils are deeper than this layer. It also accounts that soil depth for Rock outcrops (RK) is set to 0 instead of < 0 for such as water and glaciers. Lithosols (Ix) have soildepth of 10 cm.
							pfracl <- prids_frac[layer.there]
							simulation_layer_frac[ils] <- simulation_layer_frac[ils] + sum(pfracl, na.rm=TRUE)
							
							if (sum(layer.there, na.rm = TRUE) > 0) {
								irow <- lwise + ((0:(this_simCell$soils[is,]$PRIDs_N - 1)) * layer_N)[layer.there]
								simulationSoils[paste0("bulk_L", ils)] <- get_SoilDatValuesForLayer(
									dat = simulationSoils[paste0("bulk_L", ils)],
									soildat_rows = this_simCell$soils[is,]$soildat[irow, "BULK"],
									frac = pfracl)	# bulk density (kg/dm3)
								simulationSoils[paste0("sand_L", ils)] <- get_SoilDatValuesForLayer(
									dat = simulationSoils[paste0("sand_L", ils)],
									soildat_rows = this_simCell$soils[is,]$soildat[irow, "SDTO"],
									frac = pfracl)	# Sand mass (%)
								simulationSoils[paste0("clay_L", ils)] <- get_SoilDatValuesForLayer(
									dat = simulationSoils[paste0("clay_L", ils)],
									soildat_rows = this_simCell$soils[is,]$soildat[irow, "CLPC"],
									frac = pfracl)	 # clay mass (%)
								simulationSoils[paste0("cfrag_L", ils)] <- get_SoilDatValuesForLayer(
									dat = simulationSoils[paste0("cfrag_L", ils)],
									soildat_rows = this_simCell$soils[is,]$soildat[irow, "CFRAG"],
									frac = pfracl)	# coarse fragments (vol % > 2 mm)
							}
						}
					}
				
					#Adjust values for area present
					simulationSoils <- simulationSoils / c(1, simulation_frac, rep(simulation_layer_frac, each = 4))
				}
				
				simulationSoils
			})
			
			
			try_weightedMeanForSimulationCell <- compiler::cmpfun(function(i, sim_cells_SUIDs, template_simulationSoils, layer_N, layer_Nsim, layer_TopDep) {
				if (i %% 1000 == 0) print(paste(Sys.time(), "done:", i))

				temp <- calc_weightedMeanForSimulationCell(i,
							i_sim_cells_SUIDs = sim_cells_SUIDs[i, ],
							simulationSoils = template_simulationSoils,
							layer_N = layer_N, layer_Nsim = layer_Nsim, layer_TopDep = layer_TopDep)
				if (inherits(temp, "try-error")) template_simulationSoils else temp
			})
	
			if (parallel_runs && parallel_init) {
				#objects that need exporting to slaves
				list.export <- c("get_prids", "dat_wise", "layer_TopDep", "layer_N", "get_SoilDatValuesForLayer", "layer_Nsim", "calc_weightedMeanForSimulationCell", "try_weightedMeanForSimulationCell", "template_simulationSoils", "sim_cells_SUIDs")
				#call the simulations depending on parallel backend
				if (identical(parallel_backend, "mpi")) {
					exportObjects(list.export)
			
					sim_cells_soils <- Rmpi::mpi.applyLB(x = is_ToDo, fun = try_weightedMeanForSimulationCell,
						sim_cells_SUIDs = sim_cells_SUIDs,
						template_simulationSoils = template_simulationSoils,
						layer_N = layer_N, layer_Nsim = layer_Nsim, layer_TopDep = layer_TopDep)
					sim_cells_soils <- do.call(rbind, sim_cells_soils)
			
					Rmpi::mpi.bcast.cmd(rm(list=ls()))
					Rmpi::mpi.bcast.cmd(gc())
				} else if (identical(parallel_backend, "snow")) {
					snow::clusterExport(cl, list.export, envir=parent.frame())
			
					sim_cells_soils <- snow::clusterApplyLB(cl, x = is_ToDo, fun = try_weightedMeanForSimulationCell,
						sim_cells_SUIDs = sim_cells_SUIDs,
						template_simulationSoils = template_simulationSoils,
						layer_N = layer_N, layer_Nsim = layer_Nsim, layer_TopDep = layer_TopDep)
					sim_cells_soils <- do.call(rbind, sim_cells_soils)
				
					snow::clusterEvalQ(cl, rm(list=ls()))
					snow::clusterEvalQ(cl, gc())
				} else if (identical(parallel_backend, "multicore")) {
					sim_cells_soils <- foreach(i=is_ToDo, .combine="rbind", .errorhandling="remove", .inorder=FALSE, .export=list.export) %dopar%
						try_weightedMeanForSimulationCell(i, sim_cells_SUIDs = sim_cells_SUIDs,
						template_simulationSoils = template_simulationSoils,
						layer_N = layer_N, layer_Nsim = layer_Nsim, layer_TopDep = layer_TopDep)
				}
			} else {
				sim_cells_soils <- foreach(i=is_ToDo, .combine="rbind", .errorhandling="remove", .inorder=FALSE) %do%
					try_weightedMeanForSimulationCell(i, sim_cells_SUIDs = sim_cells_SUIDs,
						template_simulationSoils = template_simulationSoils,
						layer_N = layer_N, layer_Nsim = layer_Nsim, layer_TopDep = layer_TopDep)
			}
			rm(dat_wise)

			sim_cells_soils <- sim_cells_soils[order(sim_cells_soils[, "i"]), ]

			if (FALSE) {#visualize in interactive sessions
				temp <- sim_cells_soils[, grep("bulk", colnames(sim_cells_soils))]
				cats <- addNA(cut(temp[, 1], breaks=seq(0, to=max(1, max(temp, na.rm=TRUE)), length.out=10)))
				cols <- c(head(rainbow(n=nlevels(cats)), n=-1), "gray")
				plot(run_sites_wise, pch=15, cex=0.5, col=cols[cats])
				legend(x="bottomleft", legend=sQuote(levels(cats)), pch=19, col=cols)
				if (require("maps")) map("state", add=TRUE)
			}

			#Convert bulk density to matric density
			#	eqn. 20 from Saxton et al. 2006: bulkd <- matricd * (1 - rockvol) + rockvol * 2.65
			# 'bulk density' here is of the matric component, i.e., what we call matric density
			#matricd <- (sim_cells_soils[, grep("bulk", colnames(sim_cells_soils))] - 2.65 * sim_cells_soils[, grep("cfrag", colnames(sim_cells_soils))]) / (1 - sim_cells_soils[, grep("cfrag", colnames(sim_cells_soils))])
			
			i_good <- rep(FALSE, sum(do_extract))
			i_good[sim_cells_soils[complete.cases(sim_cells_soils), "i"]] <- TRUE # i is index for do_extract
			sites_externalsoils_source[which(do_extract)[!i_good]] <- NA
		
			if (any(i_good)) {
				i_Done <- rep(FALSE, times = runsN_sites) #length(i_Done) == length(runIDs_sites) == runsN_sites
				i_Done[which(do_extract)[i_good]] <- TRUE #sum(i_Done) == sum(i_good)

				sites_externalsoils_source[i_Done] <- "ISRICWISEv12_Global"
		
				#set and save soil layer structure
				lys <- 1:layer_Nsim
				sw_input_soillayers[runIDs_sites[i_Done], "SoilDepth_cm"] <- round(sim_cells_soils[i_good, "soildepth"])
				i.temp <- grep("depth_L", colnames(sw_input_soillayers))
				sw_input_soillayers[runIDs_sites[i_Done], i.temp[lys]] <- matrix(data=rep(layer_BotDep[lys], times=sum(i_good)), ncol=length(lys), byrow=TRUE)
				sw_input_soillayers[runIDs_sites[i_Done], i.temp[-lys]] <- NA
				write.csv(sw_input_soillayers, file=file.path(dir.in, datafile.soillayers), row.names=FALSE)
				unlink(file.path(dir.in, datafile.SWRWinputs_preprocessed))

				#set and save soil texture
				#add data to sw_input_soils and set the use flags
				i.temp <- grep("Matricd_L", names(sw_input_soils_use))
				sw_input_soils[runIDs_sites[i_Done], i.temp[lys]] <- round(sim_cells_soils[i_good, paste0("bulk_L", lys)], 2)
				sw_input_soils_use[i.temp][lys] <- 1
				i.temp <- grep("GravelContent_L", names(sw_input_soils_use))
				sw_input_soils[runIDs_sites[i_Done], i.temp[lys]] <- round(sim_cells_soils[i_good, paste0("cfrag_L", lys)]) / 100
				sw_input_soils_use[i.temp][lys] <- 1
				i.temp <- grep("Sand_L", names(sw_input_soils_use))
				sw_input_soils[runIDs_sites[i_Done], i.temp[lys]] <- round(sim_cells_soils[i_good, paste0("sand_L", lys)]) / 100
				sw_input_soils_use[i.temp][lys] <- 1
				i.temp <- grep("Clay_L", names(sw_input_soils_use))
				sw_input_soils[runIDs_sites[i_Done], i.temp[lys]] <- round(sim_cells_soils[i_good, paste0("clay_L", lys)]) / 100
				sw_input_soils_use[i.temp][lys] <- 1

				#write data to datafile.soils
				tempdat <- rbind(sw_input_soils_use, sw_input_soils)
				write.csv(tempdat, file=file.path(dir.sw.dat, datafile.soils), row.names=FALSE)
				unlink(file.path(dir.in, datafile.SWRWinputs_preprocessed))

				rm(lys, tempdat, i.temp, i_Done)
			}
		
			if (!be.quiet) print(paste("'ExtractSoilDataFromISRICWISEv12_Global' was extracted for n =", sum(i_good), "out of", sum(do_extract), "sites"))
			rm(sim_cells_soils, run_sites_wise, cell_res_wise, i_good)
		}
		
		if (!be.quiet) print(paste("Finished 'ExtractSoilDataFromISRICWISEv12_Global' at", Sys.time()))
	}

	
	#write data to datafile.SWRunInformation
	SWRunInformation$SoilTexture_source[runIDs_sites] <- as.character(sites_externalsoils_source)
	notDone <- is.na(sites_externalsoils_source)
	include_YN_soils <- rep(0, runsN_master)
	include_YN_soils[runIDs_sites[!notDone]] <- 1
	SWRunInformation$Include_YN_SoilSources <- include_YN_soils
	write.csv(SWRunInformation, file=file.path(dir.in, datafile.SWRunInformation), row.names=FALSE)
	unlink(file.path(dir.in, datafile.SWRWinputs_preprocessed))

	if (any(notDone)) print(paste("'ExtractSoilData': no soil information for n =", sum(notDone), "sites (e.g., sand or clay is 0): this will likely lead to crashes of SoilWat"))

	rm(do_extract, notDone, include_YN_soils)
}

#------END OF SOIL CHARACTERISTICS------
#--------------------------------------------------------------------------------------------------#




#--------------------------------------------------------------------------------------------------#
#------EXTRACT ELEVATION------
if (exinfo$ExtractElevation_NED_USA || exinfo$ExtractElevation_HWSD_Global) {
	#allow for multiple sources
	if (extract_determine_database == "SWRunInformation" && "Elevation_source" %in% colnames(SWRunInformation)) {
		sites_elevation_source <- SWRunInformation$Elevation_source[runIDs_sites]
	} else if (extract_determine_database == "order" || !("Elevation_source" %in% colnames(SWRunInformation))) {
		sites_elevation_source <- rep(NA, times = runsN_sites)
	} else {
		stop(paste("Value of 'extract_determine_database'", extract_determine_database, "not implemented"))
	}
	
	elev_probs <- if (sim_cells_or_points == "cell") c(0.025, 0.5, 0.975) else NULL
	elevation_m <- matrix(NA, nrow = runsN_sites, ncol = 1 + length(elev_probs),
		dimnames = list(NULL, c("ELEV_m", if (sim_cells_or_points == "cell") paste0("ELEV_m_q", elev_probs))))
	
	#	- extract NED data where available
	if (exinfo$ExtractElevation_NED_USA) {
		if (!be.quiet) print(paste("Started 'ExtractElevation_NED_USA' at", Sys.time()))
		#ned.usgs.gov
	
		do_extract <- !complete.cases(elevation_m) | is.na(sites_elevation_source) |
						sites_elevation_source == "Elevation_NED_USA"

		if (continueAfterAbort) {
			do_extract <- do_extract & has_nodata(SWRunInformation[runIDs_sites, ], "ELEV_m")
		}

		if (any(do_extract)) {
			if (!be.quiet) print(paste("'ExtractElevation_NED_USA' will be extracted for n =", sum(do_extract), "sites"))
			dir.ex.ned <- file.path(dir.ex.dem, 'NED_USA', "NED_1arcsec")
	
			#read raster data
			g.elev <- raster::raster(file.path(dir.ex.ned, "ned_1s_westernUS_GeogrNAD83.tif"))
			crs_data <- raster::crs(g.elev)
	
			#locations of simulation runs
			sites_ned <- run_sites[do_extract, ]
			# Align with data crs
			if (!raster::compareCRS(crs_sites, crs_data)) {
				sites_ned <- sp::spTransform(sites_ned, CRS = crs_data)	#transform points to grid-coords
			}

			if (sim_cells_or_points == "point") {
				args_extract <- list(x = sites_ned)

			} else if (sim_cells_or_points == "cell") {
				cell_res_ned <- align_with_target_res(res_from = sim_res, crs_from = sim_crs,
					sp = run_sites[do_extract, ], crs_sp = crs_sites, crs_to = crs_data)
				args_extract <- list(x = cell_res_ned, coords = sites_ned, method = "block", probs = elev_probs)			
			}

			#extract data for locations
			temp <- round(do.call("extract_from_external_raster", args = c(args_extract, data = list(g.elev))))	# elevation in m a.s.l.
			if (is.vector(temp)) {
				elevation_m[do_extract, "ELEV_m"] <- temp
			} else if (is.array(temp)) {
				elevation_m[do_extract, ] <- temp[, 1, ]
			} else stop("Unknown object returned from 'extract_from_external_raster' when extracting elevation data.")
			
			i_good <- complete.cases(elevation_m[do_extract, ]) #length(i_good) == sum(do_extract)
			sites_elevation_source[which(do_extract)[!i_good]] <- NA
			
			if (any(i_good)) {
				i_Done <- rep(FALSE, times = runsN_sites) #length(i_Done) == length(runIDs_sites) == runsN_sites
				i_Done[which(do_extract)[i_good]] <- TRUE #sum(i_Done) == sum(i_good)
				sites_elevation_source[i_Done] <- "Elevation_NED_USA"
				if (!be.quiet) print(paste("'ExtractElevation_NED_USA' was extracted for n =", sum(i_good), "out of", sum(do_extract), "sites"))
			}
	
			rm(dir.ex.ned, g.elev, sites_ned, args_extract, i_good)
		}
		
		if (!be.quiet) print(paste("Finished 'ExtractElevation_NED_USA' at", Sys.time()))
	}

	#	- extract HWSD elevation data for sites with no elevation data
	if (exinfo$ExtractElevation_HWSD_Global) {
		if (!be.quiet) print(paste("Started 'ExtractElevation_HWSD_Global' at", Sys.time()))
	
		do_extract <- !complete.cases(elevation_m) | is.na(sites_elevation_source) |
						sites_elevation_source == "Elevation_HWSD_Global"

		if (continueAfterAbort) {
			do_extract <- do_extract & has_nodata(SWRunInformation[runIDs_sites, ], "ELEV_m")
		}

		if (any(do_extract)) {
			if (!be.quiet) print(paste("'ExtractElevation_HWSD_Global' will be extracted for n =", sum(do_extract), "sites"))
			dir.ex.hwsd <- file.path(dir.ex.dem, "HWSD")
	
			#read raster data
			g.elev <- raster(file.path(dir.ex.hwsd, "GloElev_30as.asc"))
			crs_data <- raster::crs(g.elev)
	
			#locations of simulation runs
			sites_hwsd <- run_sites[do_extract, ]
			# Align with data crs
			if (!raster::compareCRS(crs_sites, crs_data)) {
				sites_hwsd <- sp::spTransform(sites_hwsd, CRS = crs_data)	#transform points to grid-coords
			}

			if (sim_cells_or_points == "point") {
				args_extract <- list(x = sites_hwsd)

			} else if (sim_cells_or_points == "cell") {
				cell_res_hwsd <- align_with_target_res(res_from = sim_res, crs_from = sim_crs,
					sp = run_sites[do_extract, ], crs_sp = crs_sites, crs_to = crs_data)
				args_extract <- list(x = cell_res_hwsd, coords = sites_hwsd, method = "block", probs = elev_probs)			
			}

			#extract data for locations
			temp <- round(do.call("extract_from_external_raster", args = c(args_extract, data = list(g.elev))))	# elevation in m a.s.l.
			if (is.vector(temp)) {
				elevation_m[do_extract, "ELEV_m"] <- temp
			} else if (is.array(temp)) {
				elevation_m[do_extract, ] <- temp[, 1, ]
			} else stop("Unknown object returned from 'extract_from_external_raster' when extracting elevation data.")

			i_good <- complete.cases(elevation_m[do_extract, ]) #length(i_good) == sum(do_extract)
			sites_elevation_source[which(do_extract)[!i_good]] <- NA
			
			if (any(i_good)) {
				i_Done <- rep(FALSE, times = runsN_sites) #length(i_Done) == length(runIDs_sites) == runsN_sites
				i_Done[which(do_extract)[i_good]] <- TRUE #sum(i_Done) == sum(i_good)

				sites_elevation_source[i_Done] <- "Elevation_HWSD_Global"
				if (!be.quiet) print(paste("'Elevation_HWSD_Global' was extracted for n =", sum(i_good), "out of", sum(do_extract), "sites"))
			}
			rm(g.elev, sites_hwsd, cell_res_hwsd, dir.ex.hwsd)
		}
		
		if (!be.quiet) print(paste("Finished 'ExtractElevation_HWSD_Global' at", Sys.time()))
	}


	#write data to datafile.SWRunInformation
	icolnew <- !(colnames(elevation_m) %in% colnames(SWRunInformation))
	if (any(icolnew)) {
		SWRunInformation <- cbind(SWRunInformation,
			matrix(NA, nrow = nrow(SWRunInformation), ncol = sum(icolnew),
				dimnames = list(NULL, colnames(elevation_m)[icolnew])))
	}
	SWRunInformation[runIDs_sites, colnames(elevation_m)] <- elevation_m

	SWRunInformation$Elevation_source[runIDs_sites] <- as.character(sites_elevation_source)

	notDone <- is.na(sites_elevation_source)
	include_YN_elev <- rep(0, runsN_master)
	include_YN_elev[runIDs_sites[!notDone]] <- 1
	SWRunInformation$Include_YN_ElevationSources <- include_YN_elev

	write.csv(SWRunInformation, file=file.path(dir.in, datafile.SWRunInformation), row.names=FALSE)
	unlink(file.path(dir.in, datafile.SWRWinputs_preprocessed))
	
	if (any(notDone)) print(paste("Elevation wasn't found for ", sum(notDone), " sites"))
	
	rm(elevation_m, sites_elevation_source, notDone, include_YN_elev)
}

#------END OF ELEVATION------
#--------------------------------------------------------------------------------------------------#


if (exinfo$ExtractSkyDataFromNOAAClimateAtlas_USA || exinfo$ExtractSkyDataFromNCEPCFSR_Global) {
	#allow for multiple sources
	if (extract_determine_database == "SWRunInformation" && "ClimateNormals_source" %in% colnames(SWRunInformation)) {
		sites_monthlyclim_source <- SWRunInformation$ClimateNormals_source[runIDs_sites]
	} else if (extract_determine_database == "order" || !("ClimateNormals_source" %in% colnames(SWRunInformation))) {
		sites_monthlyclim_source <- rep(NA, times = runsN_sites)
	} else {
		stop(paste("Value of 'extract_determine_database'", extract_determine_database, "not implemented"))
	}

	monthlyclim <- array(NA, dim = c(runsN_sites, 3, 12), dimnames = list(NULL, c("RH", "cover", "wind"), NULL))
	
	if (exinfo$ExtractSkyDataFromNOAAClimateAtlas_USA) {
		if (!be.quiet) print(paste("Started 'ExtractSkyDataFromNOAAClimateAtlas_USA' at", Sys.time()))
	
		do_extract <- has_incompletedata(monthlyclim) | is.na(sites_monthlyclim_source) |
						sites_monthlyclim_source == "ClimateNormals_NCDC2005_USA"

		if (continueAfterAbort) {
			do_extract <- do_extract & (
								has_nodata(sw_input_cloud[runIDs_sites, ], "RH") |
								has_nodata(sw_input_cloud[runIDs_sites, ], "SkyC") |
								has_nodata(sw_input_cloud[runIDs_sites, ], "wind"))
		}
		names(do_extract) <- NULL
		
		i_extract <- as.integer(which(do_extract))
		n_extract <- sum(do_extract)

		if (n_extract > 0) {
			if (!be.quiet) print(paste("'ExtractSkyDataFromNOAAClimateAtlas_USA' will be extracted for n =", n_extract, "sites"))
			reference <- "National Climatic Data Center. 2005. Climate maps of the United States. Available online http://cdo.ncdc.noaa.gov/cgi-bin/climaps/climaps.pl. Last accessed May 2010."
	
			#NOAA Climate Atlas: provides no information on height above ground: assuming 2-m which is what is required by SoilWat
			dir.ex.dat <- file.path(dir.ex.weather, "ClimateAtlasUS")
			stopifnot(file.exists(dir.ex.dat), require(raster), require(sp), require(rgdal))
			
			dir_noaaca <- list(
							RH = file.path(dir.ex.dat, "HumidityRelative_Percent"),
							cover = file.path(dir.ex.dat, "Sunshine_Percent"),
							# cover = file.path(dir.ex.dat, "SkyCoverDay_Percent"),
							wind = file.path(dir.ex.dat, "WindSpeed_mph"))
			
			files_shp <- list(
							RH = paste0("RH23", formatC(st_mo, width=2,format="d", flag="0")),
							cover = paste0("SUN52", formatC(st_mo, width=2,format="d", flag="0")),
							# cover = paste0("SKYC50", formatC(st_mo, width=2,format="d", flag="0")),
							wind = paste0("WND60B", formatC(st_mo, width=2,format="d", flag="0")))
			
			var_codes <- list(
							RH = c(10, 23, 31, 41, 51, 61, 71, 78, 90), #percent
							cover = c(11, 26, 36, 46, 56, 66, 76, 86, 96),	#percent
							# cover = c(11, 23, 31, 41, 51, 61, 71, 81, 93),	#percent
							wind = c(1.3, 2.9, 3.3, 3.8, 4.2, 4.7, 5.1, 5.6, 9.6))	#m/s; the last category is actually open '> 12.9 mph': I closed it arbitrarily with 30 mph
			stopifnot(	colnames(monthlyclim) == names(dir_noaaca), 
						colnames(monthlyclim) == names(files_shp), 
						colnames(monthlyclim) == names(var_codes))
	
			#locations of simulation runs
			sites_noaaca <- run_sites[do_extract, ]
			# Align with data crs
			noaaca <- rgdal::readOGR(dsn = dir_noaaca[["RH"]], layer = files_shp[["RH"]][1], verbose = FALSE)
			crs_data <- raster::crs(noaaca)
			if (!raster::compareCRS(crs_sites, crs_data)) {
				sites_noaaca <- sp::spTransform(sites_noaaca, CRS = crs_data)	#transform points to grid-coords
			}

			if (sim_cells_or_points == "point") {
				args_extract <- list(x = sites_noaaca)

			} else if (sim_cells_or_points == "cell") {
				cell_res_noaaca <- align_with_target_res(res_from = sim_res, crs_from = sim_crs,
					sp = sites_noaaca, crs_sp = crs_sites, crs_to = crs_data)
				args_extract <- list(x = cell_res_noaaca, coords = sites_noaaca, crs_data = crs_data)			
			}

			# determine NOAA CA extractions to do
			do_chunks <- parallel::splitIndices(n_extract, ceiling(n_extract / chunk_size.options[["ExtractSkyDataFromNOAAClimateAtlas_USA"]]))
			
			n_vars <- ncol(monthlyclim)
			n_months <- length(st_mo)
			n_chunks <- length(do_chunks)
			iv <- m <- ic <- 1
			
			# determine start location based on interrupted data extraction
			ftemp_noaaca <- file.path(dir.out.temp, "NOAA_ClimateAtlas_extraction.rds")
			
			if (continueAfterAbort && file.exists(ftemp_noaaca)) {
				prev_noaaca <- readRDS(ftemp_noaaca)
				
				if (identical(do_extract, prev_noaaca[["do_extract"]])) { # only continue if same extractions
					monthlyclim[do_extract, , ] <- prev_noaaca[["monthlyclim"]][do_extract, , ]
					
					iv <- prev_noaaca[["iv"]]
					m <- prev_noaaca[["m"]]
					if (identical(do_chunks, prev_noaaca[["do_chunks"]])) {
						ic <- prev_noaaca[["ic"]]
					} else {
						itemp <- max(unlist(prev_noaaca[["do_chunks"]][seq_len(prev_noaaca[["ic"]])]))
						cnewmaxs <- sapply(do_chunks, function(x) max(x))
						ic <- findInterval(itemp, c(0, cnewmaxs))
					}
				}
			}

			#extract data for locations
			if (iv < n_vars ||
				(iv == n_vars && m < n_months) ||
				(iv == n_vars && m == n_months && ic < n_chunks)) repeat {

				if (!be.quiet) print(paste0(Sys.time(), ": 'ExtractSkyDataFromNOAAClimateAtlas_USA' extracting for: ", paste(names(dir_noaaca)[iv], month.name[m], paste("chunk", ic, "of", n_chunks), sep = ", ")))
				
				iextr <- i_extract[do_chunks[[ic]]]
				args_chunk <- args_extract
				args_chunk[["x"]] <- args_chunk[["x"]][do_chunks[[ic]], ]
				if (!is.null(args_chunk[["coords"]]))
					args_chunk[["coords"]] <- args_chunk[["coords"]][do_chunks[[ic]], ]

				monthlyclim[iextr, iv, m] <- do.call("extract_from_external_shapefile",
						args = c(args_chunk,
								file_path = list(dir_noaaca[[iv]]),
								file_shp = list(files_shp[[iv]][m]),
								fields = list("GRIDCODE"),
								code = list(var_codes[[iv]])))
					
				if (ic < n_chunks) {
					ic <- ic + 1
				} else {
					ic <- 1
					m <- m + 1
				}
				if (m > n_months) {
					m <- 1
					iv <- iv + 1
				}

				if (continueAfterAbort) saveRDS(list(monthlyclim = monthlyclim,
													do_extract = do_extract,
													do_chunks = do_chunks,
													iv = iv, m = m, ic = ic),
											file = ftemp_noaaca)

				if (iv > n_vars) break
			}
			
			#subtract from 100% as we want cover and not no-cover
			monthlyclim[do_extract, "cover", ] <- 100 - monthlyclim[do_extract, "cover", ] 


			# Save extracted data to disk
			i_good <- do_extract & !has_incompletedata(monthlyclim) #length(i_good) == length(do_extract) == runsN_sites
			i_notgood <- do_extract & has_incompletedata(monthlyclim) #length(i_good) == length(do_extract) == runsN_sites
			sites_monthlyclim_source[i_notgood] <- NA
			
			if (any(i_good)) {
				sites_monthlyclim_source[i_good] <- "ClimateNormals_NCDC2005_USA"
				if (!be.quiet) print(paste("'ExtractSkyDataFromNOAAClimateAtlas_USA' was extracted for n =", sum(i_good), "out of", n_extract, "sites"))
	
				#add data to sw_input_cloud and set the use flags
				i.temp <- grep("RH", names(sw_input_cloud_use))
				sw_input_cloud_use[i.temp] <- 1
				sw_input_cloud[runIDs_sites[i_good], i.temp[st_mo]] <- round(monthlyclim[i_good, "RH", ], 2)
				i.temp <- grep("SkyC", names(sw_input_cloud_use))
				sw_input_cloud_use[i.temp] <- 1
				sw_input_cloud[runIDs_sites[i_good], i.temp[st_mo]] <- round(monthlyclim[i_good, "cover", ], 2)
				i.temp <- grep("wind", names(sw_input_cloud_use))
				sw_input_cloud_use[i.temp] <- 1
				sw_input_cloud[runIDs_sites[i_good], i.temp[st_mo]] <- round(monthlyclim[i_good, "wind", ], 2)
	
				#write data to datafile.cloud
				write.csv(rbind(sw_input_cloud_use, sw_input_cloud), file = file.path(dir.sw.dat, datafile.cloud), row.names = FALSE)
				unlink(file.path(dir.in, datafile.SWRWinputs_preprocessed))
			
				rm(i.temp)
			}
			rm(dir_noaaca, files_shp, var_codes, sites_noaaca, reference, noaaca, i_good, i_notgood)
		}
	
		if (!be.quiet) print(paste("Finished 'ExtractSkyDataFromNOAAClimateAtlas_USA' at", Sys.time()))
	}
	
	if (exinfo$ExtractSkyDataFromNCEPCFSR_Global) {
		#Citations: Environmental Modeling Center/National Centers for Environmental Prediction/National Weather Service/NOAA/U.S. Department of Commerce. 2010. NCEP Climate Forecast System Reanalysis (CFSR) Monthly Products, January 1979 to December 2010. Research Data Archive at the National Center for Atmospheric Research, Computational and Information Systems Laboratory.
		# http://rda.ucar.edu/datasets/ds093.2/. Accessed 8 March 2012.
		
		if (!be.quiet) print(paste("Started 'ExtractSkyDataFromNCEPCFSR_Global' at", Sys.time()))

		do_extract <- has_incompletedata(monthlyclim) | is.na(sites_monthlyclim_source) |
						sites_monthlyclim_source == "ClimateNormals_NCEPCFSR_Global"

		if (continueAfterAbort) {
			do_extract <- do_extract & (
								has_nodata(sw_input_cloud[runIDs_sites, ], "RH") |
								has_nodata(sw_input_cloud[runIDs_sites, ], "SkyC") |
								has_nodata(sw_input_cloud[runIDs_sites, ], "wind"))
		}
		names(do_extract) <- NULL
	
		if (any(do_extract)) {		
			if (!be.quiet) print(paste("'ExtractSkyDataFromNCEPCFSR_Global' will be extracted for n =", sum(do_extract), "sites"))

			#locations of simulation runs
			locations <- SWRunInformation[runIDs_sites[do_extract], c("WeatherFolder", "X_WGS84", "Y_WGS84")]
			# do the extractions
			temp <- try(get_NCEPCFSR_data(
							dat_sites = locations,
							daily = FALSE,
							monthly = TRUE,
							yearLow = startyr,
							yearHigh = endyr,
							n_site_per_core = chunk_size.options[["ExtractSkyDataFromNCEPCFSR_Global"]],
							cfsr_so = prepd_CFSR$cfsr_so,
							dir.in.cfsr = prepd_CFSR$dir.in.cfsr,
							dir.temp = dir.out.temp,
							rm_mc_files = TRUE),
						silent = TRUE)
			if (inherits(temp, "try-error")) stop(temp)

			#match weather folder names in case of missing extractions
			res <- as.matrix(temp[["res_clim"]][, -1])
			irow <- match(locations[, "WeatherFolder"], table = temp[["res_clim"]][, "WeatherFolder"], nomatch = 0)
			irowL <- irow > 0
			monthlyclim[do_extract, "RH", ][irowL, ] <- res[irow, grepl("RH", colnames(res))]
			monthlyclim[do_extract, "cover", ][irowL, ] <- res[irow, grepl("Cloud", colnames(res))]
			monthlyclim[do_extract, "wind", ][irowL, ] <- res[irow, grepl("Wind", colnames(res))]
		
			# Save extracted data to disk
			i_good <- do_extract & !has_incompletedata(monthlyclim) #length(i_good) == sum(do_extract) == runsN_sites
			i_notgood <- do_extract & has_incompletedata(monthlyclim) #length(i_good) == sum(do_extract) == runsN_sites
			sites_monthlyclim_source[i_notgood] <- NA
			
			if (any(i_good)) {
				sites_monthlyclim_source[i_good] <- "ClimateNormals_NCEPCFSR_Global"
				if (!be.quiet) print(paste("'ExtractSkyDataFromNCEPCFSR_Global' was extracted for n =", sum(i_good), "out of", sum(do_extract), "sites"))

				#add data to sw_input_cloud and set the use flags
				i.temp <- grep("RH", names(sw_input_cloud_use))
				sw_input_cloud_use[i.temp] <- 1
				sw_input_cloud[runIDs_sites[i_good], i.temp][, st_mo] <- round(monthlyclim[i_good, "RH", ], 2)
				i.temp <- grep("SkyC", names(sw_input_cloud_use))
				sw_input_cloud_use[i.temp] <- 1
				sw_input_cloud[runIDs_sites[i_good], i.temp][, st_mo] <- round(monthlyclim[i_good, "cover", ], 2)
				i.temp <- grep("wind", names(sw_input_cloud_use))
				sw_input_cloud_use[i.temp] <- 1
				sw_input_cloud[runIDs_sites[i_good], i.temp][, st_mo] <- round(monthlyclim[i_good, "wind", ], 2)

				#write data to datafile.cloud
				write.csv(rbind(sw_input_cloud_use, sw_input_cloud), file=file.path(dir.sw.dat, datafile.cloud), row.names=FALSE)
				unlink(file.path(dir.in, datafile.SWRWinputs_preprocessed))
		
				rm(i.temp)
			}
			rm(locations, temp, i_good, i_notgood, irow, irowL)
		}
		rm(do_extract)
	
		if (!be.quiet) print(paste("Finished 'ExtractSkyDataFromNCEPCFSR_Global' at", Sys.time()))
	}

	#write data to datafile.SWRunInformation
	SWRunInformation$ClimateNormals_source[runIDs_sites] <- as.character(sites_monthlyclim_source)

	notDone <- is.na(sites_monthlyclim_source)
	include_YN_climnorm <- rep(0, runsN_master)
	include_YN_climnorm[runIDs_sites[!notDone]] <- 1
	SWRunInformation$Include_YN_ClimateNormalSources <- include_YN_climnorm

	write.csv(SWRunInformation, file = file.path(dir.in, datafile.SWRunInformation), row.names = FALSE)
	unlink(file.path(dir.in, datafile.SWRWinputs_preprocessed))
	
	if (any(notDone)) print(paste("Climate normals weren't found for", sum(notDone), "sites"))

	rm(monthlyclim, sites_monthlyclim_source, notDone, include_YN_climnorm)
}

#--------------------------------------------------------------------------------------------------#

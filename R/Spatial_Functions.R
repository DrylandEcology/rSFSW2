#' Extract raster data for point or cell locations
#'
#' Several methods defined for different use cases described below.
#'
#' @param x A \linkS4class{Raster} object from which data are extracted.
#' @param y Locations for which data, provided by \code{x}, are extracted.
#' @param type A character string. One of 'point' or 'cell'. \itemize{
#'  \item If \code{type == "point"}, then y represents point locations
#'        by a two-column matrix or data.frame, by \linkS4class{SpatialPoints},
#'        or by a numeric vector of cell numbers.
#'  \item If \code{type == "cell"}, then y represents cells locations, see
#'        \code{\link{extract_SFSW2_cells_from_raster}}.
#'  }
#'
#' @seealso \code{\link[raster]{extract}}
#'
#' @importClassesFrom raster Raster
#' @importClassesFrom sp SpatialPolygons SpatialPoints
#'
#' @export
#' @name extract_rSFSW2
NULL

setGeneric("extract_rSFSW2", function(x, y, type, ...)
  standardGeneric("extract_rSFSW2"))



#' Extract the weighted mean (and sample quantiles) for raster cells or rectangles.
#'
#' @param x A raster* object from which data are extracted
#' @param y Either A RasterLayer OR raster resolution (of rectangles) as a numeric vector
#'    of length two or a matrix with two columns.
#'    If a RasterLayer, then values of \code{data} are resampled and extracted for !NA
#'    cells.
#'    If the latter, then the vector or matrix represents the rectangle extent/resolution
#'    in x- and y-coordinates.
#'    If a matrix, then rows must match \code{coords}.
#' @param \dots
#'  \itemize{
#'    \item \code{method} A character string. The method argument passed to
#'      \code{reaggregate_raster}. Default is 'block' which is the fastest.
#'    \item \code{coords} Cell centers (corresponding to !NA cells of \code{y}) that are
#'      represented by a two-column matrix of xy coordinates. If not provided, then
#'      extracted from \code{y}.
#'    \item \code{probs} A numeric vector of probabilities with values in \code{[0, 1]} at
#'      which sample quantiles are returned.
#'  }
#' @seealso \code{\link[raster]{extract}}
#' @return A matrix with rows corresponding to the !NA cells of \code{y} and columns to
#'   layers of \code{x}.
#' @export
extract_SFSW2_cells_from_raster <- function(x, y, ...) {
  stopifnot(requireNamespace("raster"), requireNamespace("sp"))

  dots <- list(...)

  if (!("method" %in% names(dots)))
    dots[["method"]] <- "block"

  if (!("coords" %in% names(dots))) {
    dots[["coords"]] <- raster::xyFromCell(y, cell = seq_len(raster::ncell(y)))
  } else {
    dots[["coords"]] <- sp::coordinates(dots[["coords"]])
  }
  if (nrow(dots[["coords"]]) == 0)
    return(matrix(NA, ncol = raster::nlayers(x)))

  if (!("probs" %in% names(dots)))
    dots[["probs"]] <- NA

  if (inherits(x, "Raster")) {
    to_res <- raster::res(y)
  } else {
    if (all(is.vector(y), length(x) == 2L, y > 0) ||  # y is x- and y-resolution
      all(is.matrix(y), ncol(y) == 2L, nrow(y) == nrow(dots[["coords"]]), y > 0)) {  # y are x- and y-resolution for each coord
      to_res <- y
    } else {
      return(matrix(NA, ncol = raster::nlayers(x)))
    }
  }

  reagg <- reaggregate_raster(x = x, coords = dots[["coords"]], to_res = to_res,
    with_weights = TRUE, method = dots[["method"]], tol = 1e-2)

  weighted.agg(reagg, probs = dots[["probs"]])
}


extract_SFSW2_default <- function(x, y, type, ...) {
  if (identical(type, "point")) {
    stopifnot(requireNamespace("raster"))
    raster::extract(x = x, y = y, ...)
  } else if (identical(type, "cell")) {
    extract_SFSW2_cells_from_raster(x, y, ...)
  } else {
    NULL
  }
}

#' @rdname extract_rSFSW2
setMethod("extract_rSFSW2",
  signature(x = "Raster", y = "vector", type = "character"),
  extract_SFSW2_default)
#' @rdname extract_rSFSW2
setMethod("extract_rSFSW2",
  signature(x = "Raster", y = "matrix", type = "character"),
  extract_SFSW2_default)
#' @rdname extract_rSFSW2
setMethod("extract_rSFSW2",
  signature(x = "Raster", y = "data.frame", type = "character"),
  function(x, y, type, ...) {
    if (identical(type, "point")) {
      stopifnot(requireNamespace("raster"))
      raster::extract(x = x, y = y, ...)
    } else {
      NULL
    }
  })
#' @rdname extract_rSFSW2
setMethod("extract_rSFSW2",
  signature(x = "Raster", y = "SpatialPoints", type = "character"),
  function(x, y, type, ...) {
    if (identical(type, "point")) {
      stopifnot(requireNamespace("raster"))
      raster::extract(x = x, y = y, ...)
    } else {
      NULL
    }
  })
#' @rdname extract_rSFSW2
setMethod("extract_rSFSW2",
  signature(x = "Raster", y = "Raster", type = "character"),
  function(x, y, type, ...) {
    if (identical(type, "cell")) {
      extract_SFSW2_cells_from_raster(x, y, ...)
    } else {
      NULL
    }
  })


#' Extract spatial polygon data for point locations
#'
#' @param x An object inheriting from \linkS4class{SpatialPolygons} from which data are
#'  extracted.
#' @param y graphics::points represented by an object inheriting from
#'  \linkS4class{SpatialPoints}.
#' @param file_shp A character string. The filename of the shapefile.
#' @param fields A character vector. If not \code{NULL}, then \code{fields} selects
#'  columns of the extracted object.
#' @param code A vector. If not \code{NULL}, then the extracted data are treated as
#'  integer codes of a factor whose levels are encoded by \code{code}.
#'
#' @seealso \code{\link[sp]{over}}
#'
#' @return A vector or matrix with length/rows corresponding to the elements of \code{y}
#'  and available columns requested by \code{fields}. If \code{!is.null(code)},
#'  then the encoded 'factor levels' are returned.
#' @export
extract_SFSW2_points_from_shp <- function(x, y, fields = NULL, code = NULL, ...) {
  stopifnot(requireNamespace("sp"))

  val <- sp::over(x = y, y = x)
  if (!is.null(fields))
    val <- val[, colnames(val) %in% fields, drop = FALSE]

  if (!is.null(code)) apply(val, 2, function(x) code[as.integer(x)]) else val
}

#' @rdname extract_rSFSW2
setMethod("extract_rSFSW2",
  signature(x = "SpatialPolygons", y = "SpatialPoints", type = "character"),
  function(x, y, type, ...) {
    if (identical(type, "point")) {
      extract_SFSW2_points_from_shp(x, y, ...)
    } else {
      NULL
    }
  })

#' @rdname extract_rSFSW2
setMethod("extract_rSFSW2",
  signature(x = "character", y = "ANY", type = "character"),
  function(x, y, type, ...) {
    if (!requireNamespace("rgdal"))
      stop("'extract_rSFSW2' requires package 'rgdal' but it is not available")

    dots <- list(...)
    if (!("file_shp" %in% names(dots)))
      stop("'extract_rSFSW2' requires argument 'file_shp' if 'x' is a character string")

    x <- rgdal::readOGR(dsn = x, layer = dots[["file_shp"]], verbose = FALSE)
    extract_rSFSW2(x = x, y = y, type = type, ...)
  })


#' Extract spatial polygon data for polygons or rectangles.
#'
#' @param x A object inheriting from \linkS4class{SpatialPolygons} from which data are
#'  extracted.
#' @param y Either an object inheriting from \linkS4class{SpatialPolygons} OR
#'  resolution(s) of rectangles as a numeric vector of length two or a matrix with two
#'  columns.
#'    If \linkS4class{SpatialPolygons}, then values of \code{data} are extracted per
#'      polygon and weighted by area.
#'    If the latter, then \code{coords} must be provided. \code{y} is the vector or matrix
#'      representing the rectangle extents in x- and y-coordinates.
#'    If a matrix, then rows must match \code{coords}.
#' @param file_shp A character string. The filename of the shapefile.
#' @param fields A character vector. If not \code{NULL}, then \code{fields} selects
#'  columns of the extracted object.
#' @param code A vector. If not \code{NULL}, then the extracted data are treated as
#'  integer codes of a factor whose levels are encoded by \code{code}.
#' @param \dots \itemize{
#'    \item \code{coords} Cell centers (corresponding to each resolution of \code{y})
#'      that arerepresented by a two-column matrix of xy coordinates. Ignored if \code{y}
#'      is inheriting from \linkS4class{SpatialPolygons}.
#'    \item \code{crs_data} A \linkS4class{CRS} object indicating the coordinate reference
#'      system (CRS) of \code{y} and coords. Ignored if \code{y} is inheriting from
#'      \linkS4class{SpatialPolygons}.
#'    \item \code{probs} A numeric vector of probabilities with values in \code{[0, 1]} at
#'      which sample quantiles are returned.
#'  }
#'
#' @seealso \code{\link[sp]{over}}
#'
#' @return A vector or matrix with length/rows corresponding to the elements of \code{y}
#'  and available columns requested by \code{fields}. If \code{!is.null(code)}, then the
#'  encoded 'factor levels' are returned.
#' @export
extract_SFSW2_cells_from_shp <- function(x, y, fields = NULL, code = NULL, ...) {
  stopifnot(requireNamespace("raster"), requireNamespace("sp"))

  dots <- list(...)
  if (!("probs" %in% names(dots)))
    dots[["probs"]] <- NA

  if (!raster::compareCRS(raster::crs(y), raster::crs(x))) {
    y <- sp::spTransform(y, CRS = raster::crs(x))
  }

  reagg <- reaggregate_shapefile(x = x, by = y, fields = fields, code = code)

  weighted.agg(reagg, probs = dots[["probs"]])
}

#' Convert resolution/rectangles into SpatialPolygons
res_to_polygons <- function(x, y, ...) {
  stopifnot(requireNamespace("sp"))

  dots <- list(...)

  if (!all(c("coords", "crs_data") %in% names(dots)))
    stop("'res_to_polygons' requires arguments 'coords' and 'crds_data'")

  coords <- sp::coordinates(dots[["coords"]])

  if (is.vector(y) && length(x) == 2L && y > 0) {
    y <- matrix(y, ncol = 2)
  }
  stopifnot(is.matrix(y), nrow(y) == 1L || nrow(y) == nrow(coords))

  to_halfres <- y / 2
  cxy <- cbind(coords[, 1] - to_halfres[, 1], coords[, 1] + to_halfres[, 1],
         coords[, 2] - to_halfres[, 2], coords[, 2] + to_halfres[, 2])

  ptemp0 <- lapply(seq_len(nrow(coords)), function(i) matrix(c(cxy[i, 1], cxy[i, 3],
    cxy[i, 1], cxy[i, 4], cxy[i, 2], cxy[i, 4], cxy[i, 2], cxy[i, 3]),
    ncol = 2, byrow = TRUE))
  ptemp1 <- lapply(ptemp0, sp::Polygon)
  ptemp2 <- lapply(seq_along(ptemp1), function(i) sp::Polygons(ptemp1[i], ID = i))

  sp::SpatialPolygons(ptemp2, proj4string = dots[["crs_data"]])
}


#' @rdname extract_rSFSW2
setMethod("extract_rSFSW2",
  signature(x = "SpatialPolygons", y = "SpatialPolygons", type = "character"),
  function(x, y, type, ...) {
    if (identical(type, "cell")) {
      extract_SFSW2_points_from_shp(x, y, ...)
    } else {
      NULL
    }
  })


#' @rdname extract_rSFSW2
setMethod("extract_rSFSW2",
  signature(x = "SpatialPolygons", y = "vector", type = "character"),
  function(x, y, type, ...) {
    if (identical(type, "cell")) {
      y <- res_to_polygons(x, y, ...)

      extract_SFSW2_points_from_shp(x, y, ...)
    } else {
      NULL
    }
  })

#' @rdname extract_rSFSW2
setMethod("extract_rSFSW2",
  signature(x = "SpatialPolygons", y = "matrix", type = "character"),
  function(x, y, type, ...) {
    if (identical(type, "cell")) {
      y <- res_to_polygons(x, y, ...)

      extract_SFSW2_points_from_shp(x, y, ...)
    } else {
      NULL
    }
  })



#-----

add_weights <- function(i, vals, x, cell_blocks, halfres, exts) {
  if (length(cell_blocks[[i]]) > 0) {
    stopifnot(requireNamespace("raster"))

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
}

#' Extract values from Raster* objects that are covered by an extent rectangle.
#'
#' A cell is covered if its center is inside the polygon (but see the weights option for
#'  considering partly covered cells).
#'
#' @inheritParams raster::extract
#' @param y A matrix with four columns, xmin, xmax, ymin, ymax; each row represents the
#   corners of an \code{\linkS4class{Extent}} object.
#' @seealso \code{\link[raster]{extract}}
#'
#' @return A list with one item for each extent of \code{y}.
#'   Each element is a matrix where each row corresponds to one of the cells of \code{x}
#     contained in a SpatialPolygon
#'   and where columns correspond to layers of \code{x}.
#'  If \code{weights} is \code{TRUE}, then an additional last column is added which
#    contains the weights of the rows.
#' @export
extract_blocks <- function(x, y, weights = FALSE) {
  stopifnot(requireNamespace("raster"))

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
}

extract2_Raster_SpatialPolygons <- function(x, ...) {
  stop("Function 'extract2_Raster_SpatialPolygons' is not defined")
}

#' Extract all raster cell values that occur within each rectangle
#'
#' This is similar to \code{\link[raster]{aggregate}} but with more control and information.
#'
#' Available extraction \code{method}s include
#'  \describe{
#'     \item{raster}{Uses the function \code{\link[raster]{extract}}}
#'     \item{raster_con}{Uses the function \code{extract2_Raster_SpatialPolygons}.
#'        This is a modified version of \code{\link[raster]{extract}} where the
#'        'connection' to the raster file is open for the entire extraction call and
#'        not re-opened/closed for each read event. This allows a massive speed-up which
#'        scales with the number of 'rectangles' to be extracted [not implemented].}
#'     \item{block}{Uses the function \code{extract_blocks}.}
#'   }
#' The weighted mean of the extracted values can be calculated as
#'  stats::weighted.mean(values, w = weights)
#'
#' @param x A raster* object from which data are extracted.
#' @param coord A numeric vector of length two or a matrix with two columns. The x and
#'   y coordinates of the center(s) of the rectangle(s).
#' @param to_res A numeric vector of length two. The x- and y-extent of the rectangle(s).
#' @param with_weigths A logical value or \code{NULL}. If \code{NULL}, then code attempts
#'  to determine whether weights are required for the call to
#'  \code{\link[raster]{extract}}.
#' @param method A character string. Selects the extraction method, see details.
#' @param tol A numeric value. The absolute tolerance for deviation used if
#'  \code{is.null(with_weights)}.
#'
#' @return A list of length corresponding to the number of rectangles. Each element is a
#'  list which contains three items each
#'  \describe{
#'    \item{N}{An integer vector. The number of unique values within the rectangle for
#'      each layer of \code{x}.}
#'    \item{values}{A list of numeric vectors. The sorted unique values as vector for
#'      each layer.}
#'    \item{weigths}{A list of numeric vectors. The weights of the \code{values} for each
#'      layer.}
#'  }
#' @export
reaggregate_raster <- function(x, coords, to_res = c(0, 0), with_weights = NULL,
  method = c("raster", "raster_con", "block"), tol = 1e-2) {

  stopifnot(requireNamespace("raster"), requireNamespace("sp"))

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
    ptemp0 <- lapply(seq_len(nrow(coords)), function(i) matrix(c(cxy[i, 1], cxy[i, 3],
      cxy[i, 1], cxy[i, 4], cxy[i, 2], cxy[i, 4], cxy[i, 2], cxy[i, 3]), ncol = 2,
      byrow = TRUE))

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
      #  - to_res is a (whole number) multiple of res(grid)
      fact <- to_res[1, ] / raster::res(x)
      #  - corners of cell_poly align with the origin of grid
      orig <- (c(cxy[1, 1], cxy[1, 3]) - raster::origin(x)) / to_res[1, ]
      with_weights <- !all(sapply(c(fact, orig), function(f) isTRUE(all.equal(round(f),
        f, tolerance = tol, scale = 1))))
    }
  }

  # extract
  nl <- raster::nlayers(x)

  # sval A list with one item for each 'ys'. Each element is a matrix where each
  # row corresponds to one of the cells contained in the 'rectangle' and where columns
  # correspond to layers in 'x' plus the last column which contain the weights of the rows.
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
}



#' The 'weighted mean' (and sample quantiles) of re-aggregation output
#'
#' @param reagg A list. The output object of a call to \code{reaggregate_raster} or to
#'  \code{reaggregate_shapefile}.
#' @param probs A numeric vector of probabilities with values in \code{[0, 1]} at which
#'  sample quantiles are returned or \code{NA}.
#'
#' @return An array. The first dimension corresponds to each rectangle, i.e., a row of
#'  \code{coords};
#' the second dimension corresponds to the layers of the re-aggregated and -sampled
#'  Raster* object \code{x};
#' And the third dimension corresponds to the aggregation type(s) (weighted mean, and
#'  sample quantiles if any).
#'
#' @export
weighted.agg <- function(reagg, probs = NA) {
  stopifnot(requireNamespace("Hmisc"))

  nf <- 1 + if (anyNA(probs)) 0 else length(probs)
  nl <- max(1, sapply(reagg, function(x) length(x[["N"]])))

  res <- array(NA_real_,
    dim = c(length(reagg), nl, nf),
    dimnames = list(NULL, paste0("Layer", seq_len(nl)), c("wmean", if (!anyNA(probs))
    paste0("q", probs))))
  FUN.VALUE <- rep(NA_real_, nf)

  for (k in seq_along(reagg)) {
    res[k, , ] <- t(vapply(seq_along(reagg[[k]][["N"]]), function(i) {
        if (reagg[[k]][["N"]][i] > 0 && reagg[[k]][["fraction"]][[i]] > 0) {
          out <- stats::weighted.mean(reagg[[k]][["values"]][[i]],
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
}


#' function to extract data for raster cells
#'
#' @description This function is slow because of the call to \code{\link[raster]{resample}}.
#'  The result is also too smooth because of 'two' smoothing steps: (i) aggregation and
#'  (ii) 'bilinear' resampling method.
#'
#' @param x A RasterLayer object for which !NA cells, values of 'data' are resampled and
#'  extracted
#' @param data A raster* object from which data are extracted
#' @param \dots
#'  \describe{
#'    \item{method}{A character string. The method used to resample values for the new
#'      RasterLayer, should be "bilinear" for bilinear interpolation, or "ngb" for using
#'      the nearest neighbor.}
#'    \item{coords}{graphics::points represented by a two-column matrix or data.frame, or
#'      SpatialPoints*; SpatialPolygons*; SpatialLines; Extent; or a numeric vector
#'      representing cell numbers.}
#'    \item{crit_v_exclude}{A character string representing a logical expression based
#'      on a variable named 'v'. If present, then the condition(s) are applied to 'data'
#'      before resampling.}
#'  }
#' @seealso \code{\link[raster]{extract}}
#' @return A vector or matrix with length/rows corresponding to the !NA cells of \code{x}
#'  and columns to layers of \code{data}.
#' @export
extract_from_external_raster_old <- function(x, data, ...) {
  stopifnot(requireNamespace("raster"))

  dots <- list(...)  # coords, method
  if (!("method" %in% names(dots))) dots[["method"]] <- "bilinear"
  if ("crit_v_exclude" %in% names(dots)) {
    vexpr <- parse(text = dots[["crit_v_exclude"]])
    fun_crit <- compiler::cmpfun(function(v) {ifelse(is.na(v) | eval(vexpr), NA, v)})
    data <- raster::calc(data, fun = fun_crit)
  }

  data2 <- raster::resample(x = data, y = x, method = dots[["method"]])
  raster::extract(x = data2, y = dots[["coords"]])  # extract by coords == run_sites[do_extract, ] to get the correct order
}



#' Re-aggregation of spatial polygon data by spatial rectangles/polygons
#'
#' Code based on sp:::aggregatePolyWeighted version 1.2.3 and modified to return complete
#'  information and not the area-weigthed sum.
#'
#' @param x A \linkS4class{SpatialPolygons} object from which data are extracted.
#' @param by A \linkS4class{SpatialPolygons} object. The 'extents' representing the
#'  rectangle(s) for which data are re-aggregated.
#' @param fields A character vector. If not \code{NULL}, then \code{fields} selects
#'  columns of the extracted object.
#' @param code A vector. If not \code{NULL}, then the extracted data are treated as
#'  integer codes of a factor whose levels are encoded by \code{code}.
#'
#' @return A list of length corresponding to the number of rectangles. Each element is a
#'  list which contains three items each
#'  \describe{
#'    \item{N}{An integer vector. The number of unique values within the rectangle for
#'      each layer of \code{x}.}
#'    \item{values}{A list of numeric vectors or matrices. The sorted unique values as
#'      vector or matrix for each layer.}
#'    \item{weigths}{A list of numeric vectors. The weights of the \code{values} for each
#'      layer.}
#'  }
#' @export
reaggregate_shapefile <- function(x, by, fields = NULL, code = NULL) {
  # Code from sp:::aggregatePolyWeighted version 1.2.3
  if (!requireNamespace("rgeos", quietly = TRUE)) stop("rgeos required")

  i <- rgeos::gIntersection(x, by, byid = TRUE, drop_lower_td = TRUE)

  # Modified code
  if (is.null(i))
    return(rep(list(list(N = -1, values = NULL, fraction = NULL)), length(by)))

  rect_subs <- t(sapply(i@polygons, function(p) {
          IDs <- as.integer(strsplit(slot(p, name = "ID"), " ")[[1]])
          if (!(length(IDs) == 2))
            stop("IDs contain spaces: this breaks identification after gIntersection()")

          c(area = slot(p, name = "area"),
            ID_data = which(row.names(x) == IDs[1]),
            ID_rect = which(row.names(by) == IDs[2]))
        }))
  subs_agg <- stats::aggregate(rect_subs[, "area"],
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
}



#' Extracts the 'units' argument from a CRS object
#'
#' @param CRS A Raster*, Spatial*, CRS, or character object with a coordinate reference
#'  system (CRS).
#' @return A character string or \code{NA}.
#' @export
crs_units <- function(CRS) {
  stopifnot(requireNamespace("raster"), requireNamespace("rgdal"))

  args_crs <- raster::crs(CRS, asText = TRUE)
  stopifnot(inherits(args_crs, "character") && rgdal::checkCRSArgs(args_crs)[[1]])

  args2 <- strsplit(args_crs, split = "+", fixed = TRUE)[[1]]
  units <- trimws(args2[grep("units", args2)])
  if (length(units) > 0) {
    strsplit(units, split = "=", fixed = TRUE)[[1]][2]
  } else NA
}

#' Aligns 'grid_from' with 'grid_to' for certain cells
#'
#' @param grid_from A RasterLayer object.
#' @param coords A matrix of x and y coordinates, or a SpatialPoints or
#'  SpatialPointsDataFrame object indicating which cells of projected 'grid_from' will
#'  be used.
#' @param grid_to A RasterLayer object.
#' @param crs_to A CRS object or \code{NULL} in which case it will be extracted from
#'  \code{grid_to}.
#'
#' @return A list with two elements
#'  \describe{
#'    \item{x}{A RasterLayer object. Cells values are \code{NA} or 1 if they contain
#'      graphics::points of \code{coords}.}
#'    \item{index}{An integer vector. The cell numbers of \code{x} that correspond to
#'      \code{coords}.}
#' }
#' @export
align_with_target_grid <- function(grid_from, coords, grid_to, crs_to = NULL) {
  stopifnot(requireNamespace("raster"))

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
}


#' Calculate resolution of one coordinate system for points in their coordinate
#'  system transformed to a target coordinate system
#'
#' @param res_from A numeric vector of length two. The resolution in x and y direction in
#'  the coordinate system \code{crs_from}.
#' @param crs_from A CRS object. The coordinate system of \code{res_from}.
#' @param sp A SpatialPoints object. Cell center graphics::points for which new
#'  resolutions will be calculated.
#' @param crs_sp A CRS object. The coordinate system of \code{sp}.
#' @param crs_to A CRS object. The coordinate system in which the resulting resolution
#'  will be calculated.
#'
#' @return A numeric vector of length two (if resolution is constant for each point) or a
#'  matrix with two columns for the x- and y-resolutions per row for each point.
#' @export
align_with_target_res <- function(res_from, crs_from, sp, crs_sp, crs_to) {
  stopifnot(requireNamespace("raster"), requireNamespace("sp"))

  if (identical(crs_units(crs_from), crs_units(crs_to))) {
    res_from
  } else {
    sp_from <- if (raster::compareCRS(crs_sp, crs_from)) {
            sp
          } else {
            # transform graphics::points to same coordinate system as resolution
            sp::spTransform(sp, CRS = crs_from)
          }

    # resolution is constant for every point in crs_from
    coords_from <- sp::coordinates(sp_from)
    from_halfres <- res_from / 2

    # cell corners of each point
    #  - lower left cell corner
    cxy0_from <- cbind(coords_from[, 1] - from_halfres[1], coords_from[, 2] - from_halfres[2])
    cxy0_from_sp <- sp::SpatialPoints(coords = cxy0_from, proj4string = crs_from)
    #  - upper right cell corner
    cxy1_from <- cbind(coords_from[, 1] + from_halfres[1], coords_from[, 2] + from_halfres[2])
    cxy1_from_sp <- sp::SpatialPoints(coords = cxy1_from, proj4string = crs_from)

    # transform cell corners to target coordinate system
    cxy0_to_sp <- sp::spTransform(cxy0_from_sp, CRS = crs_to)
    cxy1_to_sp <- sp::spTransform(cxy1_from_sp, CRS = crs_to)

    # resolution varies with latitude of graphics::points in crs_to
    cxy0_to <- sp::coordinates(cxy0_to_sp)
    cxy1_to <- sp::coordinates(cxy1_to_sp)

    abs(cxy1_to - cxy0_to)
  }
}


#' Set-up information for a spatially aware simulation project
#' @export
setup_spatial_simulation <- function(SFSW2_prj_meta, SFSW2_prj_inputs,
  use_sim_spatial = FALSE) {

  sim_space <- list(scorp = NA, run_sites = NA, sim_raster = NA, crs_sites = NA,
    sim_res = NA, sim_crs = NA)

  #--- Make sure that flag 'scorp' has a valid option
  sim_space[["scorp"]] <- match.arg(SFSW2_prj_meta[["in_space"]][["scorp"]], c("point", "cell"))

  if (use_sim_spatial) {
    stopifnot(requireNamespace("raster"), requireNamespace("sp"), requireNamespace("rgdal"))

    if (sim_space[["scorp"]] == "cell") {
      if (file.exists(SFSW2_prj_meta[["fnames_in"]][["fsimraster"]])) {
        # Make sure sim_raster agrees with sim_res and sim_crs; sim_raster takes priority
        sim_space[["sim_raster"]] <- raster::raster(SFSW2_prj_meta[["fnames_in"]][["fsimraster"]])
        sim_space[["sim_res"]] <- raster::res(SFSW2_prj_meta[["in_space"]][["sim_raster"]])
        sim_space[["sim_crs"]] <- raster::crs(SFSW2_prj_meta[["in_space"]][["sim_raster"]])

      } else {
        sim_space[["sim_res"]] <- SFSW2_prj_meta[["in_space"]][["sim_res"]]
      }

      # Make sure that sim_res is valid
      stopifnot(is.finite(sim_space[["sim_res"]]), length(sim_space[["sim_res"]]) == 2L,
        sim_space[["sim_res"]] > 0)
    }

    #--- Make sure that sim_crs is valid
    if (is.na(sim_space[["sim_crs"]]) && is.character(SFSW2_prj_meta[["in_space"]][["sim_crs"]])) {
      sim_space[["sim_crs"]] <- sp::CRS(SFSW2_prj_meta[["in_space"]][["sim_crs"]])
    }
    #   - package 'raster' must be loaded so that method 'CRS' for 'as.character' is available
    temp <- rgdal::checkCRSArgs(as.character(sim_space[["sim_crs"]]))
    stopifnot(temp[[1]])
    sim_space[["sim_crs"]] <- sp::CRS(temp[[2]])

    #--- SpatialPoints of simulation cell centers/sites in WGS84
    sim_space[["crs_sites"]] <- sp::CRS("+init=epsg:4326")  # epsg:4326 is sp::CRS("+proj = longlat +datum = WGS84 +no_defs")
    sim_space[["run_sites"]] <- sp::SpatialPoints(coords =
      SFSW2_prj_inputs[["SWRunInformation"]][SFSW2_prj_meta[["sim_size"]][["runIDs_sites"]], c("X_WGS84", "Y_WGS84")],
      proj4string = sim_space[["crs_sites"]])

    #--- Create raster from simulation cells if not existing (is this really needed?)
    if (sim_space[["scorp"]] == "cell" && !inherits(sim_space[["sim_raster"]], "Raster")) {
      temp <- sim_space[["run_sites"]]
      ttemp <- try(sp::gridded(temp) <- TRUE)

      if (!inherits(ttemp, "try-error") && isTRUE(ttemp)) {
        sim_space[["sim_raster"]] <- raster::raster(temp)
        cells <- raster::cellFromXY(sim_space[["sim_raster"]],
          sp::coordinates(sim_space[["run_sites"]]))
        sim_space[["sim_raster"]][cells] <- 1L

        raster::writeRaster(sim_space[["sim_raster"]],
          file = SFSW2_prj_meta[["fnames_in"]][["fsimraster"]])

      } else {
        print(paste0("rSFSW2's ", shQuote(match.call()[1]), ": failed to create ",
          "'sim_raster' because 'run_sites' are not gridded even though the project ",
          "description 'sim_space[['scorp']]' declares that they represent cells."))
      }
    }
  }

  SFSW2_prj_meta[["sim_space"]] <- sim_space

  SFSW2_prj_meta
}



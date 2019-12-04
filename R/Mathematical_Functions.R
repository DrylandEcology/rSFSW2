
in_box <- function(xy, xbounds, ybounds, i_use) {
  !i_use &
  xy[, 1] >= xbounds[1] & xy[, 1] <= xbounds[2] &
  xy[, 2] >= ybounds[1] & xy[, 2] <= ybounds[2]
}





#' Index of the closest value in the matrix to the passed in value.
whereNearest <- function(val, matrix) {
  which.min(abs(matrix - val))
}

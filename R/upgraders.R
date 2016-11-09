

#' Upgrade 'datafile.soils' from version 11 to version 12
#'
#' Add a column of total organic content (TOC) for each soil layer
#'
#' @param file A character string. The file path to the datafile to upgrade.
#' @param new_name A character string. The basename of the upgraded file. If \code{NULL}
#'  then the code attempts to replace 'v11' with 'v12' in \code{file}.
#'
#' @return A logical value. \code{TRUE} if upgrade was successful.
upgrade_soilsin_v11_to_v12 <- function(file, new_name = NULL) {
  temp <- file.exists(file)
  if (!temp) return(FALSE)

  if (is.null(new_name)) {
    new_name <- sub("v11", "v12", basename(file))
  }

  temp <- try(swsf_read_inputfile(file))
  if (inherits(temp, "try-error")) return(FALSE)
  data <- temp[["data"]]
  use <- temp[["use"]]
  rm(temp)

  iclay <- grep("Clay", names(use[-1]))
  nlyrs <- length(iclay)
  ncols <- dim(data)[2] - 1
  nvar <- ncols / nlyrs
  temp <- rep(cumsum(rep(1, nlyrs)), each = nvar)
  temp <- c(rep(0, iclay[1]), temp[-((length(temp) - iclay[1] + 1):length(temp))])
  icols_transfer <- c(1, 1 + seq_len(ncols) + temp)
  ncols_new <- dim(data)[2] + length(iclay)

  colnames2 <- rep("", ncols_new)
  colnames2[icols_transfer] <- names(use)
  colnames2[-icols_transfer] <- paste0("TOC_GperKG_L", seq_len(nlyrs))

  use2 <- rep(FALSE, ncols_new)
  use2[icols_transfer] <- use
  names(use2) <- colnames2

  data2 <- data.frame(data[, "Label"],
    matrix(NA, nrow = dim(data)[1], ncol = ncols_new - 1))
  data2[, icols_transfer[-1]] <- data[, -1]
  names(data2) <- colnames2

  # write data to disk
  temp <- write.csv(reconstitute_inputfile(use2, data2),
    file = file.path(dirname(file), new_name), row.names = FALSE)

  is.null(temp)
}

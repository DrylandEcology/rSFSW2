#' Upgrade a 'data input file'
#'
#' Add (a) new column(s)
#'
#' @param file A character string. The file path to the datafile to upgrade.
#' @param new_name A character string. The basename of the upgraded file. If \code{NULL}
#'  then the code attempts to replace 'vOLD' with 'vNEW' in \code{file}.
#'
#' @return A logical value. \code{TRUE} if upgrade was successful.
#' @name upgrade_datafile
upgrade_datafile <- function(file, new_name, insert_after_tag, inserted_colnames) {
  temp <- file.exists(file)
  if (!temp) return(FALSE)

  if (is.null(new_name))
    return(FALSE)

  temp <- try(swsf_read_inputfile(file))
  if (inherits(temp, "try-error")) return(FALSE)
  data <- temp[["data"]]
  use <- temp[["use"]]
  rm(temp)

  icolnext <- grep(insert_after_tag, names(use[-1]))
  ngroups <- length(icolnext)
  ncols <- dim(data)[2] - 1
  temp <- rep(cumsum(rep(1, ngroups + 1)) - 1,
    times = c(icolnext[1], diff(icolnext), ncols - icolnext[length(icolnext)]))
  icols_transfer <- c(1, 1 + seq_len(ncols) + temp)
  ncols_new <- dim(data)[2] + length(icolnext)

  colnames2 <- rep("", ncols_new)
  colnames2[icols_transfer] <- names(use)
  colnames2[-icols_transfer] <- inserted_colnames[seq_len(ngroups)]

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


#' Upgrade 'datafile.prod' from version 10 to version 11
#'
#' Add a column of critical soil water potential (SWPcrit) for each functional type
#'
#' @rdname upgrade_datafile
upgrade_prodin_v10_to_v11 <- function(file, new_name = NULL) {
  if (is.null(new_name)) {
    new_name <- sub("v10", "v11", basename(file))
  }

  upgrade_datafile(file, new_name, insert_after_tag = "HydRed_OnOff",
    inserted_colnames = paste0(c("Grass", "Shrub", "Tree", "Forb"), "_SWPcrit_MPa"))
}


#' Upgrade 'datafile.soils' from version 11 to version 12
#'
#' Add a column of total organic content (TOC) for each soil layer
#'
#' @rdname upgrade_datafile
upgrade_soilsin_v11_to_v12 <- function(file, new_name = NULL) {
  if (is.null(new_name)) {
    new_name <- sub("v11", "v12", basename(file))
  }

  upgrade_datafile(file, new_name, insert_after_tag = "Clay",
    inserted_colnames = paste0("TOC_GperKG_L", seq_len(100)))
}

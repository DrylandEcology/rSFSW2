#!/usr/bin/env Rscript

#------ Settings for script ------
prj_tag <- "EDGE"

do_overwrite_figs <- FALSE

do_continue <- TRUE
do_subfolders <- FALSE
experiments <- "DefaultSettings" # "DefaultSettings"
width_simtag <- 3
out_format <- "csv" # "rds" or "csv

exlcude_varExpTrt <- "Exp_Block"
max_depth_cm <- 50

used_years <- 2015:2019 # set to NULL to use all simulated years

shelterdates <- data.frame(
  spring = c("2015-04-29", "2016-04-27", "2017-04-26", "2018-04-27", "2019-04-25"),
  fall = c("2015-11-05", "2016-10-26", "2017-10-19", "2018-10-31", NA)
)


#----- Command Line Arguments ------
# Argument passed to this script:
#   - args[1] = Relative path to rSFSW2 simulation folder
# Example: Rscript SFSW2_project_CheckSharedData.R ../../1_SOILWAT2_Simulations/20221012_PonderosaDrought_SOILWAT2_simulations ../20221012_PonderosaDrought_SOILWAT2_simulations/20221118_SOILWAT2_OutputShared__20221012_PonderosaDrought_SOILWAT2_simulations

args <- commandArgs(trailingOnly = TRUE)

prjs <- if (length(args) == 0) {
  list(
    c(
      sim = paste0("20231017_", prj_tag, "_SOILWAT2_simulations"),
      out = "20231127"
    ),
    c(
      sim = paste0("20240104_", prj_tag, "_SOILWAT2_simulations"),
      out = "20240105"
    )
  )
} else {
  "has_CLAs"
}


for (kp in seq_along(prjs)) {

  if (!identical(prjs, "has_CLSa")) {
    args <- c(
      file.path("../../2_SOILWAT2_Simulations", prjs[[kp]][["sim"]]),
      file.path("..", prjs[[kp]][["sim"]], "Outputs"),
      file.path(
        "..",
        prjs[[kp]][["sim"]],
        paste0(prjs[[kp]][["out"]], "_SOILWAT2_OutputShared__", prjs[[kp]][["sim"]])
      ),
      prj_tag
    )
  }


  #------
  dir_prj <- "."
  dir_sim <- file.path(dir_prj, args[[1L]])
  cat("\nrSFSW2 simulation folder:", args[[1L]], fill = TRUE)
  stopifnot(dir.exists(dir_sim))
  tag_sim <- paste(
    strsplit(basename(dir_sim), split = "_", fixed = TRUE)[[1L]][2:3],
    collapse = "_"
  )

  dir_metrics <- file.path(dir_prj, args[[2L]])
  stopifnot(dir.exists(dir_metrics))

  dir_shared <- file.path(dir_prj, args[[3L]])
  stopifnot(dir.exists(dir_shared))
  cat("Shared output folder:", dir_shared, fill = TRUE)

  dir_fig <- file.path(dirname(dir_shared), paste0("Figures__", basename(args[[3L]])))
  dir.create(dir_fig, recursive = TRUE, showWarnings = FALSE)

  dir_outs <- file.path(dirname(dir_shared), "Outputs")

  experiments <- args[[4L]]


  #--- Load simulation project ---
  SFSW2_prj_meta <- rSFSW2::init_rSFSW2_project(
    fmetar = file.path(dir_sim, "SFSW2_project_descriptions.R"),
    update = FALSE,
    chdir = TRUE
  )

  tmp <- rSFSW2:::process_inputs(
    project_paths = SFSW2_prj_meta[["project_paths"]],
    fnames_in = SFSW2_prj_meta[["fnames_in"]],
    use_preprocin = TRUE,
    verbose = FALSE
  )

  SWRunInformation <- tmp[["SWRunInformation"]]
  sw_input_soillayers <- tmp[["sw_input_soillayers"]]
  sw_input_soils <- tmp[["sw_input_soils"]]

  varExpTrt <- setdiff(
    grep("^Exp_", colnames(SWRunInformation), value = TRUE),
    exlcude_varExpTrt
  )

  expTrts <- if (length(varExpTrt) > 0) {
    apply(SWRunInformation[, varExpTrt, drop = FALSE], 1, paste, collapse = "-")
  }


  sim_size <- SFSW2_prj_meta[["sim_size"]]
  runsN_main <- if ("runsN_main" %in% names(sim_size)) {
    sim_size[["runsN_main"]]
  } else {
    sim_size[["runsN_master"]]
  }
  expN <- sim_size[["expN"]]
  runIDs_sites <- sim_size[["runIDs_sites"]]
  runsN_total <- sim_size[["runsN_total"]]

  years <- SFSW2_prj_meta[["sim_time"]][["useyrs"]]
  if (!is.null(used_years)) years <- used_years


  #--- Find shared output files ------
  ftmp <- list.files(dir_shared, pattern = paste0(".", out_format), full.names = TRUE)

  res_label <- sapply(
    strsplit(
      sub(paste0(".", out_format), "", basename(ftmp)),
      split = "_",
      fixed = TRUE
    ),
    function(x) paste0(x[-(1:2)], collapse = "_")
  )

  stopifnot(length(res_label) == runsN_total, length(runsN_total) > 0, runsN_total > 0)



  #--- Detect variables ------
  fname_data_vars <- file.path(dir_outs, "data__variables.rds")

  if (file.exists(fname_data_vars)) {
    list_daily_vars <- readRDS(fname_data_vars)

  } else {
    pb <- utils::txtProgressBar(max = length(ftmp), style = 3)
    list_daily_vars <- NULL

    for (k in seq_along(ftmp)) {
      x <- switch(
        EXPR = out_format,
        csv = read.csv(ftmp[k], nrows = 1, check.names = FALSE),
        rds = readRDS(ftmp[k])
      )

      list_daily_vars <- unique(c(
        list_daily_vars,
        grep("Sim_|Input_", colnames(x), value = TRUE)
      ))

      utils::setTxtProgressBar(pb, k)
    }

    close(pb)

    list_daily_vars <- sort(list_daily_vars)
    saveRDS(list_daily_vars, file = fname_data_vars)
  }


  var_ppt <- grep("_PPT_", list_daily_vars, value = TRUE)
  var_pptcum <- sub("PPT_", "PPTcum_", var_ppt)
  var_vwc <- grep("_VWC_", list_daily_vars, value = TRUE)

  id <- grep(
    paste0("to", formatC(max_depth_cm, width = 3, flag = 0), "_cm$"),
    var_vwc
  )

  var_vwc <- var_vwc[seq_len(id)]

  stopifnot(
    length(var_ppt) == 1L,
    length(var_vwc) > 0L
  )


  #--- Prepare data containers ------
  fname_data_ts_daily <- file.path(dir_outs, "data-vwc__ts_daily.rds")
  do_ts_daily <- !file.exists(fname_data_ts_daily)


  #--- Read data ------
  if (!do_ts_daily) {
    res_ts_daily <- readRDS(fname_data_ts_daily)

  } else {
    template_res_ts_daily <- as.data.frame(
      array(
        dim = c(0L, 4L + length(var_ppt) + length(var_vwc)),
        dimnames = list(
          NULL,
          c("Site", "Year", "DOY", "Date", var_ppt, var_vwc)
        )
      )
    )
  }


  if (do_ts_daily) {
    pb <- utils::txtProgressBar(max = length(ftmp), style = 3)

    tmp_res <- list()

    for (k in seq_along(ftmp)) {
      x <- switch(
        EXPR = out_format,
        csv = read.csv(ftmp[k], check.names = FALSE),
        rds = readRDS(ftmp[k])
      )

      tmp_vars_used <- intersect(colnames(x), c("Year", "DOY", var_ppt, var_vwc))

      tmp_tmp <- template_res_ts_daily
      tmp_tmp[seq_len(nrow(x)), tmp_vars_used] <- x[, tmp_vars_used, drop = FALSE]

      tmp_tmp[, "Site"] <- res_label[[k]]
      tmp_tmp[, "Date"] <- as.Date(
        paste(tmp_tmp[["Year"]], tmp_tmp[["DOY"]], sep = "-"),
        format = "%Y-%j"
      )

      tmp_res[[k]] <- tmp_tmp

      utils::setTxtProgressBar(pb, k)
    }

    res_ts_daily <- do.call(rbind, tmp_res)

    close(pb)

    #--- * Experimental treatments ------
    if (!is.null(expTrts)) {
      res_ts_daily[["ExpTrt"]] <- res_ts_daily[["fExpTrt"]] <- NA

      #ids <- match(tmp_data2[["Site"]], SWRunInformation[["Label"]], nomatch = 0)
      ids <- unlist(
        lapply(
          seq_along(SWRunInformation[["Label"]]),
          function(kl) {
            tmp <- grep(SWRunInformation[["Label"]][[kl]], res_ts_daily[["Site"]])
            if (length(tmp) > 0) rep(kl, length(tmp)) else 0L
          }
        )
      )
      res_ts_daily[ids > 0, "ExpTrt"] <- expTrts[ids]
      res_ts_daily[ids > 0, "fExpTrt"] <- factor(expTrts)[ids]



      #--- ** Shelter period ------
      res_ts_daily[["shelter"]] <- res_ts_daily[["shelterperiod"]] <- NA_character_
      tmp <- res_ts_daily[["Date"]]

      # Pre-treatment
      ids <- tmp < shelterdates[1L, "spring"]
      res_ts_daily[["shelter"]][ids] <- "Pre"
      res_ts_daily[["shelterperiod"]][ids] <- "Pre"

      # Warm-season rainout shelters
      idsi <- list(
        tmp >= shelterdates[1L, "spring"] & tmp < shelterdates[1L, "fall"],
        tmp >= shelterdates[2L, "spring"] & tmp < shelterdates[2L, "fall"],
        tmp >= shelterdates[3L, "spring"] & tmp < shelterdates[3L, "fall"],
        tmp >= shelterdates[4L, "spring"] & tmp < shelterdates[4L, "fall"]
      )
      for (ki in seq_along(idsi)) {
        res_ts_daily[["shelterperiod"]][idsi[[ki]]] <- paste0("Warm", ki)
      }
      ids <- idsi[[1L]] | idsi[[2L]] | idsi[[3L]] | idsi[[4L]]
      res_ts_daily[["shelter"]][ids] <- "Warm"

      # Cool-season rainout shelters
      idsi <- list(
        tmp >= shelterdates[1L, "fall"] & tmp < shelterdates[2L, "spring"],
        tmp >= shelterdates[2L, "fall"] & tmp < shelterdates[3L, "spring"],
        tmp >= shelterdates[3L, "fall"] & tmp < shelterdates[4L, "spring"],
        tmp >= shelterdates[4L, "fall"] & tmp < shelterdates[5L, "spring"]
      )
      for (ki in seq_along(idsi)) {
        res_ts_daily[["shelterperiod"]][idsi[[ki]]] <- paste0("Cool", ki)
      }
      ids <- idsi[[1L]] | idsi[[2L]] | idsi[[3L]] | idsi[[4L]]
      res_ts_daily[["shelter"]][ids] <- "Cool"

      # Post-treatment
      ids <- tmp >= shelterdates[5L, "spring"]
      res_ts_daily[["shelter"]][ids] <- "Post"
      res_ts_daily[["shelterperiod"]][ids] <- "Post"

      stopifnot(
        !anyNA(res_ts_daily[["shelter"]]),
        !anyNA(res_ts_daily[["shelterperiod"]])
      )


      #--- ** Calculate cumulative precipitation per shelter period ------
      res_ts_daily[[var_pptcum]] <- NA_real_

      for (site in unique(res_ts_daily[["Site"]])) {
        id_site <- res_ts_daily[["Site"]] %in% site

        ppt <- res_ts_daily[id_site, var_ppt]
        sp <- res_ts_daily[id_site, "shelterperiod"]

        res_ts_daily[id_site, var_pptcum] <- unlist(
          lapply(
            unique(sp),
            function(p) {
              cumsum(ppt[sp == p])
            }
          )
        )
      }
    }

    saveRDS(res_ts_daily, file = fname_data_ts_daily)
  }




  #--- Create plots ------

  #--- * Functions ------

  #' Convert PNGs to (bitmap) PDF
  #' see https://stackoverflow.com/questions/27399672/r-bitmap-output-in-pdf
  to_bitmap_pdf <- function(filename, size_page, n_pages = NULL, delete = TRUE) {

    tmp_fnames <- list.files(
      path = dirname(filename),
      pattern = paste0(
        sub(".pdf", "", basename(filename)),
        "[0-9][0-9][0-9][.]png"
      ),
      full.names = TRUE
    )
    stopifnot(length(tmp_fnames) == n_pages)

    grDevices::pdf(
      file = filename,
      height = size_page[1],
      width = size_page[2]
    )
    par_prev <- graphics::par(mar = rep(0, 4))

    for (i in seq_along(tmp_fnames)) {
      plot(c(0,1), c(0,1), type = "n")
      graphics::rasterImage(
        png::readPNG(tmp_fnames[i]),
        xleft = 0,
        ybottom = 0,
        xright = 1,
        ytop = 1,
        interpolate = FALSE
      )
    }

    graphics::par(par_prev)
    dev.off()

    if (isTRUE(delete)) {
      unlink(tmp_fnames)
    }

    invisible(filename)
  }


  #--- * Figures ------
  colors_Hoover2021Fig2 <- c("seagreen", "limegreen", "navy", "blue", "red4", "orange3")
  pal_Hoover2021Fig2 <- grDevices::colorRampPalette(colors_Hoover2021Fig2)

  for (ktype in c("Values")) {


    #--- ** Time-series plot of daily values ------
    fname_ts_daily__vwc <- file.path(
      dir_fig,
      paste0(
        "Fig_", experiments,
        "_VWC_",
        years[[1L]], "-", years[[length(years)]],
        "_Daily_TimeSeries_", ktype,
        ".pdf"
      )
    )

    if (do_overwrite_figs || !file.exists(fname_ts_daily__vwc)) {
      tmp_data <- switch(
        EXPR = ktype,
        Values = res_ts_daily
      )

      ids <- tmp_data[["Year"]] %in% years

      tmp_data2 <- tidyr::pivot_longer(
        tmp_data[ids, , drop = FALSE],
        cols = tidyselect::all_of(c(var_pptcum, var_vwc)),
        names_to = "Variable",
        values_to = "Value"
      ) |>
        as.data.frame()

      if (!is.null(expTrts)) {
        ids <- order(tmp_data2[["ExpTrt"]], tmp_data2[["Site"]])
        tmp_data2[["Site"]] <- factor(
          as.character(tmp_data2[["Site"]]),
          levels = unique(tmp_data2[["Site"]][ids])
        )

        tmp_data2_ExpTrt <- tmp_data2 |>
          dplyr::group_by(ExpTrt, Date, Variable) |>
          dplyr::summarise(
            TrtMean = mean(Value),
            TrtLow = quantile(Value, probs = 0.05),
            TrtHigh = quantile(Value, probs = 0.95)
          ) |>
          as.data.frame()
      }


      #--- Create PNGs and then convert to (bitmap) PDF
      # (vector) PDF are too large and slow
      #n_panels <- grDevices::n2mfrow(length(unique(tmp_data2[["Variable"]])))
      n_panels <- c(length(unique(tmp_data2[["Variable"]])), 1L)
      size_page <- c(2.5 * n_panels[1], 10 * n_panels[2])

      grDevices::png(
        filename = sub(".pdf", "%03d.png", fname_ts_daily__vwc),
        height = size_page[1],
        width = size_page[2],
        units = "in",
        res = 150
      )

      list_plots <- NULL
      n_pages <- Inf
      k <- 1

      while (k <= n_pages) {

        tmp <- ggplot2::ggplot(
          data = tmp_data2,
          mapping = ggplot2::aes(x = Date, y = Value)
        ) +
          ggplot2::scale_x_date(
            limits = range(as.Date(unlist(shelterdates)), na.rm = TRUE),
            date_minor_breaks = "month"
          ) +
          ggplot2::scale_y_continuous(limits = c(0, NA)) +
          ggplot2::scale_color_viridis_d(guide = "none", option = "magma") +
          #ggplot2::scale_color_manual(guide = "none", palette = pal_Hoover2021Fig2) +
          ggplot2::geom_line(
            alpha = 0.2,
            ggplot2::aes(color = Site),
            show.legend = FALSE
          )


        if (!is.null(expTrts)) {
          tmp <- tmp +
            ggplot2::scale_fill_viridis_d(option = "magma") +
            #ggplot2::scale_fill_manual(palette = pal_Hoover2021Fig2) +
            ggplot2::geom_ribbon(
              data = tmp_data2_ExpTrt,
              ggplot2::aes(
                x = Date,
                y = TrtMean,
                ymin = TrtLow,
                ymax = TrtHigh,
                fill = ExpTrt
              ),
              alpha = 0.3,
              show.legend = TRUE
            ) +
            ggnewscale::new_scale_color() +
            ggplot2::scale_color_viridis_d(option = "magma") +
            #ggplot2::scale_color_manual(palette = pal_Hoover2021Fig2) +
            ggplot2::geom_line(
              data = tmp_data2_ExpTrt,
              ggplot2::aes(x = Date, y = TrtMean, color = ExpTrt),
              linewidth = 1,
              linetype = "dashed"
            )
        }

        tmp <- tmp +
          ggplot2::geom_vline(
            xintercept = as.Date(unlist(shelterdates))
          )

        tmp <- tmp +
          #ggplot2::facet_wrap(ggplot2::vars(Variable), scales = "free") +
          ggforce::facet_wrap_paginate(
            ggplot2::vars(Variable),
            scales = "free",
            nrow = n_panels[1],
            ncol = n_panels[2],
            page = k
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            legend.position = "top"
          )

        print(tmp)

        if (is.infinite(n_pages)) {
          n_pages <- ggforce::n_pages(tmp)
        }

        k <- k + 1
      }

      dev.off()


      to_bitmap_pdf(
        filename = fname_ts_daily__vwc,
        size_page = size_page,
        n_pages = n_pages
      )
    }

  }
}

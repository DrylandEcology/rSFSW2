#!/usr/bin/env Rscript

#------ Settings for script ------
prj_tag <- "PIPOFutureManagement"

do_overwrite_figs <- FALSE

do_continue <- TRUE
do_subfolders <- FALSE
removeIncompleteSoilLayers <- TRUE

experiments <- "DefaultSettings" # "DefaultSettings"
ids_scen_used <- c(1L, 2L, 91L) # NULL
width_simtag <- 3
out_format <- "rds" # "rds" or "csv

exclude_varExpTrt <- c("ExpTrt_BasalArea", "ExpTrt_Litter")

simSoilLayers <- c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 201)


#----- Command Line Arguments ------
# Argument passed to this script:
#   - args[1] = Relative path to rSFSW2 simulation folder
# Example: Rscript SFSW2_project_CheckSharedData.R ../../1_SOILWAT2_Simulations/20221012_PonderosaDrought_SOILWAT2_simulations ../20221012_PonderosaDrought_SOILWAT2_simulations/20221118_SOILWAT2_OutputShared__20221012_PonderosaDrought_SOILWAT2_simulations

args <- commandArgs(trailingOnly = TRUE)

prjs <- if (length(args) == 0) {
  list(
    c(
      sim = paste0("20241029_", prj_tag, "_SOILWAT2_simulations"),
      out = "20250114"
    )
  )
} else {
  "has_CLAs"
}


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
    height = size_page[[1L]],
    width = size_page[[2L]]
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
  grDevices::dev.off()

  if (isTRUE(delete)) {
    unlink(tmp_fnames)
  }

  invisible(filename)
}


#--- Loop over projects ------
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

  dir_outs <- file.path(dirname(dir_shared), "Outputs_Checks")
  dir.create(dir_outs, recursive = TRUE, showWarnings = FALSE)

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
  if (TRUE) {
    sw_input_prod <- tmp[["sw_input_prod"]]
    sw_input_soillayers <- tmp[["sw_input_soillayers"]]
    sw_input_soils <- tmp[["sw_input_soils"]]
    xveg <- try(readRDS(file.path(dir_metrics, "veg_biomass_annual.rds")))
    xcov <- try(readRDS(file.path(dir_metrics, "LandCover_annualClim.rds")))
  }

  varExpTrt <- setdiff(
    grep("^ExpTrt_", colnames(SWRunInformation), value = TRUE),
    exclude_varExpTrt
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

  stopifnot(length(runsN_total) > 0, runsN_total > 0)

  sim_scens <- SFSW2_prj_meta[["sim_scens"]]
  Nscen <- nrow(sim_scens[["df"]])

  idsScens <- if (is.null(ids_scen_used)) {
    seq_len(Nscen)
  } else {
    ids_scen_used[ids_scen_used <= Nscen]
  }


  #--- Loop over climate scenarios ------
  for (ksc in idsScens) {

    #--- Aggregated data files ------
    fname_data_vars <- file.path(dir_outs, paste0("data__variables.rds"))
    do_data_vars <- !file.exists(fname_data_vars)

    fname_data_ts_annual <- file.path(
      dir_outs, paste0("data-sc", ksc, "__ts_annual.rds")
    )
    do_ts_annual <- !file.exists(fname_data_ts_annual)

    fname_data_mean_daily <- file.path(
      dir_outs, paste0("data-sc", ksc, "__mean_daily.rds")
    )
    do_mean_daily <- !file.exists(fname_data_mean_daily)


    #--- Find shared output files ------
    if (do_data_vars || do_ts_annual || do_mean_daily) {
      ftmp <- list.files(
        dir_shared,
        pattern = paste0("_sc", ksc, ".", out_format),
        full.names = TRUE
      )

      res_label <- sapply(
        strsplit(
          sub(paste0(".", out_format), "", basename(ftmp)),
          split = "_",
          fixed = TRUE
        ),
        function(x) paste0(x[-(1:2)], collapse = "_")
      )

      stopifnot(length(res_label) == runsN_total)
    }


    #--- Simulated years for scenario ------
    it <- sim_scens[["df"]][ksc, "itime"]
    years <- seq.int(
      from = sim_scens[["itime"]][it, "simstartyr"],
      to = sim_scens[["itime"]][it, "endyr"]
    )


    #--- Detect variables (they are fixed across climate scenarios) ------

    if (!do_data_vars) {
      list_daily_vars <- readRDS(fname_data_vars)

    } else {
      pb <- utils::txtProgressBar(max = length(ftmp), style = 3)
      list_daily_vars <- NULL

      ssl <- c(0, simSoilLayers)

      patternSoilTemperature <- paste(
        paste0("_", formatC(ssl, width = 3L, flag = 0L), "_cm$"),
        collapse = "|"
      )

      patternDepthLayer <- paste(
        paste0(
          "_",
          formatC(ssl[-length(ssl)], width = 3L, flag = 0L),
          "to",
          formatC(ssl[-1L], width = 3L, flag = 0L),
          "_cm$"
        ),
        collapse = "|"
      )

      for (k in seq_along(ftmp)) {
        x <- switch(
          EXPR = out_format,
          csv = utils::read.csv(ftmp[k], nrows = 1, check.names = FALSE),
          rds = readRDS(ftmp[k])
        )

        tmp <- grep("Sim_|Input_", colnames(x), value = TRUE)

        # Remove incomplete soil layers
        if (removeIncompleteSoilLayers) {
          tmp1 <- grep("_cm$", tmp, invert = TRUE, value = TRUE)
          tmp2 <- grep("_SoilTemp_[[:print:]]+_cm", tmp, value = TRUE)
          tmp3 <- setdiff(tmp, c(tmp1, tmp2))

          tmp <- c(
            tmp1,
            grep(patternSoilTemperature, tmp2, value = TRUE),
            grep(patternDepthLayer, tmp3, value = TRUE)
          )
        }

        list_daily_vars <- unique(c(list_daily_vars, tmp))

        utils::setTxtProgressBar(pb, k)
      }

      close(pb)

      list_daily_vars <- sort(list_daily_vars)
      saveRDS(list_daily_vars, file = fname_data_vars)
    }



    #--- Prepare data containers ------
    if (!do_ts_annual) {
      res_ts_annual <- readRDS(fname_data_ts_annual)
    } else {
      res_ts_annual <- array(
        dim = c(runsN_total, length(years), length(list_daily_vars)),
        dimnames = list(res_label, years, list_daily_vars)
      )
    }

    if (!do_mean_daily) {
      res_mean_daily <- readRDS(fname_data_mean_daily)
    } else {
      res_mean_daily <- array(
        dim = c(runsN_total, 366, length(list_daily_vars)),
        dimnames = list(res_label, seq_len(366), list_daily_vars)
      )
    }


    #--- Read data ------
    if (do_ts_annual || do_mean_daily) {
      pb <- utils::txtProgressBar(max = length(ftmp), style = 3)

      for (k in seq_along(ftmp)) {
        x <- switch(
          EXPR = out_format,
          csv = read.csv(ftmp[k], check.names = FALSE),
          rds = readRDS(ftmp[k])
        )

        tmp_vars_used <- intersect(colnames(x), list_daily_vars)

        ids_yrs1 <- years %in% x[, "Year"]
        ids_yrs2 <- x[, "Year"] %in% years

        if (do_ts_annual) {
          res_ts_annual[k, ids_yrs1, tmp_vars_used] <- as.matrix(
            aggregate(
              x[ids_yrs2, tmp_vars_used, drop = FALSE],
              by = list(x[ids_yrs2, "Year"]),
              mean,
              na.rm = TRUE
            )
          )[, -1, drop = FALSE]
        }

        if (do_mean_daily) {
          tmp <- as.matrix(
            aggregate(
              x[ids_yrs2, tmp_vars_used],
              by = list(x[ids_yrs2, "DOY"]),
              mean,
              na.rm = TRUE
            )
          )[, -1, drop = FALSE]
          res_mean_daily[k, seq_len(nrow(tmp)), tmp_vars_used] <- tmp
        }

        utils::setTxtProgressBar(pb, k)
      }

      close(pb)

      if (do_ts_annual) {
        saveRDS(res_ts_annual, file = fname_data_ts_annual)
      }

      if (do_mean_daily) {
        saveRDS(res_mean_daily, file = fname_data_mean_daily)
      }
    }

    #--- Calculate values ------
    res_mean_annual <- apply(res_ts_annual, c(1, 3), mean, na.rm = TRUE)


    #--- Create plots ------
    #--- * Figures ------

    for (ktype in c("Values")) {

      #--- ** Density plot of mean annual values ------
      fname_mean_annual <- file.path(
        dir_fig,
        paste0("Fig_", experiments, "_Annual_Mean_", ktype, "_sc", ksc, ".pdf")
      )

      if (do_overwrite_figs || !file.exists(fname_mean_annual)) {
        tmp_data <- switch(
          EXPR = ktype,
          Values = res_mean_annual,
          `Deltas-PostPre` = res_mean_annual_deltas
        )

        res_mean_annual2 <- cbind(reshape2::melt(tmp_data), tmp = 0)

        colnames(res_mean_annual2) <- c("Site", "Variable", "Value", "tmp")

        res_mean_annual2[["Variable"]] <- factor(
          as.character(res_mean_annual2[["Variable"]]),
          levels = dimnames(tmp_data)[[2L]]
        )


        #--- Create PNGs and then convert to (bitmap) PDF
        # (vector) PDF are too large and slow
        n_panels <- c(5, 4)
        size_page <- c(2.5 * n_panels[1], 3 * n_panels[2])

        grDevices::png(
          filename = sub(".pdf", "%03d.png", fname_mean_annual),
          height = size_page[1],
          width = size_page[2],
          units = "in",
          res = 150
        )

        list_plots <- NULL
        n_pages <- Inf
        k <- 1

        while (k <= n_pages) {
          tmp <- ggplot2::ggplot(res_mean_annual2) +
            ggplot2::aes(x = Value, y = tmp) +
            #ggplot2::geom_density() +
            ggridges::geom_density_ridges(
              jittered_points = TRUE,
              point_size = 0.5,
              alpha = 0.5
            ) +
            colorspace::scale_color_discrete_sequential(palette = "Hawaii") +
            #ggplot2::scale_color_viridis_d() +
            #ggplot2::facet_wrap(ggplot2::vars(Variable), scales = "free") +
            ggforce::facet_wrap_paginate(
              ggplot2::vars(Variable),
              scales = "free",
              nrow = n_panels[1],
              ncol = n_panels[2],
              page = k
            ) +
            ggplot2::labs(
              title = paste0("Mean across years ", min(years), "-", max(years)),
              x = "",
              y = ""
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
              axis.text.y = ggplot2::element_blank()
            )

          print(tmp)

          if (is.infinite(n_pages)) {
            n_pages <- ggforce::n_pages(tmp)
          }

          k <- k + 1
        }

        grDevices::dev.off()


        to_bitmap_pdf(
          filename = fname_mean_annual,
          size_page = size_page,
          n_pages = n_pages
        )
      }



      #--- ** Time-series plot of annual values ------
      fname_ts_annual <- file.path(
        dir_fig,
        paste0("Fig_", experiments, "_Annual_TimeSeries_", ktype, "_sc", ksc, ".pdf")
      )

      if (do_overwrite_figs || !file.exists(fname_ts_annual)) {
        tmp_data <- switch(
          EXPR = ktype,
          Values = res_ts_annual,
          `Deltas-PostPre` = res_ts_annual_deltas
        )

        res_ts_annual2 <- reshape2::melt(tmp_data)
        colnames(res_ts_annual2) <- c("Site", "Year", "Variable", "Value")

        if (!is.null(expTrts)) {
          res_ts_annual2[, "ExpTrt"] <- factor(expTrts)

          ids <- order(res_ts_annual2[["ExpTrt"]], res_ts_annual2[["Site"]])
          res_ts_annual2[["Site"]] <- factor(
            as.character(res_ts_annual2[["Site"]]),
            levels = unique(res_ts_annual2[["Site"]][ids])
          )
        }


        #--- Create PNGs and then convert to (bitmap) PDF
        # (vector) PDF are too large and slow
        n_panels <- c(5, 4)
        size_page <- c(2.5 * n_panels[1], 3 * n_panels[2])

        grDevices::png(
          filename = sub(".pdf", "%03d.png", fname_ts_annual),
          height = size_page[1],
          width = size_page[2],
          units = "in",
          res = 150
        )

        list_plots <- NULL
        n_pages <- Inf
        k <- 1

        while (k <= n_pages) {
          tmp <- ggplot2::ggplot(res_ts_annual2) +
            ggplot2::aes(x = Year, y = Value) +
            ggplot2::geom_line(
              ggplot2::aes(color = Site),
              show.legend = FALSE
            ) +
            #colorspace::scale_color_discrete_sequential(palette = "Hawaii") +
            ggplot2::scale_color_viridis_d(guide = "none")

          if (!is.null(expTrts)) {
            tmp <- tmp +
              ggnewscale::new_scale_color() +
              ggplot2::scale_color_viridis_d() +
              ggplot2::geom_smooth(
                method = "glm",
                #formula = 'y ~ s(x, bs = "cs")',
                se = TRUE,
                level = 0.95,
                alpha = 0.1,
                ggplot2::aes(color = ExpTrt),
                show.legend = TRUE
              )
          } else {
            tmp <- tmp +
              ggplot2::geom_smooth(
                method = "glm",
                #formula = 'y ~ s(x, bs = "cs")',
                se = TRUE,
                level = 0.95,
                color = "red"
              )
          }

          tmp <- tmp +
            #ggplot2::facet_wrap(ggplot2::vars(Variable), scales = "free") +
            ggforce::facet_wrap_paginate(
              ggplot2::vars(Variable),
              scales = "free",
              nrow = n_panels[1],
              ncol = n_panels[2],
              page = k
            ) +
            ggplot2::labs(
              title = paste0("Years ", min(years), "-", max(years)),
              x = NULL,
              y = if (ktype == "Values") "variable" else "delta(variable)"
            ) +
            ggplot2::theme_bw()

          print(tmp)

          if (is.infinite(n_pages)) {
            n_pages <- ggforce::n_pages(tmp)
          }

          k <- k + 1
        }

        grDevices::dev.off()


        to_bitmap_pdf(
          filename = fname_ts_annual,
          size_page = size_page,
          n_pages = n_pages
        )
      }




      #--- ** Time-series plot of mean daily values ------
      fname_ts_dailyclim <- file.path(
        dir_fig,
        paste0("Fig_", experiments, "_DailyClim_TimeSeries_", ktype, "_sc", ksc, ".pdf")
      )

      if (do_overwrite_figs || !file.exists(fname_ts_dailyclim)) {
        tmp_data <- switch(
          EXPR = ktype,
          Values = res_mean_daily,
          `Deltas-PostPre` = res_mean_daily_deltas
        )

        res_mean_daily2 <- reshape2::melt(tmp_data)
        colnames(res_mean_daily2) <- c("Site", "DOY", "Variable", "Value")

        if (!is.null(expTrts)) {
          res_mean_daily2[, "ExpTrt"] <- factor(expTrts)

          ids <- order(res_mean_daily2[["ExpTrt"]], res_mean_daily2[["Site"]])
          res_mean_daily2[["Site"]] <- factor(
            as.character(res_mean_daily2[["Site"]]),
            levels = unique(res_mean_daily2[["Site"]][ids])
          )
        }


        #--- Create PNGs and then convert to (bitmap) PDF
        # (vector) PDF are too large and slow
        n_panels <- c(5, 4)
        size_page <- c(2.5 * n_panels[1], 3 * n_panels[2])

        grDevices::png(
          filename = sub(".pdf", "%03d.png", fname_ts_dailyclim),
          height = size_page[1],
          width = size_page[2],
          units = "in",
          res = 150
        )

        list_plots <- NULL
        n_pages <- Inf
        k <- 1

        while (k <= n_pages) {
          tmp <- ggplot2::ggplot(res_mean_daily2) +
            ggplot2::aes(x = DOY, y = Value) +
            ggplot2::geom_line(
              ggplot2::aes(color = Site),
              show.legend = FALSE
            ) +
            #colorspace::scale_color_discrete_sequential(palette = "Hawaii") +
            ggplot2::scale_color_viridis_d(guide = "none")


          if (!is.null(expTrts)) {
            tmp <- tmp +
              ggnewscale::new_scale_color() +
              ggplot2::scale_color_viridis_d() +
              ggplot2::geom_smooth(
                method = "gam",
                se = TRUE,
                level = 0.95,
                alpha = 0.1,
                ggplot2::aes(color = ExpTrt),
                show.legend = TRUE
              )
          } else {
            tmp <- tmp +
              ggplot2::geom_smooth(
                method = "gam",
                se = TRUE,
                level = 0.95,
                color = "red"
              )
          }

          tmp <- tmp +
            #ggplot2::facet_wrap(ggplot2::vars(Variable), scales = "free") +
            ggforce::facet_wrap_paginate(
              ggplot2::vars(Variable),
              scales = "free",
              nrow = n_panels[1],
              ncol = n_panels[2],
              page = k
            ) +
            ggplot2::labs(
              title = paste0("Mean across years ", min(years), "-", max(years)),
              x = NULL,
              y = if (ktype == "Values") "variable" else "delta(variable)"
            ) +
            ggplot2::theme_bw()

          print(tmp)

          if (is.infinite(n_pages)) {
            n_pages <- ggforce::n_pages(tmp)
          }

          k <- k + 1
        }

        grDevices::dev.off()


        to_bitmap_pdf(
          filename = fname_ts_dailyclim,
          size_page = size_page,
          n_pages = n_pages
        )
      }

    }
  }
}

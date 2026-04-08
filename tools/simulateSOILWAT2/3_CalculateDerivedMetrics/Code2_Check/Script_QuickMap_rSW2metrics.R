#!/usr/bin/env Rscript

#------ Settings for script ------

var_coords <- c("X_WGS84", "Y_WGS84")
var_coords <- c("Plot_X_WGS84", "Plot_Y_WGS84")

prj_tag <- "EDGE"

do_overwrite_figs <- FALSE

sizePanelWidthInch <- 7

varSpatialGrouping <- NULL


#----- Command Line Arguments ------
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


  do_maps <- TRUE
  include_metrics <- "EcologicalDroughtMetrics2023_annual"


  N_scen <- 1
  id_scen_used <- 1

  years_historical <- 2015:2023
  years_future_projection <- NULL
  years <- c(
    list(years_historical),
    lapply(
      id_scen_used[-1],
      function(k) years_future_projection
    )
  )

  yaggs <- c("mean", "trend")

  #------ Paths
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

  experiment <- args[[4L]]


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


  x_spatial_groups <- if (
    isTRUE(varSpatialGrouping %in% colnames(SWRunInformation))
  ) {
    SWRunInformation[, varSpatialGrouping, drop = TRUE]
  } else {
    rep("", nrow(SWRunInformation))
  }
  list_spatial_groups <- unique(x_spatial_groups)

  pts_units <- rSW2st::as_points(
    x = SWRunInformation[, var_coords],
    to_class = "sf",
    crs = 4326
  )


  #--- Load metrics
  tmp <- list.files(dir_metrics, full.names = TRUE)
  obj_output_quick <- tmp[grep("(\\<input_)|(_daily.rds\\>)|(\\<SMTRs)", basename(tmp), invert = TRUE)]
  if (length(include_metrics) > 0) {
    ids <- vapply(
      include_metrics,
      grep,
      x = basename(obj_output_quick),
      FUN.VALUE = NA_integer_
    )
    obj_output_quick <- obj_output_quick[ids]
  }
  obj_tags_quick <- sub(".rds", "", basename(obj_output_quick))



  fun_across_years <- function(x, agg) {
    apply(
      X = x,
      MARGIN = 2,
      FUN = if (agg == "cv") {
        function(x, na.rm = TRUE) {
          sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
        }
      } else if (agg == "trend") {
        # Sen Slope of Mann-Kendall Trend Test
        function(x, na.rm = TRUE) {
          if (sum(!is.na(x)) > 3) {
            modifiedmk::mkttest(x)["Sen's slope"]
          } else {
            NA
          }
        }
      } else {
        match.fun(agg)
      },
      na.rm = TRUE
    )
  }


  #---

  if (do_maps) {
    for (k1 in seq_along(obj_tags_quick)) {
      #--- Read and prepare data
      xdata <- readRDS(obj_output_quick[k1])


      for (ksc in seq_along(N_scen)) {
        tmp <- paste0("sc", ksc, "_", years[[ksc]])
        xvars <- tmp[tmp %in% colnames(xdata)]
        used_years <- as.integer(sapply(
          strsplit(xvars, split = "_", fixed = TRUE),
          `[`,
          j = 2
        ))

        for (ksg in seq_along(list_spatial_groups)) {
          ftmp_fig <- file.path(
            dir_fig,
            paste0(
              "Fig_", experiment, "_",
              if (nchar(list_spatial_groups[ksg]) > 0) {
                paste0(list_spatial_groups[ksg], "_")
              },
              obj_tags_quick[k1],
              "_sc", ksc,
              ".pdf"
            )
          )

          if (do_overwrite_figs || !file.exists(ftmp_fig)) {
            ids_plot_units <- which(
              x_spatial_groups %in% list_spatial_groups[ksg]
            )
            pts_plot_units <- pts_units[ids_plot_units, ]

            tmp <- sf::st_bbox(pts_plot_units)

            fig_lims <- list(
              xlim = tmp[c("xmin", "xmax")],
              ylim = tmp[c("ymin", "ymax")]
            )

            # See `ggplot2::coord_sf()$aspect` to figure out the aspect ratio:
            # sf::st_is_longlat(sf::st_crs(crs_map)) is FALSE => ratio = 1
            asp <- (tmp[["ymax"]] - tmp[["ymin"]]) / (tmp[["xmax"]] - tmp[["xmin"]])


            ids_pts_plot_units_included <- match(
              unique(xdata[, "site"]),
              SWRunInformation[ids_plot_units, "Label"],
              nomatch = 0
            )

            ids_plot_data <- which(
              xdata[, "site"] %in% SWRunInformation[ids_plot_units, "Label"]
            )
            x_plot_data <- xdata[ids_plot_data, c("site", "group", xvars)]

            # TODO
            N_sites <- length(unique(x_plot_data[, "site"]))
            used_yaggs <- yaggs
            # end TODO

            vaggs <- unique(x_plot_data[, "group"])
            n_panels <- c(length(vaggs), 1L + length(used_yaggs))

            #--- Calculate plots and store in list
            plot_list <- array(list(), dim = n_panels)

            for (k2 in seq_along(vaggs)) {
              tag_title <- paste(obj_tags_quick[k1], vaggs[k2], sep = " - ")

              xtmp <- t(x_plot_data[x_plot_data[, "group"] == vaggs[k2], xvars])

              tmp_sa <- utils::stack(as.data.frame(xtmp))
              tmp_sa$x <- used_years[1] + rep(seq_len(nrow(xtmp)), ncol(xtmp))

              # Time-series line plots: variable ~ year by site
              if (N_sites > 100) {
                # Plot band across sites + overall smoothed trend
                tmp_say <- aggregate(
                  x = tmp_sa[, "values"],
                  by = list(Year = tmp_sa[, "x"]),
                  FUN = function(x) {
                    quantile(
                      x = x[is.finite(x)],
                      probs = c(0, 0.025, 0.25, 0.75, 0.975, 1)
                    )
                  }
                )

                tmp_say <- cbind(tmp_say["Year"], tmp_say[["x"]])

                tmp <- ggplot2::ggplot(tmp_say) +
                  ggplot2::aes(Year) +
                  ggplot2::geom_ribbon(
                    ggplot2::aes(ymin = `0%`, ymax = `100%`),
                    alpha = 0.5,
                    fill = "darkseagreen3",
                    color = "transparent"
                  ) +
                  ggplot2::geom_ribbon(
                    ggplot2::aes(ymin = `2.5%`, ymax = `97.5%`),
                    alpha = 0.5,
                    fill = "darkblue",
                    color = "transparent"
                  ) +
                  ggplot2::geom_ribbon(
                    ggplot2::aes(ymin = `25%`, ymax = `75%`),
                    alpha = 0.5,
                    fill = "darkred",
                    color = "transparent"
                  )

              } else {
                # Plot each site as a line + overall smoothed trend
                # TODO: use  colors from mean across years
                tmp <- ggplot2::ggplot(tmp_sa) +
                  ggplot2::aes(x, values, color = ind) +
                  ggplot2::geom_line(show.legend = FALSE) +
                  ggplot2::scale_color_viridis_d(alpha = 0.5, option = "C")
              }

              tmp <- tmp +
                ggplot2::labs(
                  x = "Year",
                  y = obj_tags_quick[k1],
                  title = tag_title
                ) +
                egg::theme_article()

              # Add overall smoothed trend by a cubic-spline
              tmp_sa2 <- tmp_sa[is.finite(tmp_sa$values) & is.finite(tmp_sa$x), ]
              tmp_trend <- try(
                mgcv::gam(values ~ s(x, bs = "cs"), data = tmp_sa2),
                silent = TRUE
              )

              if (inherits(tmp_trend, "try-error")) {
                tmp_trend <- try(
                  stats::lm(
                    values ~ splines::bs(x, 3),
                    data = tmp_sa2
                  ),
                  silent = TRUE
                )
              }

              spv <- if (inherits(tmp_trend, "try-error")) {
                1
              } else {
                tmp_trends <- try(summary(tmp_trend), silent = TRUE)
                if (inherits(tmp_trends, "try-error")) 1 else tmp_trends[["s.pv"]]
              }

              tmp <- try(
                if (spv < 0.05) {
                  xt <- seq(min(tmp_sa[["x"]]), max(tmp_sa[["x"]]), length.out = 101)
                  py <- predict(tmp_trend, newdata = data.frame(x = xt))

                  tmp + ggplot2::geom_line(
                    ggplot2::aes(x, y),
                    data = data.frame(x = xt, y = py),
                    lwd = 2,
                    color = "black",
                    show.legend = FALSE
                  )

                } else {
                  tmp
                },
                silent = TRUE
              )

              plot_list[k2, 1][[1]] <- if (!inherits(tmp, "try-error")) {
                tmp
              } else {
                ggplot2::ggplot() + ggplot2::theme_void()
              }
              # end of time-series plot


              for (k3 in seq_along(used_yaggs)) {
                tag_title_k3 <- paste(used_yaggs[k3], sep = " - ")

                var_across_yrs <- fun_across_years(xtmp, used_yaggs[k3])
                ids <- !is.finite(var_across_yrs)
                var_across_yrs[ids] <- NA

                # Spatial point plot: mean/cv/trend across years by station
                xltmp <- cbind(
                  pts_plot_units[ids_pts_plot_units_included, ],
                  across_years = NA
                )
                xltmp[, "across_years"] <- var_across_yrs

                if (N_sites > 1000) {
                  # Rasterize
                  xltmp_r <- stars::st_rasterize(sf = xltmp)

                  tmp <- ggplot2::ggplot() +
                    stars::geom_stars(data = xltmp_r) +
                    ggplot2::coord_equal() +
                    #ggplot2::facet_wrap(~ band) +
                    ggplot2::theme_void() +
                    ggplot2::scale_x_discrete(expand = c(0, 0)) +
                    ggplot2::scale_y_discrete(expand = c(0, 0))

                } else {
                  # Spatial points
                  tmp <- ggplot2::ggplot(xltmp) +
                    ggplot2::geom_sf(
                      data = xltmp,
                      ggplot2::aes(fill = across_years), shape = 21, size = 2
                    )
                }

                if (exists("poly_Blitzen_LCC")) {
                  tmp <- tmp +
                    ggplot2::geom_sf(data = poly_Blitzen_LCC, fill = NA)
                }

                tmp <- tmp +
                  ggplot2::borders("state", fill = NA) +
                  ggplot2::coord_sf(
                    xlim = fig_lims[["xlim"]],
                    ylim = fig_lims[["ylim"]],
                    expand = TRUE
                  ) +
                  ggspatial::annotation_scale(location = "bl", width_hint = 0.4) +
                  ggplot2::labs(title = tag_title_k3) +
                  ggplot2::xlab("") +
                  ggplot2::ylab("") +
                  egg::theme_article() +
                  ggplot2::theme(
                    legend.key.height = ggplot2::unit(1, "cm")
                  )


                tmp <- try(
                  if (all(is.na(var_across_yrs))) {
                    tmp + ggplot2::scale_fill_grey()
                  } else {
                    if (used_yaggs[k3] == "trend") {
                      tmp + ggplot2::scale_fill_gradient2(
                        low = "orange",
                        high = "darkblue",
                        na.value = "white"
                      )
                    } else {
                      tmp + ggplot2::scale_fill_viridis_c(
                        na.value = "white"
                      )
                    }
                  },
                  silent = TRUE
                )

                plot_list[k2, 1 + k3][[1]] <- if (!inherits(tmp, "try-error")) {
                  tmp
                } else {
                  ggplot2::ggplot() + ggplot2::theme_void()
                }
              }
            }


            #--- Print figures
            tmp <- patchwork::wrap_plots(
              plots = t(plot_list),
              nrow = n_panels[[1L]],
              ncol = n_panels[[2L]]
            )

            grDevices::pdf(
              file = ftmp_fig,
              height = n_panels[[1L]] * sizePanelWidthInch * asp,
              width = n_panels[[2L]] * sizePanelWidthInch
            )

            plot(tmp)

            grDevices::dev.off()
          }
        }
      }
    }
  }
}

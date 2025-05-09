#------ . ------
#--- Ponderosa Pine basal area and SOILWAT2 biomass ------
#
# Developed 2022-Nov-18 for
# Rodman, Kyle C. et al. (in review) Ecological Applications
#
# Based on data from Andrews et al. (2021) Journal of Applied Ecology
#
#------ . ------

#------ References ------

# Kyle C. Rodman†,1, John B. Bradford2,3,, Alicia M. Formanack4, Peter Z. Fulé5,
# David W. Huffman1, Thomas E. Kolb5, Ana T. Miller-ter Kuile4.6, Donald P.
# Normandin1, Kiona Ogle4, Rory J. Pederson1, Daniel R. Schlaepfer3,7, Michael
# T. Stoddard1, Amy E.M. Waltz1. Restoration Treatments Enhance Tree Growth and
# Alter Climatic Constraints During Extreme Drought. Ecological Applications

# Andrews, C. M., D’Amato, A. W., Fraver, S., Palik, B., Battaglia, M. A., &
# Bradford, J. B. (2020). Low stand density moderates growth declines during hot
# droughts in semi‐arid forests. Journal of Applied Ecology, 57(6), 1089–1102.
# https://doi.org/10.1111/1365-2664.13615


#------ . ------
#------ Paths ------
dir_prj <- ".."

dir_dataraw <- file.path(dir_prj, "data-raw")
stopifnot(dir.exists(dir_dataraw))

dir_data <- file.path(dir_prj, "data")
dir.create(dir_data, recursive = TRUE, showWarnings = FALSE)

dir_results <- file.path(dir_prj, "results")
dir.create(dir_results, recursive = TRUE, showWarnings = FALSE)


#------ . ------

#------ Biomass inputs ------
# Data from Andrews et al. (2021) Journal of Applied Ecology

# FVEF = Taylor Woods study at the Fort Valley Experimental Forest, located in
# Northern Arizona, USA (35.275, −111.721) seven distinct density treatments
# (maintained at target densities on an approximate decadal basis (1962, 1967,
# 1972, 1982, 1992, 2002 and 2017)
#    * 5, 9, 14, 18, 23, 28 and 34 m2/ha basal area
#       * FVEF_30 = low = 9 m2/ha = 30 ft2/a
#       * FVEF_100 = medium = 23 m2/ha = 100 ft2/a
#       * FVEF_150 = high = 34 m2/ha = 150 ft2/a
#    * FVEF_Control = untreated control (51 m2/ha)
#    * clear-cut treatment (not used here)

# Interpret FVEF biomass values from Andrews et al. 2020 as on the ground


fname_biomass_data <- file.path(
  dir_data, "Andrews2020_biomassDataPIPO.rds"
)

if (file.exists(fname_biomass_data)) {
  fvef_trees <- readRDS(fname_biomass_data)

} else {

  andrews2022jae <- utils::read.csv(
    file = file.path(
      dir_dataraw,
      "SWRuns_InputData_v11_All_Sites_Veg_08_17.csv"
    )
  )

  fvef <- andrews2022jae[grep("FVEF", andrews2022jae[["Label"]]), , drop = FALSE]

  tmp <- lapply(
    c(
      "Composition_TreeFraction",
      "Litter", "Biomass", "FractionLive", "LAIconv"
    ),
    function(tag) {
      if (grepl("Composition", tag)) {
        res <- data.frame(
          Label = fvef[["Label"]],
          time = 0,
          Tree = fvef[[tag]]
        )
      } else {
        tmp_vars <- grep(paste0("Tree_", tag), colnames(fvef), value = TRUE)
        res <- reshape(
          fvef[, c("Label", tmp_vars)],
          direction = "long",
          idvar = "Label",
          varying = tmp_vars,
          sep = paste0("_", tag, "_m")
        )
      }
      data.frame(
        res,
        Variable = tag
      )
    }
  )

  fvef_trees <- do.call(rbind, tmp)


  fvef_trees[, "ba_m2PERha"] <- vapply(
    strsplit(fvef_trees[["Label"]], split = "_", fixed = TRUE),
    function(x) {
      switch(
        EXPR = x[[2]],
        Control = 51,
        `30` = 9,
        `100` = 23,
        `150` = 34
      )
    },
    FUN.VALUE = NA_real_
  )

  ids <- which(fvef_trees[["Variable"]] %in% "Composition_TreeFraction")
  fvef_trees[["Variable"]][ids] <- "Cover"

  saveRDS(fvef_trees, file = fname_biomass_data)
}


#--- * Figure: values from Andrews et al. 2020 -------
fname_fig <- file.path(dir_results, "TreeBiomass_Andrews2020JAE_FVEF.png")

if (!file.exists(fname_fig)) {
  tmp <- ggplot2::ggplot(data = fvef_trees) +
    ggplot2::aes(x = time, y = Tree, color = Label) +
    ggplot2::facet_wrap(
      ggplot2::vars(Variable),
      scales = "free"
    ) +
    ggplot2::geom_step() +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Month") +
    ggplot2::scale_x_continuous(
      breaks = seq_along(month.abb),
      labels = month.abb
    ) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_classic()

  grDevices::png(
    filename = fname_fig,
    width = 13,
    height = 5,
    units = "in",
    res = 200
  )
  suppressMessages(print(tmp))
  grDevices::dev.off()
}


#------ . ------
#------ Regression between basal area and monthly variables ------

#--- * Fit regressions biomass ~ ba ------
fname_biomass_regression <- file.path(
  dir_data, "Andrews2020_biomassRegressionPIPO.rds"
)

if (file.exists(fname_biomass_regression)) {
  fvef_trees_fun <- readRDS(fname_biomass_regression)

} else {
  fvef_trees_fun <- data.frame(
    unique(fvef_trees[, c("Variable", "time"), drop = FALSE]),
    m = NA,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  #vars_addzero <- c("Cover", "Litter", "Biomass", "LAIconv")
  vars_addzero <- c("Cover", "Biomass")


  for (k in seq_len(nrow(fvef_trees_fun))) {
    ids <-
      fvef_trees[["Variable"]] == fvef_trees_fun[k, "Variable", drop = TRUE] &
      fvef_trees[["time"]] == fvef_trees_fun[k, "time", drop = TRUE]

    data_withzero <- rbind(
      fvef_trees[ids, c("ba_m2PERha", "Tree"), drop = FALSE],
      if (fvef_trees_fun[k, "Variable", drop = TRUE] %in% vars_addzero) {
        data.frame(ba_m2PERha = 0, Tree = 0)
      }
    )

    if (FALSE) {
      m <- lm(
        Tree ~ ba_m2PERha,
        data = data_withzero
      )
    }

    fvef_trees_fun[["m"]][k] <- list(
      switch(
        EXPR = fvef_trees_fun[k, "Variable", drop = TRUE],
        # fractional response regression
        # (see https://stackoverflow.com/questions/37584715/fractional-response-regression-in-r)
        Cover = mgcv::gam(
          Tree ~ s(ba_m2PERha, k = 3L),
          data = data_withzero,
          family = stats::quasibinomial()
        ),
        # regular regression but should be bounded c(0, Inf)
        mgcv::gam(
          Tree ~ s(ba_m2PERha, k = 3L),
          data = data_withzero,
          family = stats::gaussian()
        )
      )
    )
  }


  saveRDS(fvef_trees_fun, file = fname_biomass_regression)
}



#--- * Figure: interpolated biomass -------
fname_fig <- file.path(dir_results, "TreeBiomass_Interpolated.png")

if (!file.exists(fname_fig)) {
  grDevices::png(
    filename = fname_fig,
    width = 3.5,
    height = 5,
    units = "in",
    res = 200
  )

  prev_par <- graphics::par(
    mfrow = c(3, 2),
    mar = c(2.5, 2, 0.5, 0.5),
    mgp = c(1, 0, 0),
    tcl = 0.2
  )


  xs <- seq(0, max(fvef_trees[["ba_m2PERha"]]))

  for (k in c(1, 1 + 1 + 12 * 0:3)) {
    ids <-
      fvef_trees[["Variable"]] == fvef_trees_fun[k, "Variable", drop = TRUE] &
      fvef_trees[["time"]] == fvef_trees_fun[k, "time", drop = TRUE]

    data_withzero <- rbind(
      fvef_trees[ids, c("ba_m2PERha", "Tree"), drop = FALSE],
      if (fvef_trees_fun[k, "Variable", drop = TRUE] %in% vars_addzero) {
        data.frame(ba_m2PERha = 0, Tree = 0)
      }
    )

    yfit <- stats::predict(
      fvef_trees_fun[["m"]][k][[1L]],
      newdata = data.frame(ba_m2PERha = xs),
      type = "response"
    )
    ypred <- stats::predict(
      fvef_trees_fun[["m"]][k][[1L]],
      newdata = data.frame(ba_m2PERha = xs),
      type = "response"
    )
    # Add linear fit as comparison
    mlin <- stats::lm(Tree ~ ba_m2PERha, data = data_withzero)
    ypredlin <- stats::predict(
      mlin,
      newdata = data.frame(ba_m2PERha = xs),
      type = "response"
    )

    plot(
      xs,
      yfit,
      type = "l",
      ylim = c(0, max(ypred, ypredlin, data_withzero[, "Tree"])),
      xlab = "BA [m2/ha]",
      ylab = fvef_trees_fun[k, "Variable", drop = TRUE]
    )

    graphics::points(
      data_withzero[, "ba_m2PERha"],
      data_withzero[, "Tree"],
      pch = 16L,
      col = "orange"
    )

    graphics::abline(mlin, lty = 2L, col = "gray")

    if (k == 1L) {
      graphics::legend(
        "bottomright",
        legend = c("GAM", "lm", "Andrews2020JAE-FVEF"),
        col = c("black", "gray", "orange"),
        pch = c(NA, NA, 16L, 4L, 4L),
        lty = c(1L, 2L, NA, NA, NA),
        merge = TRUE,
        cex = 0.5,
        pt.cex = 1
      )
    }
  }

  graphics::par(prev_par)
  grDevices::dev.off()
}



#------ . ------


#------ . ------

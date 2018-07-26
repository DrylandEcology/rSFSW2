# context("Soil data extraction")
# 
# # =============================================================================
# # The tests contained here are still in development and not all inclusive (yet); 
# # they do not check for individual value correctness for extraction functions, 
# # instead the goal here is to provide tests that are general enough that changing 
# # the files from which to extract data from won't cause the tests to fail. Ie. the 
# # tests are designed to test the underlining structures created from these functions.
# # =============================================================================
# 
# # set stage manually so that future changes won't cause the test to fail ======
# resume <- TRUE
# verbose <- FALSE
# MMC <- environment();
# sim_size <- environment();
# sim_space <- environment();
# fnames_in <- environment();
# SFSW2_glovars <- environment();
# SFSW2_glovars[["slyrs_ids"]] <- seq.int(1, 20, 1);
# SFSW2_glovars[["slyrs_maxN"]] <- 20;
# SWRunInformation <- data.frame(row.names <- c(1,2,3,4,5,6))
# SWRunInformation[,1] <- c(-106.2995, -106.2748, -106.2813, -106.2875, -106.2875, -106.2875)
# SWRunInformation[,2] <- c(35.7655, 35.76451, 35.77990, 35.79530, 35.79530, 35.79530)
# SWRunInformation[,3] <- rep("GriddedFROM100m", 6)
# SWRunInformation[,4] <- rep.int(1, 6)
# colnames(SWRunInformation) <- c("X_WGS84", "Y_WGS84", "SoilTexture_source", "Include_YN_SoilSources")
# sim_size$expN <- 2
# sim_size$runsN_master <- 6
# sim_size$runIDs_sites <- c(1,2,3,5,6)
# sim_size$runsN_sites <- 5
# sim_size$runsN_total <- 12
# sim_size$runIDs_total <- c(1  ,2,  3,  4,  5,  6,  7,  8,  9, 10 ,11, 12)
# sim_size$runsN_job <- 10
# sim_size$runsN_Pid <- 12
# sim_size$runsIDs_todo <- NULL
# sim_size$runsN_todo <- 0
# sim_size$digitsN_total <- 3
# sim_size$runIDs_sites_by_dbW <- c(1,2,3,4,5)
# 
# sim_space$scorp <- "point"
# sim_space$crs_sites <- sp::CRS("+init=epsg:4326")
# 
# sim_space$sim_res <- NA
# sim_space$run_sites <- sp::SpatialPoints(coords <- SWRunInformation[sim_size[["runIDs_sites"]],
#                                                  c("X_WGS84", "Y_WGS84")],
#                                                  proj4string <- sim_space[["crs_sites"]])
# 
# sim_space$sim_crs <- sp::CRS("+init=epsg:4326")
# temp_crs <- as.character(sim_space[["sim_crs"]])
# if (requireNamespace("rgdal", quietly = TRUE)) {
#   temp <- rgdal::checkCRSArgs(temp_crs)
#   stopifnot(temp[[1]])
#   sim_space[["sim_crs"]] <- sp::CRS(temp[[2]])
# }
# 
# fnames_in$fslayers <- "/home/natemccauslin/Desktop/TestPrj4/1_Input/SWRuns_InputData_SoilLayers_v9.csv"
# fnames_in$fprepocin <- "/home/natemccauslin/Desktop/TestPrj4/1_Input/SWRuns_InputAll_PreProcessed.rds"
# fnames_in$fsoils <- "/home/natemccauslin/Desktop/TestPrj4/1_Input/datafiles/SWRuns_InputData_soils_v12.csv"
# 
# field_sources <- "SoilTexture_source"
# include_sources <- "Include_YN_SoilSources"
# how_determine_sources <- "SWRunInformation"
# 
# # set up input_soils_use
# input_soils_use_colnames <- list("Matricd_L", "GravelContent_L", "EvapCoeff_L", "Grass_TranspCoeff_L", "Shrub_TranspCoeff_L", "Tree_TranspCoeff_L", "Forb_TranspCoeff_L",
#                                 "TranspRegion_L", "Sand_L", "Clay_L", "TOC_GperKG_L", "Imperm_L", "SoilTemp_L")
# newList <- vector("list", (length(input_soils_use_colnames) * 20) + 1)
# j <- 1
# counter <- 1;
# while (j <= length(newList)){
#   for (i in c(1:20)){
#     newList[j] <- paste0(input_soils_use_colnames[[counter]],i)
#     if(counter >= length(input_soils_use_colnames)) {
#       counter <- 1;
#     }
#     else{
#       counter <- counter + 1;
#     }
#     j <- j + 1;    
#   }
# }
# newList <- c("Label", newList)
# sw_input_soils_use <- as.data.frame(t(rep(FALSE, 281)))
# colnames(sw_input_soils_use) <- newList
# 
# # set up sw_input_soillayers
# sw_input_soillayers <- data.frame(row.names = c(1,2,3,4,5,6))
# soillayer_names <- vector("list", 22)
# soillayer_names[[1]] <- "Label"
# soillayer_names[[2]] <- "SoilDepth_cm"
# for(i in (3:22)){
#   soillayer_names[[i]] <- paste0("depth_L", (i - 2))
# }
# sw_input_soillayers[,1] <- c("Site01","Site02","Site03","Site04","Site05","Site06")
# for(i in (2:22)){
#   sw_input_soillayers[,i] <- NA
# }
# colnames(sw_input_soillayers) <- soillayer_names
# 
# # set up sw_input_soils
# input_soils <- list("Matricd_L", "GravelContent_L", "EvapCoeff_L", "Grass_TranspCoeff_L", "Shrub_TranspCoeff_L", "Tree_TranspCoeff_L", "Forb_TranspCoeff_L",
#                                 "TranspRegion_L", "Sand_L", "Clay_L", "TOC_GperKG_L", "Imperm_L", "SoilTemp_L")
# newList <- vector("list", (length(input_soils) * 20) + 1)
# j <- 1
# counter <- 1;
# while (j <= length(newList)){
#   for (i in c(1:20)){
#     newList[j] <- paste0(input_soils[[counter]], i)
#     if(counter >= length(input_soils)) {
#       counter <- 1;
#     }
#     else{
#       counter <- counter + 1;
#     }
#     j <- j + 1;    
#   }
# }
# newList <- c("Label", newList)
# sw_input_soils <- data.frame(row.names = c(1,2,3,4,5,6))
# sw_input_soils[,1] <- c("Site01","Site02","Site03","Site04","Site05","Site06")
# for(i in (2:281)){
#   sw_input_soils[,i] <- NA
# }
# colnames(sw_input_soils) <- newList
# 
# # prepare for extractions tests, set MMC
# MMC <- prepare_ExtractData_Soils(SWRunInformation, sim_size, field_sources,
#                                            include_sources, how_determine_sources, sw_input_soillayers, sw_input_soils_use,
#                                            sw_input_soils)
# 
# # run tests ===================================
# 
# test_that("Get Datasource Masterfield", {
#   # create copies of the above variables and change them here for expanding testing,
#   # tests rely on the variables above and changing them here will keep them reliable
#   master_sources <- rSFSW2::get_datasource_masterfield(SWRunInformation,
#                                                       field_sources, sim_size, "SWRunInformation")
#   expect_equal(master_sources, rep("GriddedFROM100m", 5))
# })
# test_that("Get Datasource Includefield", {
#   # create copies of the above variables and change them here for expanding testing,
#   # tests rely on the variables above and changing them here will keep them reliable
#   include_sources <- rSFSW2::get_datasource_includefield(SWRunInformation, include_sources,
#                                                         sim_size)
#   expect_equal(include_sources, rep(TRUE, 5))
# })
# test_that("Prepare Extract Data Soils", {
#   # check that the resulting general structure is correct
#   expect_is(MMC, "list")
#   expect_is(MMC[["vars"]], "data.frame")
#   expect_is(MMC[["input2"]], "data.frame")
#   expect_is(MMC[["input"]], "data.frame")
#   expect_is(MMC[["cn"]], "character")
#   expect_is(MMC[["source"]], "character")
#   expect_is(MMC[["data"]], "matrix")
#   expect_is(MMC[["idone"]], "logical")
#   expect_is(MMC[["use"]], "data.frame")
#   expect_is(MMC[["nvars"]], "integer")
# })
# i_Done <- rep(TRUE, 5)
# digits <- 2
# ldepths_cm <- c(5, 15, 30, 60, 100, 200)
# lys <- seq.int(length(ldepths_cm))
# 
# test_that("Update Soils Input", {
#   MMC <- update_soils_input(MMC, sim_size, digits, i_Done, ldepths_cm,
#                             lys, fnames_in)
#   # check that the resulting general structure is correct
#   expect_is(MMC, "list")
#   expect_is(MMC[["vars"]], "data.frame")
#   expect_is(MMC[["input2"]], "data.frame")
#   expect_is(MMC[["input"]], "data.frame")
#   expect_is(MMC[["cn"]], "character")
#   expect_is(MMC[["source"]], "character")
#   expect_is(MMC[["data"]], "matrix")
#   expect_is(MMC[["idone"]], "logical")
#   expect_is(MMC[["use"]], "data.frame")
#   expect_is(MMC[["nvars"]], "integer")
# })

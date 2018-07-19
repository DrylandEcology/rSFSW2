context("Soil data extraction")
resume = TRUE
verbose = FALSE
MMC = environment();
sim_size = environment();
sim_space = environment();
fnames_in = environment();
# set up manually so that a future change in site won't make test fail
SWRunInformation = data.frame(row.names = c(1,2,3,4,5,6))
SWRunInformation[,1] = c(-106.2995, -106.2748, -106.2813, -106.2875, -106.2875, -106.2875)
SWRunInformation[,2] = c(35.7655, 35.76451, 35.77990, 35.79530, 35.79530, 35.79530)
SWRunInformation[,3] = rep("GriddedFROM100m", 6)
colnames(SWRunInformation) = c("X_WGS84", "Y_WGS84", "SoilTexture_source")
sim_size$expN = 2
sim_size$runsN_master = 6
sim_size$runIDs_sites = c(1,2,3,5,6)
sim_size$runsN_sites = 5
sim_size$runsN_total = 12
sim_size$runIDs_total = c(1  ,2,  3,  4,  5,  6,  7,  8,  9, 10 ,11, 12)
sim_size$runsN_job = 10
sim_size$runsN_Pid = 12
sim_size$runsIDs_todo = NULL
sim_size$runsN_todo = 0
sim_size$digitsN_total = 3
sim_size$runIDs_sites_by_dbW = c(1,2,3,4,5)

sim_space$scorp = "point"
sim_space$crs_sites = sp::CRS("+init=epsg:4326")
sim_space$sim_res = NA
sim_space$run_sites = sp::SpatialPoints(coords = SWRunInformation[sim_size[["runIDs_sites"]],
                                                 c("X_WGS84", "Y_WGS84")],
                                                 proj4string = sim_space[["crs_sites"]])
fnames_in$fslayers = "/home/natemccauslin/Desktop/TestPrj4/1_Input/SWRuns_InputData_SoilLayers_v9.csv"
fnames_in$fprepocin = "/home/natemccauslin/Desktop/TestPrj4/1_Input/SWRuns_InputAll_PreProcessed.rds"
fnames_in$fsoils = "/home/natemccauslin/Desktop/TestPrj4/1_Input/datafiles/SWRuns_InputData_soils_v12.csv"

master_sources = rSFSW2::get_datasource_masterfield(SWRunInformation,
                                                    "SoilTexture_source", sim_size, "SWRunInformation")

test_that("Get Datasource Masterfield", {
  # create copies of the above variables and change them here for expanding testing,
  # tests rely on the variables above and changing them here will keep them reliable
  expect_equal(master_sources, rep("GriddedFROM100m", 5))
  
})
test_that("Get Datasource Includefield", {
  
})

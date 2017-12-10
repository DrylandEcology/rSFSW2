#!/usr/bin/env Rscript


#--- INPUTS
dSOILWAT2 <- "SoilWat2_defaults"
dir_prj <- file.path("data-raw", "1_Data_SWInput", dSOILWAT2)
dir_backup <- sub("SoilWat2_defaults", "SoilWat2_defaults_copy", dir_prj)

# 'example1' of rSOILWAT2 package is defined as 'default' from SOILWAT2
dir_orig <- system.file("extdata", "example1", package = "rSOILWAT2")


#-----------------------
#--- BACKUP PREVIOUS FILES
print(paste("Create backup of", shQuote(dir_prj), "as", shQuote(dir_backup)))
dir.create(dir_backup, showWarnings = FALSE)
stopifnot(dir.exists(dir_backup))
file.copy(from = dir_prj, to = dir_backup, recursive = TRUE, copy.mode = TRUE,
  copy.date = TRUE)

unlink(dir_prj, recursive = TRUE)
dir.create(dir_prj, showWarnings = FALSE)
stopifnot(dir.exists(dir_prj))


#-----------------------
#--- COPY AND CREATE SoilWat2_defaults FROM ORIGINAL SOILWAT2 INPUTS
file.copy(from = dir_orig, to = file.path(dir_prj, ".."), recursive = TRUE, copy.mode = TRUE,
  copy.date = TRUE)
file.rename(from = file.path(dir_prj, "..", basename(dir_orig)), to = dir_prj)


# Remove unncessary files
unlink(file.path(dir_prj, "files_step_soilwat_grid.in"))
unlink(file.path(dir_prj, "files_step_soilwat.in"))
unlink(file.path(dir_prj, "Input", "estab_v32_grid.in"))
unlink(file.path(dir_prj, "Input", "estab_v32.in"))
unlink(file.path(dir_prj, "Output"), recursive = TRUE)


# Delete all but one soil layer
ftemp <- file.path(dir_prj, "Input", "soils.in")
fin <- readLines(ftemp)
line <- intersect(c(grep("(depth)", fin), grep("(gravel_content)", fin)),
  c(grep("(sand)", fin), grep("(clay)", fin)))
stopifnot(length(line) == 1, line > 0, line < length(fin))
fin <- fin[1:(line + 1)]
writeLines(fin, con = ftemp)


# Turn soil temperature on
ftemp <- file.path(dir_prj, "Input", "siteparam.in")
fin <- readLines(ftemp)
line <- grep("flag, 1 to calculate soil_temperature", fin)
stopifnot(length(line) == 1, line > 0, line < length(fin))
substr(fin[line], 1, 1) <- "1"
writeLines(fin, con = ftemp)

# Delete weather data folder (all rSFSW2 projects get their own weather data)
unlink(file.path(dir_prj, "Input", "data_weather"), recursive = TRUE)
if (FALSE) {
# Move weather data folder to same level as 'Input/'
file.rename(from = file.path(dir_prj, "Input", "data_weather"),
  to = file.path(dir_prj, "Weather_Test"))

ftemp <- file.path(dir_prj, "files.in")
fin <- readLines(ftemp)
line <- grep("data file containing historical weather", fin)
stopifnot(length(line) == 1, line > 0, line < length(fin))
fin[line] <- sub("Input/data_weather/weath", "Input/Weather_Test/weath", fin[line])
writeLines(fin, con = ftemp)
}

#-----------------------
print(paste("NOTE: Remove", shQuote(dir_backup), "before pushing to repository if",
  "script worked well."))

# this is a quick program to chop the grib files used in cfsr_convert program from R

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# MAKE SURE THE FOLLOWING CONDITIONS ARE MET BEFORE RUNNING:
#   1) dynamically link cfsr_convert to do this call: "make linkr"
#  2) compile wGrib2 program beforehand & have it located in the same directory as cfsr_convert.  Instructions for how to compile wGrib2 are in cfsr_convert.c header.
#  3) set the variable num_threads located below
#  4) set the dir.gribfiles & dir.gribout directories accordingly
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

num_threads <- 2 # SET THIS VARIABLE BEFORE RUNNING TO THE NUMBER OF CORES TO USE

dir.gribfiles <- "/Users/lauenrothslabimac01/Desktop/Donovan/CFSR_weather_prog/griblargeC"
dir.gribout <- paste0(getwd(), "/", "chopped") #WARNING: program will remove this directory before starting...

t.overall <- Sys.time()

dyn.load("cfsr_convert.so")
library('doMC')

chopGribFile <- function(file.in, file.out, file.temp, file.temp2, theType) {

  f.in <- gsub("/", "//", file.in) # paths in C need two slashes instead of one...
  f.out <- gsub("/", "//", file.out)
  f.temp <- gsub("/", "//", file.temp)
  f.temp2 <- gsub("/", "//", file.temp2)

  chop <- .C("chopGribFile_R", file.in = as.character(f.in), file.out = as.character(f.out), file.temp = as.character(f.temp), file.temp2 = as.character(f.temp2), type = as.integer(theType))

  1
}

n_months <- 12
n_years <- 32
n_gribfiles <- n_years * n_months * 3 #1152 files in all

#setting up the input and output grib files for when we call foreach later...
file.in <- file.out <- file.temp <- file.temp2 <- rep("NA", n_gribfiles)
type <- rep(-999, n_gribfiles)
num <- 1
for (i in 1:3) {
  if (i == 1) {
    dir.pre.in <- paste0(dir.gribfiles, "/", "tmax/tmax.gdas.")
    dir.pre.out <- paste0(dir.gribout, "/", "tmax/tmax.gdas.")
  } else if (i == 2) {
    dir.pre.in <- paste0(dir.gribfiles, "/", "tmin/tmin.gdas.")
    dir.pre.out <- paste0(dir.gribout, "/", "tmin/tmin.gdas.")
  } else {
    dir.pre.in <- paste0(dir.gribfiles, "/", "ppt/prate.gdas.")
    dir.pre.out <- paste0(dir.gribout, "/", "ppt/prate.gdas.")
  }
  for (j in 1:n_years)
    for (k in 1:n_months) {
      if (k < 10)
        mo <- paste0("0", as.character(k))
      else
        mo <- as.character(k)
      yr <- as.character(j + 1978)
      file.in[num] <- paste0(dir.pre.in, yr, mo, ".grb2")
      file.out[num] <- paste0(dir.pre.out, yr, mo, ".grb2")
      file.temp[num] <- paste0(dir.pre.out, yr, mo, "_temp.grb2")
      file.temp2[num] <- paste0(dir.pre.out, yr, mo, "_temp2.grb2")
      type[num] = i - 1
      num <- num + 1
    }
}

# removing the output directory if it already exists...
if (file.exists(dir.gribout)) {
  print("Removing output directory")
  unlink(dir.gribout, recursive = TRUE)
}

# creating the output directories...
dir.create(dir.gribout, showWarnings = FALSE)
dir.create(paste0(dir.gribout, "/tmax"), showWarnings = FALSE)
dir.create(paste0(dir.gribout, "/tmin"), showWarnings = FALSE)
dir.create(paste0(dir.gribout, "/ppt"), showWarnings = FALSE)

# for parallelization
registerDoMC(num_threads)

print("")
print("STARTING CHOPS")
t.chops <- Sys.time()
nChopped <- foreach(i = 1:n_gribfiles, .combine = "+", .inorder = FALSE) %dopar% chopGribFile(file.in[i], file.out[i], file.temp[i], file.temp2[i], type[i]) #this line does the actual work...
print("")
print(paste("CHOPPED", nChopped, "FILES!!!"))
print(paste("Timing chops =", round(difftime(Sys.time(), t.chops, units = "secs"), 2), "secs"))
print(paste("Timing overall =", round(difftime(Sys.time(), t.overall, units = "secs"), 2), "secs"))
print("")

# this is a quick program to chop the grib files used in cfsr_convert program from R

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# MAKE SURE THE FOLLOWING CONDITIONS ARE MET BEFORE RUNNING: 
# 	1) dynamically link cfsr_convert to do this call: "make linkr"
#	2) compile wGrib2 program beforehand & have it located in the same directory as cfsr_convert.  Instructions for how to compile wGrib2 are in cfsr_convert.c header.
#	3) set the variable num_threads located below
#	4) set the dir.gribfiles & dir.gribout directories accordingly
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

num_threads <- 2 # SET THIS VARIABLE BEFORE RUNNING TO THE NUMBER OF CORES TO USE

dir.gribfiles <- "/Users/lauenrothslabimac01/Desktop/Donovan/CFSR_weather_prog/griblargeC"
dir.gribout <- paste(getwd(), "/", "chopped", sep="") #WARNING: program will remove this directory before starting...

t.overall <- Sys.time()

dyn.load("cfsr_convert.so")
library('doMC')

chopGribFile <- function(file.in, file.out, file.temp, file.temp2, theType) {

	f.in <- gsub("/", "//", file.in) # paths in C need two slashes instead of one...
	f.out <- gsub("/", "//", file.out)
	f.temp <- gsub("/", "//", file.temp)
	f.temp2 <- gsub("/", "//", file.temp2)

	chop <- .C("chopGribFile_R", file.in=as.character(f.in), file.out=as.character(f.out), file.temp=as.character(f.temp), file.temp2=as.character(f.temp2), type=as.integer(theType))
	
	return (1)
}

n_months <- 12
n_years <- 32 
n_gribfiles <- n_years * n_months * 3 #1152 files in all

#setting up the input and output grib files for when we call foreach later...
file.in <- file.out <- file.temp <- file.temp2 <- rep("NA", n_gribfiles)
type <- rep(-999, n_gribfiles)
num <- 1
for(i in 1:3) {
	if(i == 1) {
		dir.pre.in <- paste(dir.gribfiles, "/", "tmax/tmax.gdas.", sep="")
		dir.pre.out <- paste(dir.gribout, "/", "tmax/tmax.gdas.", sep="")
	} else if(i == 2) {
		dir.pre.in <- paste(dir.gribfiles, "/", "tmin/tmin.gdas.", sep="")
		dir.pre.out <- paste(dir.gribout, "/", "tmin/tmin.gdas.", sep="")
	} else {
		dir.pre.in <- paste(dir.gribfiles, "/", "ppt/prate.gdas.", sep="")
		dir.pre.out <- paste(dir.gribout, "/", "ppt/prate.gdas.", sep="")
	}
	for(j in 1:n_years)
		for(k in 1:n_months) {
			if(k < 10)
				mo <- paste("0", as.character(k), sep="")
			else
				mo <- as.character(k)
			yr <- as.character(j + 1978)
			file.in[num] <- paste(dir.pre.in, yr, mo, ".grb2", sep="")
			file.out[num] <- paste(dir.pre.out, yr, mo, ".grb2", sep="")
			file.temp[num] <- paste(dir.pre.out, yr, mo, "_temp.grb2", sep="")
			file.temp2[num] <- paste(dir.pre.out, yr, mo, "_temp2.grb2", sep="")
			type[num] = i - 1
			num <- num + 1
		}
}

# removing the output directory if it already exists...
if(file.exists(dir.gribout)) {
	print("Removing output directory")
	unlink(dir.gribout, recursive=TRUE)
}
	
# creating the output directories...
dir.create(dir.gribout, showWarnings = FALSE)
dir.create(paste(dir.gribout, "/tmax", sep=""), showWarnings = FALSE)
dir.create(paste(dir.gribout, "/tmin", sep=""), showWarnings = FALSE)
dir.create(paste(dir.gribout, "/ppt", sep=""), showWarnings = FALSE)

# for parallelization
registerDoMC(num_threads)

print("")
print("STARTING CHOPS")
t.chops <- Sys.time()
nChopped <- foreach(i = 1:n_gribfiles, .combine="+", .inorder=FALSE) %dopar% chopGribFile(file.in[i], file.out[i], file.temp[i], file.temp2[i], type[i]) #this line does the actual work...
print("")
print(paste("CHOPPED", nChopped, "FILES!!!"))
print(paste("Timing chops =", round(difftime(Sys.time(), t.chops, units="secs"), 2), "secs"))
print(paste("Timing overall =", round(difftime(Sys.time(), t.overall, units="secs"), 2), "secs"))
print("")
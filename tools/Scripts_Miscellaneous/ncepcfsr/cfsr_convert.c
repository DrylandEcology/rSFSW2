/**************************************************************************************************************************************
	cfsr_convert.c
	
	Author: 	Donovan Miller
	Purpose: 	Access CFSR data (at: "http://rda.ucar.edu/pub/cfsr.html"), and prepare daily weather and mean monthly climate files for SoilWat
	Date: 		07/12/2012
	Usage: 		./cfsr_convert yearLow yearHigh inputFile 
					-inputFile should be a .csv file containing latitude in the 8th column, longitude in the 7th column, and site name in the 11th column for each row, wherein the first row is column headings.
					-program doesn't always seem to read in .csv files made by Excel properly for some reason (this problem should be fixed now)
					-latitude and longitude values should be in decimal degrees
					
				./cfsr_convert -c
					-this option will chop up all of the daily grib files & get rid of the unnecessary values... this will take a while.

 	Requires:	wgrib2 program (at: "http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/").  The compiled version of wgrib2 must be in the same folder as this program.	
 				filefuncs2, generic2, & mymemory2.  For file i/o mainly.  The versions used by this program are slightly edited from the ones in soilwat, so don't get them confused.

 	WARNING:	process forking & running of wgrib2 will only work on a UNIX based computer.  For use on a windows computer, rewrite wgrib2() function to use spawnv().  Also, all of the multi-threading would have to be rewritten as it is also written using fork().  Also, all the calls to system will probably have to be rewritten.  Probably many more things would have to be rewritten as well, but that's all I can think of at the moment...
 	
 	REFER TO THE MAKEFILE FOR COMPILING INSTRUCTIONS
 	
 	NOTE: wgrib2 can be a pain to compile.  On mac it was pretty painless, I just typed make and it worked (the terminal will look like it's freaking out for a few minutes though while compiling).  The compiler I was using was gcc.
 			On JANUS it's slightly more complicated, as they don't have gcc.  First, I had to give "function.sh" file executable permissions using chmod.  Second, I had to add "CC=icc" line to the makefile.  Lastly, I had to remove the line that stops the makefile if the intel compiler (icc) is specified.
 			There is a note in the makefile that the Jasper library (used for jpeg2000 compression in wgrib2) will not work correctly when compiled with the intel compiler, but that shouldn't matter since the grib2 files we are extracting data from are not packed using the jpeg2000 format... wgrib2 is also able to change the way the data is packed if truly necessary, but this program is not set up to handle that.

	***NOTE: remember to set the maxSites definition below accordingly!!! only bother if you're running this from C, because if you're running it from R then the value isn't used.
**************************************************************************************************************************************/

/**************************************************************************************************************************************
	includes, defines, & variables
**************************************************************************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <sys/time.h>

#include "filefuncs2.h"

#define weathPrefix "weath"

#define yearBoundLow  1979
#define yearBoundHigh 2010

#define chrBuf 4096
#define maxSites 2 //make sure to change this number... I have it set to 2 at the moment, because last time I forgot to set it and crashed the computer b/c it can't handle creating & running 4300 threads at once

char errstr[chrBuf];

//boolean values...
int toDebug = 0; 			// 1 to print debug messages, 0 to not. (recommended: 0)
int multiThread = 1; 		// 1 to run in multiple threads (much faster), 0 to not. (recommended: 1, 0 if on JANUS b/c it's not working correctly for some unknown reason on JANUS.  It could be issues with pointers, a bug in the intel compiler, or maybe some other arbitrary reason)
int removeFiles = 1;		// 1 to remove unnecessary files afterwards, 0 to not. (recommended: 1)
int suppresswGrib2 = 1; 	// 1 to suppress the output of wGrib2 into the terminal, 0 to not. (recommended: 1, wGrib2 outputs a crap ton...)
int usewGrib2 = 1;			// 1 to use wGrib2, 0 to not. (recommended: 1)
int redirectStdOut = 1;		// 1 to redirect stderr & stdout to 'stdout.log' file, 0 to not.  (recommended: 1 if running on JANUS, since otherwise the output of stderr & stdout will get messed up because of the way JANUS handles the stream buffering)
int useChoppedFiles = 1;	// 1 to use chopped daily grib files, 0 to use regular unaltered daily grib files (recommended: 1 as it's maybe as much as 120 times faster).  Daily grib files must be chopped before running, this can be done by calling this program with only a "-c" argument ie. "./cfsr_convert -c"
							// the chopped up daily files for some reason have the times messed up (but the days are correct)... the values in them are correct though (I checked them a TON) so don't worry too much.

int calledFromCL = 0;   	// 1 if called from command line, 0 if not... the code sets it later if appropriate

#define gribDir "griblargeC2//" //the directory the gribfiles are located in... "" for the same directory the program is in.  Needs a // at the end...

//grib2 files are available at: "http://rda.ucar.edu/pub/cfsr.html" 
//NOTE: the monthly files do not have data for 2010 for some reason, so keep that in mind...
#define humidityGrib "pgbh06.gdas.R_H.2m.grb2" 		//name of the relative-humidity grib2 file (years 1979-2009) grid: 0.5° x 0.5°, 0E to 359.5E and 90N to 90S (720 x 361 Longitude/Latitude)
#define windGrib "flxf06.gdas.WND.10m.grb2" 		//name of the surface wind speed grib2 file (years 1979-2009) grid: 0.313° x ~0.312° from 0E to 359.687E and 89.761N to 89.761S (1152 x 576 Gaussian Longitude/Latitude)
#define cloudGrib "flxf06.gdas.T_CDC.EATM.grb2"		//name of the cloud cover grib2 file (years 1979-2009) grid: 0.313° x ~0.312° from 0E to 359.687E and 89.761N to 89.761S (1152 x 576 Gaussian Longitude/Latitude)

// the prefix is followed by the year and then the month (the month has a 0 in front of it if less then 10, ie 01 02 03 04... etc)
// make sure to leave the folder names the same...

#define tmaxGribPre "tmax//tmax.gdas." // (years 1979-2010) grid: 0.313° x ~0.312° from 0E to 359.687E and 89.761N to 89.761S (1152 x 576 Gaussian Longitude/Latitude)
#define tminGribPre "tmin//tmin.gdas."
#define pptGribPre "ppt//prate.gdas."

/**************************************************************************************************************************************
	functions
**************************************************************************************************************************************/

// remove file function
void removeAFile(char* filename) {
	if(file_exists(filename)) {
		if(toDebug)
			printf("Removing: %s\n", filename);
		remove(filename);
	} else if(toDebug)
		printf("Tried to remove: %s but it doesn't exist\n", filename);
}

void removeADir( const char *dname) { // doing this the cheap and easy way...
	char temp[512];
	sprintf(temp, "rm -R %s", strdup(dname));
	system(temp);
}

// flushes all the print statements...
void printFlush() {
	if(calledFromCL == 1) {
		fflush(stdout); // flush stdout & stderr... call this before forking
		fflush(stderr);
	}
}

// this function handles errors by printing a message to stdout, and then exiting if specified...
void error(int toExit, const char *message) { 
	printf("%s", message);
	if(toExit == 1) {
		printf(" exiting\n");
		exit(0);
	}
}

// gets the values at the specified column indices
// indices should be in order from smallest to largest... indices start at 0
// result is returned in values
// which is the separating character...
void getValues(char* line, int n, int indices[], char* values[], char* which) { 
	char *s = line, *context, *token;
	int i = 0, j;
	
	//strtok_r is used here to tokenize the string based upon the string which...
	while((token = strtok_r(s, which, &context))) {				//the _r version (of strtok) is used because it's multi-thread safe, while the regular version is not...
		if(i > indices[n - 1]) return;
		
		for( j=0; j<n; j++)
			if(i == indices[j]) 
				values[j] = strdup(s);	
		s = context;
		i++;
	}
}

// gets the month, when given a date... if the year isn't within the bounds (or the date format is wrong), then it returns -1
int getMonth(char* date, int yearLow, int yearHigh) {
	if(date == NULL) return 0;
	if(strlen(date) < 8) return 0;
	char *s = (char*) malloc(5); // allocate 5 b/c of the null terminator...
	
	strncpy(s, date + 1, 4);
	int year = atoi(s);
	
	if(yearLow <= year && yearHigh >= year) {
		char* s2 = (char*) malloc(3); // allocate 3 b/c of the null terminator...
		strncpy(s2, date + 6, 2);
		int month = atoi(s2);
		free(s2);
		if(month > 0 && month < 13) {
			free(s);
			return month - 1;
		}
	}
	free(s); //free s, because we don't need it anymore
	return -1;
}

// this function converts temperatures in Kelvin to temperatures in Celsius
double kelvinToCelsius(double kelvin) {
	return (kelvin - 273.15); // celsius = kelvin - 273.15, pretty simple equation...
}

// converting from (kg m^-2 s^-1) to cm/day... kg m^-2 s^-1 should be equal to mm/second
// for precipitation...
double kgToCM(double kg) {
	return ((60 * 60 * 24 * kg) / 10.0);
}

// this function takes in the u & v components of the wind (m s^-1) and returns the wind speed (m/s)
// make sure u & v values are positive before sending them to the function...
double getWindSpeed(double u, double v) {
	// refer to: "http://www.aprweather.com/pages/wind.htm"
	// think in terms of a triangle
	// side BA = abs(V), side CB = abs(U), & side CA = wind velocity (what we're after)
	// to get CA, we must first get the angle at C, which will be tan^-1(BA/CB)
	//double angleC, velocity;
	//angleC = atan(v / u);
	
	// we can then use the fact that sin(angle C) = BA/CA to find CA
	// so, the wind velocity = CA = BA/sin(angle C)
	//velocity = v / sin(angleC);
	
	return sqrt( (u * u) + (v * v)); //this will get the windspeed corectly I think too...
	//return velocity; // we can simply return the velocity since 1 (m s^-1) is equal to 1 m/s, so no conversion is necessary
}

// returns true if the given year is a leap year... false otherwise
int is_leap_year(int year) { // has to be divisible by 4, but not 100, or divisible by 400 to be true
	return ((year % 4 == 0 && year % 100 != 0) || year % 400 == 0);
}

// forks the process and then runs wgrib2 & waits until it finishes... this will only work correctly on a UNIX based computer.  Replace all the forking and use spawnv on a windows based computer.
void wgrib2(char *argv[]) { 
	if(usewGrib2 == 0) return;
	printFlush(); // empty the io buffers before forking...
	int mypid = fork();

    if( mypid < 0) {
    	error(1, "ERROR: process failed to fork and run wgrib2\n");
    } else if(0 == mypid) { // child process
    	if(suppresswGrib2 == 1) {
    		freopen("/dev/null", "w", stdout); //redirects stdout to "/dev/null".  /dev/null is a special file that discards all data written to it.  It's like a black hole.  This doesn't affect the output of the main program, since this is only done in the child process.  (which is terminated after the execv call)
    		// fclose(stdout); // closes stdout... this doesn't actually work to suppress wgrib2, b/c if you close the stdout, then wgrib2 simply outputs the messages it would in the terminal in the csv files it outputs, thereby making it not work... really freaking aggravating.
        }
        execv("wgrib2", argv);
		//program should never reach this line unless there is an error calling execv
		fprintf(stderr, "ERROR: running wgrib2 failed\n");
        exit(1);
    } else {  // parent process
        waitpid (0, NULL, 0); // waitpid() waits for the child process to finish...
    }
}

// runs wgrib2 and redirects the stdout to the file specified...
void wgrib22(char *argv[], char *fileName) { 
	if(usewGrib2 == 0) return;
	printFlush(); // empty the io buffers before forking...
	int mypid = fork();

    if( mypid < 0) {
    	error(1, "ERROR: process failed to fork and run wgrib2\n");
    } else if(0 == mypid) { // child process
    	// opening and closing the file, so that it actually exists when we redirect the stdout to it...
    	FILE* aFile = OpenFile(fileName, "w");
    	CloseFile(&aFile);
    	 // redirecting stdout to the file specified
    	freopen(fileName, "w", stdout);
    	// running wgrib2
        execv("wgrib2", argv);
		//program should never reach this line unless there is an error calling execv
		fprintf(stderr, "ERROR: running wgrib2 failed\n");
        exit(1);
    } else {  // parent process
        waitpid (0, NULL, 0); // waitpid() waits for the child process to finish...
    }
}

// this function is used to get the closest latitude & longitude to the ones specified...
// the string returned must still be parsed though...
// used in getCoords
char* getCoordsString(char* inputFileName, char* site, double longitude, double latitude) {
	int success = 0;
	char* grib_cargv[9], temp[chrBuf], temp2[chrBuf];
	
	grib_cargv[0] = "wgrib2";
	grib_cargv[1] = strdup(inputFileName);
	grib_cargv[2] = "-colon"; //
	grib_cargv[3] = ",";
	grib_cargv[4] = "-lon";
	sprintf(temp, "%5.6f", longitude);
	grib_cargv[5] = strdup(temp);
	sprintf(temp, "%5.6f", latitude);
	grib_cargv[6] = strdup(temp);
	grib_cargv[7] = "-end";
	grib_cargv[8] = NULL;
	sprintf(temp2, "%s.txt", site);
	wgrib22(grib_cargv, strdup(temp2));
	
	FILE* dailyCoordsFile = OpenFile(temp2, "r");
	if(GetALine(dailyCoordsFile, temp))
		success = 1;
	CloseFile(&dailyCoordsFile);
	removeAFile(temp2);
	
	if(success == 1)
		return strdup(temp);
	return NULL;
}

// gets the coordinates of the gaussian grid in the grb file inputFileName closest to the input coordinates
// returns 1 if successful, 0 if not... result is placed in outLongi and outLati
int getCoords(char* inputFileName, char* site, double inLongi, double inLati, float* outLongi, float* outLati) {
	char* coordsString = getCoordsString(inputFileName, site, inLongi, inLati);
	if(coordsString == NULL)
		return 0;
	
	int indices[] = {1, 2};	
	char* values[2];
	getValues(coordsString, 2, indices, values, "=");
		
	sscanf(values[0], "%f", outLongi);
	sscanf(values[1], "%f", outLati);
	if(toDebug) printf("coords %s actual %5.4f %5.4f grib %5.4f %5.4f\n", site, inLongi, inLati, *outLongi, *outLati);	
	return 1;
}

// calls the system... will supress the output of the call if suppresswGrib2 is 1...
void callSystem(char* call) {
	int pid = fork();
	if(pid == 0) { //child
		if(suppresswGrib2 == 1) freopen("/dev/null", "w", stdout);
		system(call);
		exit(0);
	} else //parent
		waitpid(0, NULL, 0);
}

// calls wgrib2 to chop a daily grib file... changes the packing of the output grib file to complex3 also
// type 0 is for tmax, type 1 is for tmin, and type 2 is for ppt
void chopGribFile(char *inFileName, char *outFileName, char *tempFileName, char *tempFileName2, int type) {
	if(type < 0 || type > 2) {
		printf("Invalid chop type specified, exiting\n");
		exit(0);
	}
	
	char systemCall[4096]; //might need to make this bigger if the directories are really long...
	
	// chopping out unneeded variables... gets rid of the 1, 2, 3, 4, & 5 hourly variables that are not useful for our purposes
	sprintf(systemCall, "./wgrib2 %s -if_n 6::6 -s -set_grib_type c3 -grib_out %s", inFileName, tempFileName);
	callSystem(systemCall);
	
	//epic system call... rewrites the grib files, changing the 6 hourly values into daily values... for tmax it gets the max, for tmin it gets the min, for ppt it gets the accumulation...
	if(type == 0)
		sprintf(systemCall, "./wgrib2 %s | grep \":d=$YYYYMMDD\" | ./wgrib2 -i %s -if '00:TMAX:' -rpn sto_1 -fi -if '06:TMAX:' -rpn sto_2 -fi -if '12:TMAX:' -rpn sto_3 -fi -if '18:TMAX:' -rpn sto_4 -fi -if_reg '1:2:3:4' -rpn 'rcl_1:rcl_2:rcl_3:rcl_4:max:max:max' -set_ave '4@6 hour max(0-6 hour max fcst),missing=0' -set_grib_type c3 -grib_out %s", tempFileName, tempFileName, tempFileName2);
	else if(type == 1)
		sprintf(systemCall, "./wgrib2 %s | grep \":d=$YYYYMMDD\" | ./wgrib2 -i %s -if '00:TMIN:' -rpn sto_1 -fi -if '06:TMIN:' -rpn sto_2 -fi -if '12:TMIN:' -rpn sto_3 -fi -if '18:TMIN:' -rpn sto_4 -fi -if_reg '1:2:3:4' -rpn 'rcl_1:rcl_2:rcl_3:rcl_4:min:min:min' -set_ave '4@6 hour max(0-6 hour max fcst),missing=0' -set_grib_type c3 -grib_out %s", tempFileName, tempFileName, tempFileName2);
	else if(type == 2)
		sprintf(systemCall, "./wgrib2 %s | grep \":d=$YYYYMMDD\" | ./wgrib2 -i %s -if '00:PRATE:' -rpn sto_1 -fi -if '06:PRATE:' -rpn sto_2 -fi -if '12:PRATE:' -rpn sto_3 -fi -if '18:PRATE:' -rpn sto_4 -fi -if_reg '1:2:3:4' -rpn 'rcl_1:rcl_2:rcl_3:rcl_4:+:+:+' -set_ave '4@6 hour max(0-6 hour max fcst),missing=0' -set_grib_type c3 -grib_out %s", tempFileName, tempFileName, tempFileName2);
	callSystem(systemCall);
	
	// chops up the grib files even further, getting rid of more unnecessary values... leaving only what is truly necessary (ie 1 value for each day).
	sprintf(systemCall, "./wgrib2 %s -if_n 1::4 -s -set_grib_type c3 -grib_out %s", tempFileName2, outFileName);
	callSystem(systemCall);
	
	removeAFile(tempFileName);
	removeAFile(tempFileName2);
}

// version to be called from R
void chopGribFile_R(char **inFileName, char **outFileName, char **tempFileName, char **tempFileName2, int *type) {
	chopGribFile(*inFileName, *outFileName, *tempFileName, *tempFileName2, *type);
}

/**************************************************************************************************************************************
	chopDailyGribFiles
		-chops up the daily grib files, getting rid of the unnecessary values.  Leaves the 6-hour values and gets rid of the 1, 2, 3, 4, & 5 hour values as they are not necessary for our purposes.
		-directory is the directory to put the new files in...
**************************************************************************************************************************************/
void chopDailyGribFiles(char *directory) {
	struct timeval mtime1, mtime2; //for timing
	gettimeofday(&mtime1, NULL); //for timing
	
	char temp[chrBuf], temp2[chrBuf], temp3[chrBuf], temp4[chrBuf], temp5[chrBuf];
	
	// making the directories if necessary
	if(!DirExists(directory)) MkDir2(directory);
	sprintf(temp, "%s//tmax", directory);
	if(!DirExists(temp)) MkDir2(temp);	
	sprintf(temp, "%s//tmin", directory);
	if(!DirExists(temp)) MkDir2(temp);
	sprintf(temp, "%s//ppt", directory);
	if(!DirExists(temp)) MkDir2(temp);
	
	int yrLow = yearBoundLow, yrHigh = yearBoundHigh;
	
	int i, j;
	for( i=yrLow; i <= yrHigh; i++)
		for( j=0; j < 12; j++) {
			if(j < 9)
				sprintf(temp2, "0%d", j + 1);
			else
				sprintf(temp2, "%d", j + 1);
			
			// tmax
			sprintf(temp, "%s%s%d%s.grb2", gribDir, tmaxGribPre, i, temp2); //the grib file to read in...
			sprintf(temp3, "%s//%s%d%s.grb2", directory, tmaxGribPre, i, temp2); //the output grib file...
			sprintf(temp4, "%s//%s%d%s_temp.grb2", directory, tmaxGribPre, i, temp2);
			sprintf(temp5, "%s//%s%d%s_temp2.grb2", directory, tmaxGribPre, i, temp2);
			chopGribFile(temp, temp3, temp4, temp5, 0);
			
			// tmin
			sprintf(temp, "%s%s%d%s.grb2", gribDir, tminGribPre, i, temp2); //the grib file to read in...
			sprintf(temp3, "%s//%s%d%s.grb2", directory, tminGribPre, i, temp2); //the output grib file...
			sprintf(temp4, "%s//%s%d%s_temp.grb2", directory, tminGribPre, i, temp2);
			sprintf(temp5, "%s//%s%d%s_temp2.grb2", directory, tminGribPre, i, temp2);
			chopGribFile(temp, temp3, temp4, temp5, 1);
			
			// ppt
			sprintf(temp, "%s%s%d%s.grb2", gribDir, pptGribPre, i, temp2); //the grib file to read in...
			sprintf(temp3, "%s//%s%d%s.grb2", directory, pptGribPre, i, temp2); //the output grib file...
			sprintf(temp4, "%s//%s%d%s_temp.grb2", directory, pptGribPre, i, temp2);
			sprintf(temp5, "%s//%s%d%s_temp2.grb2", directory, pptGribPre, i, temp2);
			chopGribFile(temp, temp3, temp4, temp5, 2);
			
			exit(0);
		}
		
	gettimeofday(&mtime2, NULL);
	int diffTime2 = mtime2.tv_sec - mtime1.tv_sec;
	if(toDebug) printf("chopping took: %d secs\n", diffTime2);
	
	exit(0);
}

// this function is simply to reduce some code duplication in the dailyWeather function...
void dailyGribLoop( int start, int end, int yrLow, double latitude, double longitude, float* lati, float* longi, char* site, char* directory, char* grib_argv[] ) {
	int i;
	char temp[chrBuf], temp2[chrBuf], temp3[chrBuf], tempMo[chrBuf];
	for(i = start; i < end; i++) {
		int yr = (i / 36) + yrLow;
		int mo = 1 + (i / (3 * (yr - yrLow + 1)));
		if(mo > 9)
			sprintf(tempMo, "%d", mo);
		else
			sprintf(tempMo, "0%d", mo);
		
		if(i % 3 == 0) {
			sprintf(temp, "%s%s%d%s.grb2", gribDir, tmaxGribPre, yr, tempMo);
			sprintf(temp2, "%s//tmax//%d//tmax_%d%s.csv", directory, yr, yr, tempMo);
		} else if(i % 2 == 0) {
			sprintf(temp, "%s%s%d%s.grb2", gribDir, tminGribPre, yr, tempMo);
			sprintf(temp2, "%s//tmin//%d//tmin_%d%s.csv", directory, yr, yr, tempMo);
			//printf("\n");
		} else {
			sprintf(temp, "%s%s%d%s.grb2", gribDir, pptGribPre, yr, tempMo);
			sprintf(temp2, "%s//ppt//%d//ppt_%d%s.csv", directory, yr, yr, tempMo);
		}
		
		// reading in the longitude and latitude values if they haven't been already, and setting them...
		if(*lati == -999 || *longi == -999) {
			getCoords(strdup(temp), strdup(site), longitude, latitude, longi, lati);
			sprintf(temp3, "%5.4f:%5.4f", *longi - 0.0001, *longi + 0.0001); 
			grib_argv[6] = strdup(temp3);
			sprintf(temp3, "%5.4f:%5.4f", *lati - 0.0001, *lati + 0.0001); 
			grib_argv[7] = strdup(temp3);
		}
		
		//printf("%s\n", temp);
		grib_argv[1] = strdup(temp);	//input file
		grib_argv[9] = strdup(temp2);	//output file
		wgrib2(grib_argv);
	}
}

/**************************************************************************************************************************************
	dailyWeather
		- gets the daily weather (tmax, tmin, prcp) for the specified years at the specified latitude & longitude, outputs the results into files for soilwat weather input format
**************************************************************************************************************************************/
void dailyWeather( double latitude, double longitude, int yearLow, int yearHigh, char *site, char *directory, int siteNum) {
	struct timeval mtime1, mtime2; //for timing
	gettimeofday(&mtime1, NULL); //for timing
	
	if(yearLow < yearBoundLow || yearLow > yearHigh || yearHigh > yearBoundHigh) {
		printf("ERROR: invalid years %d-%d\n", yearLow, yearHigh);
		exit(0);
	}
	
	float lati = -999, longi = -999;
	char *grib_argv[11], temp[chrBuf], temp2[chrBuf];
	
	// making directories...
	if(!DirExists(directory)) MkDir2(directory);
	sprintf(temp, "%s//ppt", directory);
	if(!DirExists(temp)) MkDir2(temp);
	sprintf(temp, "%s//tmax", directory);
	if(!DirExists(temp)) MkDir2(temp);
	sprintf(temp, "%s//tmin", directory);
	if(!DirExists(temp)) MkDir2(temp);
	
	int i, j, yrLow = yearLow, yrHigh = yearHigh;
	
	for( i=yrLow; i <= yrHigh; i++)
		for( j=0; j<3; j++) {
			if(j == 0) sprintf(temp, "%s//tmax//%d", directory, i);
			else if(j == 1) sprintf(temp, "%s//tmin//%d", directory, i);
			else sprintf(temp, "%s//ppt//%d", directory, i);
			
			MkDir2(temp);
		}
	
	grib_argv[0] = "wgrib2"; //first one has to be wgrib2
	sprintf(temp, "%s%s", gribDir, tmaxGribPre);
	grib_argv[1] = strdup(temp); //input file name
	grib_argv[2] = "-rpn";
	grib_argv[3] = """sto_1:-9999:rcl_1:merge:""";
	grib_argv[4] =  "-undefine";
	grib_argv[5] = "out-box";
	sprintf(temp, "%5.4f:%5.4f", longi - 0.0001, longi + 0.0001); 
	sprintf(temp2, "%5.4f:%5.4f", lati - 0.0001, lati + 0.0001); 
	grib_argv[6] = strdup(temp);
	grib_argv[7] = strdup(temp2);
	grib_argv[8] = "-csv";
	sprintf(temp, "%s//%s_tmax.csv", directory, site);
	grib_argv[9] = strdup(temp); //output file name
	grib_argv[10] = NULL; // the last one has to be NULL
	
	// to make it so that all the grib files aren't being read in at the same time...
	int nGribFiles = (((yrHigh - yrLow) + 1) * 12) * 3;
	int siteN = siteNum;
	while(siteN >= nGribFiles)
		siteN = siteN - nGribFiles;
	
	dailyGribLoop(siteN, nGribFiles, yrLow, latitude, longitude, &lati, &longi, strdup(site), strdup(directory), grib_argv);
	dailyGribLoop(0, siteN, yrLow, latitude, longitude, &lati, &longi, strdup(site), strdup(directory), grib_argv);
	
	// a bit of code duplication in this part... but whatever it shouldn't be that big of a deal plus I was getting too many errors when I tried to change it.
	int yr, doy, counter;
	double tMax[366], tMin[366], ppt[366];
	char inputBuf[chrBuf], outFileName[chrBuf];
	
	for(yr = yrLow; yr <= yrHigh; yr++) {
		
		sprintf(outFileName, "%s//%s.%d", directory, weathPrefix, yr);
		FILE* outFile = OpenFile(outFileName, "w");
		fprintf(outFile, "# weather for site %s year = %d\n", site, yr);
		fprintf(outFile, "# DOY Tmax(C) Tmin(C) PPT(cm)\n");
		
		// getting the tmax for every day...
		doy = 0;
		for( i=0; i < 12; i++) {
			if(i < 9)
				sprintf(temp2, "0%d", i + 1);
			else
				sprintf(temp2, "%d", i + 1);
			
			char inFileName[chrBuf];
			sprintf(inFileName, "%s//tmax//%d//tmax_%d%s.csv", directory, yr, yr, temp2);
			FILE* inFile = OpenFile(inFileName, "r");
			counter = 0;
			
			while(GetALine(inFile, inputBuf)) {
				if(useChoppedFiles == 0)
					for( j=0; j<5; j++)
						GetALine(inFile, inputBuf);
				counter++;	
				int indices[] = {0, 6};	
				char* values[2];
				getValues(inputBuf, 2, indices, values, ",");
				double value = atof(values[1]);
				
				if(counter == 1)
					tMax[doy] = value;
				else if(value > tMax[doy])
					tMax[doy] = value;
				
				if(counter >= 4 || useChoppedFiles == 1) {
					tMax[doy] = kelvinToCelsius(tMax[doy]);
					doy++;
					counter = 0;
				}
			}
			
			CloseFile(&inFile);
		}
		
		// getting the tmin for every day...
		doy = 0;
		for( i=0; i < 12; i++) {
			if(i < 9)
				sprintf(temp2, "0%d", i + 1);
			else
				sprintf(temp2, "%d", i + 1);
			
			char inFileName[chrBuf];
			sprintf(inFileName, "%s//tmin//%d//tmin_%d%s.csv", directory, yr, yr, temp2);
			FILE* inFile = OpenFile(inFileName, "r");
			counter = 0;
			
			while(GetALine(inFile, inputBuf)) {
				if(useChoppedFiles == 0)
					for( j=0; j<5; j++)
						GetALine(inFile, inputBuf);
				counter++;	
				int indices[] = {0, 6};	
				char* values[2];
				getValues(inputBuf, 2, indices, values, ",");
				double value = atof(values[1]);
				
				if(counter == 1)
					tMin[doy] = value;
				else if(value < tMin[doy])
					tMin[doy] = value;
				
				if(counter >= 4 || useChoppedFiles == 1) {
					tMin[doy] = kelvinToCelsius(tMin[doy]); // the temperatures from the grib files are in Kelvin, soilwat needs them in Celsius...
					doy++;
					counter = 0;
				}
			}
			
			CloseFile(&inFile);
		}
		
		// getting the ppt for every day...
		doy = 0;
		for( i=0; i < 12; i++) {
			if(i < 9)
				sprintf(temp2, "0%d", i + 1);
			else
				sprintf(temp2, "%d", i + 1);
			
			char inFileName[chrBuf];
			sprintf(inFileName, "%s//ppt//%d//ppt_%d%s.csv", directory, yr, yr, temp2);
			FILE* inFile = OpenFile(inFileName, "r");
			counter = 0;
			
			while(GetALine(inFile, inputBuf)) {
				if(useChoppedFiles == 0)
					for( j=0; j<5; j++)
						GetALine(inFile, inputBuf);
				counter++;	
				int indices[] = {0, 6};	
				char* values[2];
				getValues(inputBuf, 2, indices, values, ",");
				
				if(counter == 1)
					ppt[doy] = atof(values[1]);
				else
					ppt[doy] = atof(values[1]) + ppt[doy];
				
				if(counter >= 4 || useChoppedFiles == 1) {
					// the ppt grib file gives us the avg ppt rate for 6 hrs
					// be adding the avgs for 6 hrs and then dividing them by 4, we get the avg ppt rate for the day
					// after getting the avg ppt rate for the day, we then convert it cm/day because that's what soilwat needs
					ppt[doy] = kgToCM(ppt[doy] / 4.0);
					doy++;
					counter = 0;
				}
			}
			
			CloseFile(&inFile);
		}
		
		int nDays = 365;
		if(is_leap_year(yr)) 
			nDays++;
		for( i=0; i<nDays; i++) //outputting the values into the outfile...
			fprintf(outFile, "%d\t%5.2f\t%5.2f\t%5.2f\n", i+1, tMax[i], tMin[i], ppt[i]); // '\t' is the tab character
		
		CloseFile(&outFile);
	}
	
	if(removeFiles) {
		sprintf(temp, "%s//tmax", directory);
		removeADir(temp);
		sprintf(temp, "%s//tmin", directory);
		removeADir(temp);
		sprintf(temp, "%s//ppt", directory);
		removeADir(temp);
	}
	
	gettimeofday(&mtime2, NULL);
	int diffTime2 = mtime2.tv_sec - mtime1.tv_sec;
	if(toDebug) printf("daily - site: %s latitude: %5.4f longitude: %5.4f time: %d secs\n", site, latitude, longitude, diffTime2);
}

// this is the version of dailyWeather designed to be called from R
void dailyWeather_R(double *latitude, double *longitude, int *yearLow, int *yearHigh, char **site, char **directory, int* siteNum) {
	dailyWeather(*latitude, *longitude, *yearLow, *yearHigh, *site, *directory, *siteNum);
}

// at the moment this call is only be used in R, this function and the dailyWeather2Write are an alternate way of doing the same thing as the other daily weather function...
// nSites should be less then 900... 
// type 0 = tmax, 1 = tmin, 2 = ppt...
void dailyWeather2(int nSites, double latitudes[], double longitudes[], int year, int month, int type) {
	// ./wgrib2 in.grb2 -lon 0 0 -lon 2 2 -lon 3 3 | grep -o ",val=[\-]*[0-9.0-9]\{1,\}"
	
	char temp[chrBuf], tempMo[5], systemCall[32784]; //this is obscenely large and probably normally not recommended, but whatever...
	int i;
	
	// get ready for another epic system call...
	if(month < 10)
		sprintf(tempMo, "0%d", month);
	else
		sprintf(tempMo, "%d", month);
		
	if(type == 0) sprintf(temp, "%s%s%d%s.grb2", gribDir, tmaxGribPre, year, tempMo);
	else if(type == 1) sprintf(temp, "%s%s%d%s.grb2", gribDir, tminGribPre, year, tempMo);
	else if(type == 2) sprintf(temp, "%s%s%d%s.grb2", gribDir, pptGribPre, year, tempMo);
	
	sprintf(systemCall, "./wgrib2 %s", strdup(temp));
	for( i=0; i<nSites; i++)
		sprintf(systemCall, "%s -lon %5.4f %5.4f", systemCall, longitudes[i], latitudes[i]); // adding all of the longitude & latitude values to the call
	// this grep expression gets rid of everything in the inventory output by wgrib2, except the values themselves, altogether leaving one value per line making it really easy to parse later on...
	sprintf(systemCall, "%s | grep -o ',val=[\\-]*[0-9.0-9]\\{1,\\}[e]*[\\-]*[0-9]*'", systemCall); //ridiculous grep expression... because wgrib2 writes out some of the data values using scientific notation
	
	//we redirect the output from stdout to one of these files...
	if(type == 0) sprintf(systemCall, "%s > temporary_dy//tmax//tmax_%d%s.txt", systemCall, year, tempMo);
	else if(type == 1) sprintf(systemCall, "%s > temporary_dy//tmin//tmin_%d%s.txt", systemCall, year, tempMo);
	else if(type == 2) sprintf(systemCall, "%s > temporary_dy//ppt//ppt_%d%s.txt", systemCall, year, tempMo);
	
	callSystem(systemCall);
}

void dailyWeather2_R(int* nSites, double latitudes[], double longitudes[], int* year, int* month, int* type) {
	dailyWeather2(*nSites, latitudes, longitudes, *year, *month, *type);
}

// at the moment, this call is only used in R
// this function takes the files output by dailyWeather2 and writes them into the weath.year file for each site... must be called for every year.
// WARNING: this call opens & writes to lots of files, so there might be errors if your OS can't handle the amount of files it opens.
void dailyWeather2Write(int nSites, char* siteNames[], char* siteDirs[], int year) {
	char temp[chrBuf], tempMo[5];
	double tmax, tmin, ppt;
	FILE* siteFiles[nSites];
	int i, j=0, mo;
	
	for( i=0; i<nSites; i++) {
		sprintf(temp, "%s//%s.%d", siteDirs[i], weathPrefix, year); 
		siteFiles[i] = OpenFile(temp, "w");
		fprintf(siteFiles[i], "# weather for site %s year = %d\n", siteNames[i], year);
		fprintf(siteFiles[i], "# DOY Tmax(C) Tmin(C) PPT(cm)\n");
	}
	
	for( mo = 1; mo < 13; mo++) {
		if(mo < 10)
			sprintf(tempMo, "0%d", mo);
		else
			sprintf(tempMo, "%d", mo);
		
		sprintf(temp, "temporary_dy//tmax//tmax_%d%s.txt", year, tempMo);
		FILE* tmaxFile = OpenFile(temp, "r");
		sprintf(temp, "temporary_dy//tmin//tmin_%d%s.txt", year, tempMo);
		FILE* tminFile = OpenFile(temp, "r");
		sprintf(temp, "temporary_dy//ppt//ppt_%d%s.txt", year, tempMo);
		FILE* pptFile = OpenFile(temp, "r");
	
		i = 0;
		while(GetALine(tmaxFile, temp)) {
			tmax = tmin = ppt = 0.0;
			
			sscanf(temp, ",val=%lf", &tmax); 
			GetALine(tminFile, temp);
			sscanf(temp, ",val=%lf", &tmin);
			GetALine(pptFile, temp);
			sscanf(temp, ",val=%lf", &ppt); // %lf reads in scientific notation correctly, luckily...
			
			fprintf(siteFiles[i], "%d\t%5.2f\t%5.2f\t%5.2f\n", j+1, kelvinToCelsius(tmax), kelvinToCelsius(tmin), kgToCM( (ppt / 4.0) )); // '\t' is the tab character
			
			i++;
			if(i >= nSites) {
				i = 0;
				j++;
			}
		}
	
		CloseFile(&tmaxFile);
		CloseFile(&tminFile);
		CloseFile(&pptFile);
	}
	
	for( i=0; i<nSites; i++)
		CloseFile(&siteFiles[i]);
}

void dailyWeather2Write_R(int* nSites, char* siteNames[], char* siteDirs[], int* year) {
	dailyWeather2Write(*nSites, siteNames, siteDirs, *year);
}


/**************************************************************************************************************************************
	monthlyClimate
		- gets the mean monthly weather values (cloud cover, relative humidity, surface wind speed) for each month (36 values total)
		- returns the result as an array of 36 values in the order of monthly cloud cover, monthly relative humidity, monthly surface wind speed
**************************************************************************************************************************************/
void monthlyClimate( double latitude, double longitude, int yearLow, int yearHigh, char *site, char *directory, double result[] ) {
	struct timeval time1, time2; //for timing
	gettimeofday(&time1, NULL); //for timing
	
	if(yearLow < yearBoundLow || yearLow > yearHigh || yearHigh > yearBoundHigh)
		error(1, "ERROR: invalid years");
	int i, yearL = yearLow, yearH = yearHigh;
	char temp[chrBuf], temp2[chrBuf], *grib_argv[11];
	
	if(yearL == 2010 && yearH == 2010) { // because the monthly grib files don't have data for 2010...
		printf("WARNING: no monthly data for 2010, using data from 2009 instead\n");
		yearL = yearH = 2009;
	}
	
	if(!DirExists(directory))
		MkDir2(directory);
		
	float lati = -999, longi = -999, latiLowR = -999, longiLowR = -999;
	sprintf(temp, "%s03res.grb2", gribDir);
	getCoords(strdup(temp), strdup(site), longitude, latitude, &longi, &lati);
	
	sprintf(temp, "%s05res.grb2", gribDir);
	getCoords(strdup(temp), strdup(site), longitude, latitude, &longiLowR, &latiLowR);
	
	// calling wgrib2 to get the values needed...
	
	// ./wgrib2 gribfiles/pgbh06.gdas.R_H.EATM.grb2 -rpn "sto_1:-9999:rcl_1:merge:" -undefine out-box 0.5555:0.5555 1:1 -csv gribtest.csv
	grib_argv[0] = "wgrib2"; //first one has to be wgrib2
	sprintf(temp, "%s%s", gribDir, humidityGrib);
	grib_argv[1] = strdup(temp); //input file name
	grib_argv[2] = "-rpn";
	grib_argv[3] = """sto_1:-9999:rcl_1:merge:""";
	grib_argv[4] =  "-undefine";
	grib_argv[5] = "out-box";
	sprintf(temp, "%5.4f:%5.4f", longiLowR - 0.0001, longiLowR + 0.0001); //the relative humidity grib file has a lower resolution then the rest of the grib files...
	sprintf(temp2, "%5.4f:%5.4f", latiLowR - 0.0001, latiLowR + 0.0001); 
	grib_argv[6] = strdup(temp); //longitude
	grib_argv[7] = strdup(temp2); //latitude
	grib_argv[8] = "-csv";
	sprintf(temp, "%s//rh.csv", directory);
	grib_argv[9] = strdup(temp); //output file name
	grib_argv[10] = NULL; // the last one has to be NULL
	wgrib2(grib_argv); // relative humidity
	
	sprintf(temp, "%s%s", gribDir, cloudGrib);
	grib_argv[1] = strdup(temp);
	sprintf(temp, "%5.4f:%5.4f", longi - 0.0001, longi + 0.0001); //the cloud cover and wind speed files have a higher resolution than the relative humidity grib file...
	sprintf(temp2, "%5.4f:%5.4f", lati - 0.0001, lati + 0.0001);
	grib_argv[6] = strdup(temp); //longitude
	grib_argv[7] = strdup(temp2); //latitude
	sprintf(temp, "%s//cc.csv", directory);
	grib_argv[9] = strdup(temp);
	wgrib2(grib_argv); // cloud cover
	
	sprintf(temp, "%s%s", gribDir, windGrib);
	grib_argv[1] = strdup(temp);
	sprintf(temp, "%s//ws.csv", directory);
	grib_argv[9] = strdup(temp);
	wgrib2(grib_argv); // wind speed

	int counter[12], counter2[12], counter3[12];
	double rhMean[12], ccMean[12], wsMean[12];
	int indices[] = {0, 6};
	char* values[2];
	
	for( i=0; i < 12; i++)
		counter[i] = counter2[i] = counter3[i] = rhMean[i] = ccMean[i] = wsMean[i] = -1; //initialized to -1, because when you initialize arrays to 0, C does weird things with them that don't make any bloody sense if you're used to like any other programming language...
	
	char rhOutputN[chrBuf], ccOutputN[chrBuf], wsOutputN[chrBuf], inputBuf[chrBuf];
	sprintf(rhOutputN, "%s//rh.csv", directory);
	
	//getting the rhMean...
	FILE* rhOutput = OpenFile(rhOutputN, "r");
	while(GetALine(rhOutput, inputBuf)) {
		getValues(inputBuf, 2, indices, values, ",");
			
		int month = getMonth(values[0], yearL, yearH);	
		if(month > -1 && month < 12) {
			double d = atof(values[1]);
			rhMean[month] += d;
			counter[month] = counter[month] + 1;
			if(counter[month] == 0) { // to compensate for the fact that I initialized the values to -1 and not 0
				rhMean[month]++;
				counter[month]++;
			}
		}
	}
	CloseFile(&rhOutput);
	for( i=0; i<12; i++)
		if(counter[i] != -1)
			rhMean[i] = rhMean[i] / (counter[i] + 0.0);
		else
			rhMean[i] = -99.9;
			
	sprintf(ccOutputN, "%s//cc.csv", directory);
	//getting the ccMean...
	FILE* ccOutput = OpenFile(ccOutputN, "r");
	while(GetALine(ccOutput, inputBuf)) {
		getValues(inputBuf, 2, indices, values, ",");
			
		int month = getMonth(values[0], yearL, yearH);	
		if(month > -1 && month < 12) {
			double d = atof(values[1]);
			ccMean[month] += d;
			counter2[month] = counter2[month] + 1;
			if(counter2[month] == 0) { // to compensate for the fact that I initialized the values to -1 and not 0
				ccMean[month]++;
				counter2[month]++;
			}
		}
	}
	CloseFile(&ccOutput);
	for( i=0; i<12; i++)
		if(counter2[i] != -1)
			ccMean[i] = ccMean[i] / (counter2[i] + 0.0);
		else
			ccMean[i] = -99.9;
			
	sprintf(wsOutputN, "%s//ws.csv", directory);
	//getting the wsMean...
	FILE* wsOutput = OpenFile(wsOutputN, "r");
	while(GetALine(wsOutput, inputBuf)) {
		getValues(inputBuf, 2, indices, values, ",");
			
		int month = getMonth(values[0], yearL, yearH);	
		if(month > -1 && month < 12) {
			double u = fabs(atof(values[1]));
			GetALine(wsOutput, inputBuf);
			getValues(inputBuf, 2, indices, values, ",");
			double v = fabs(atof(values[1]));
			
			wsMean[month] += getWindSpeed(u, v);
			counter3[month] = counter3[month] + 1;
			if(counter3[month] == 0) { // to compensate for the fact that I initialized the values to -1 and not 0
				wsMean[month]++;
				counter3[month]++;
			}
		}
	}
	CloseFile(&wsOutput);
	for( i=0; i<12; i++)
		if(counter3[i] != -1) {
			wsMean[i] = wsMean[i] / (counter3[i] + 0.0);
		} else
			wsMean[i] = -99.9;
	
	for( i=0; i<12; i++) {
		result[i + 24] = wsMean[i];
		result[i + 12] = rhMean[i];
		result[i] = ccMean[i];
	}

	//remove the csv files output by wgrib2, since we don't need them anymore
	if(removeFiles == 1) {
		removeAFile(rhOutputN);
		removeAFile(ccOutputN);
		removeAFile(wsOutputN);
	}
	
	gettimeofday(&time2, NULL);
	int diffTimee = time2.tv_sec - time1.tv_sec;
	if(toDebug) printf("monthly - site: %s latitude: %5.4f longitude: %5.4f time: %d secs\n", site, latitude, longitude, diffTimee);
}

// version to be called from R
void monthlyClimate_R(double *latitude, double *longitude, int *yearLow, int *yearHigh, char **site, char **directory, double *result) {
	monthlyClimate(*latitude, *longitude, *yearLow, *yearHigh, *site, *directory, result);
}

/**************************************************************************************************************************************
	writeMonthlyClimate
		- writes the mean monthly weather values to a .csv file
**************************************************************************************************************************************/
void writeMonthlyClimate( double latitude, double longitude, int yearLow, int yearHigh, char *site, char *directory ) {
	double monthlyValues[36];
	monthlyClimate(latitude, longitude, yearLow, yearHigh, site, directory, monthlyValues);
	
	char outFileName[chrBuf];
	sprintf(outFileName, "%s//mc.csv", directory);
	FILE* outFile = OpenFile(outFileName, "w");
	
	//do the file writing...
	fprintf(outFile, "Month,Cloud_Cover,Rel_Humidity,Surface_Wind\n"); 
	int i;
	for( i=0; i < 12; i++)
		fprintf(outFile, "%d,%5.1f,%5.1f,%5.1f\n", (i+1), monthlyValues[i], monthlyValues[i + 12], monthlyValues[i + 24]);
	
	CloseFile(&outFile);
}

// version to be called from R
void writeMonthlyClimate_R(double *latitude, double *longitude, int *yearLow, int *yearHigh, char **site, char **directory) {
	writeMonthlyClimate(*latitude, *longitude, *yearLow, *yearHigh, *site, *directory);
}

// this function and writeMonthlyClimate2 do the same thing as writeMonthlyClimate, except in a different way...
// this is only used in R at the moment... it outputs a file containing the values for the years specified for the type specified into a file in the siteDir for each site
// type 0 = relative humidity, 1 = wind speed, 2 = cloud cover...
void monthlyClimate2(int nSites, double latitudes[], double longitudes[], char* siteDirs[], int yearLow, int yearHigh, int type) {
	char temp[chrBuf], inBuf[chrBuf], systemCall[32784]; //this is obscenely large and probably normally not recommended, but whatever...
	int i, j, yr, mo, yrHigh=yearHigh, yrLow=yearLow;
	double value;
	
	// get ready for another epic system call...
	if(type == 0) sprintf(temp, "%s%s", gribDir, humidityGrib);
	else if(type == 1) sprintf(temp, "%s%s", gribDir, windGrib);
	else if(type == 2) sprintf(temp, "%s%s", gribDir, cloudGrib);
	
	sprintf(systemCall, "./wgrib2 %s", temp);
	for( i=0; i<nSites; i++) {
		sprintf(systemCall, "%s -lon %5.4f %5.4f", systemCall, longitudes[i], latitudes[i]);
	}
	// :d=1979010100: a bunch of random crap ,lon=306.249574,lat=-68.846432,val=81.6:
	// :d=[0-9]\\{1,\\}:
	sprintf(systemCall, "%s | grep -o ',val=[\\-]*[0-9.0-9]\\{1,\\}[e]*[\\-]*[0-9]*'", systemCall);
	
	if(type == 0) sprintf(systemCall, "%s > temporary_dy//rh.txt", systemCall);
	else if(type == 1) sprintf(systemCall, "%s > temporary_dy//ws.txt", systemCall);
	else if(type == 2) sprintf(systemCall, "%s > temporary_dy//cc.txt", systemCall);
	
	callSystem(systemCall);
	
	FILE* siteFiles[nSites];
	for( i=0; i<nSites; i++) {
		if(type == 0) sprintf(temp, "%s//rh.txt", siteDirs[i]); 
		else if(type == 1) sprintf(temp, "%s//ws.txt", siteDirs[i]);
		else if(type == 2) sprintf(temp, "%s//cc.txt", siteDirs[i]);
		siteFiles[i] = OpenFile(temp, "w");
	}
	
	if(type == 0) sprintf(temp, "temporary_dy//rh.txt");
	else if(type == 1) sprintf(temp, "temporary_dy//ws.txt");
	else if(type == 2) sprintf(temp, "temporary_dy//cc.txt");
	FILE *inFile = OpenFile(temp, "r");
	
	if(yrHigh == 2010) { //this is to account for the fact that the monthly grib files do not contain data for 2010
		yrHigh = 2009;
		if(yearLow == 2010)
			yrLow = 2009;
	}
	i = j = 0;
	mo = 1;
	yr = yearBoundLow;
	while(GetALine(inFile, inBuf) && (yr <= yrHigh)) {
		sscanf(inBuf, ",val=%lf", &value);
		
		if(yr >= yrLow)
			fprintf(siteFiles[i], "%5.6f\n", value);
		
		i++;
		if(i >= nSites) {
			i = 0;
			if(type == 1) {
				j++;
				if(j > 1) {
					mo++;
					j = 0;
				}
			} else
				mo++;
			if(mo > 12) {
				mo = 1;
				yr++;
			}
		}
	}
	
	for( i=0; i<nSites; i++)
		CloseFile(&siteFiles[i]);	
	
	CloseFile(&inFile);
}

void monthlyClimate2_R(int* nSites, double latitudes[], double longitudes[], char* siteDirs[], int* yearLow, int* yearHigh, int* type) {
	monthlyClimate2(*nSites, latitudes, longitudes, siteDirs, *yearLow, *yearHigh, *type);
}

// this function is only called from R at the moment...
// used to take the raw values from rh.txt cc.txt & ws.txt and combine them into the monthly averages contained in mc.csv
// it simply needs the directory that the 3 files are located in...
void writeMonthlyClimate2(char* siteDir) {
	char temp[chrBuf], rhFileName[chrBuf], wsFileName[chrBuf], ccFileName[chrBuf];
	double rhMean[12], ccMean[12], wsMean[12], value;
	int mo, counter, i;
	
	for( i=0; i<12; i++) {
		rhMean[i] = -1;
		wsMean[i] = -1;
		ccMean[i] = -1;
		rhMean[i]++;
		wsMean[i]++;
		ccMean[i]++;
	}
	
	// opening the files we need...
	sprintf(rhFileName, "%s//rh.txt", siteDir);
	FILE* rhFile = OpenFile(strdup(rhFileName), "r");
	sprintf(wsFileName, "%s//ws.txt", siteDir);
	FILE* wsFile = OpenFile(strdup(wsFileName), "r");
	sprintf(ccFileName, "%s//cc.txt", siteDir);
	FILE* ccFile = OpenFile(strdup(ccFileName), "r");
	sprintf(temp, "%s//mc.csv", siteDir);
	FILE* outFile = OpenFile(temp, "w");
	
	mo = counter = 0;
	while(GetALine(rhFile, temp)) {
		value = atof(temp);
		rhMean[mo] += value;
		
		mo++;
		if(mo >= 12) {
			mo = 0;
			counter++;
		}
	}
	for( i=0; i<12; i++)
		if(counter != 0)
			rhMean[i] = rhMean[i] / (counter + 0.0);
		else
			rhMean[i] = -99.9;
		
	mo = counter = 0;
	while(GetALine(wsFile, temp)) {
		value = fabs(atof(temp));
		
		GetALine(wsFile, temp);
		double value2 = fabs(atof(temp));
		
		wsMean[mo] += getWindSpeed(value, value2);
		
		mo++;
		if(mo >= 12) {
			mo = 0;
			counter++;
		}
	}
	for( i=0; i<12; i++)
		if(counter != 0)
			wsMean[i] = wsMean[i] / (counter + 0.0);
		else
			wsMean[i] = -99.9;
		
	mo = counter = 0;
	while(GetALine(ccFile, temp)) {
		value = atof(temp);
		ccMean[mo] += value;
		
		mo++;
		if(mo >= 12) {
			mo = 0;
			counter++;
		}
	}
	for( i=0; i<12; i++)
		if(counter != 0)
			ccMean[i] = ccMean[i] / (counter + 0.0);
		else
			ccMean[i] = -99.9;
			
	//write the mc.csv output file...
	fprintf(outFile, "Month,Cloud_Cover,Rel_Humidity,Surface_Wind\n"); 
	for( i=0; i < 12; i++)
		fprintf(outFile, "%d,%5.1f,%5.1f,%5.1f\n", (i+1), ccMean[i], rhMean[i], wsMean[i]);
	
	// close the files we used...
	CloseFile(&rhFile);
	CloseFile(&wsFile);
	CloseFile(&ccFile);
	CloseFile(&outFile);
	
	// clean up unneeded files...
	if(removeFiles) {
		removeAFile(rhFileName);
		removeAFile(wsFileName);
		removeAFile(ccFileName);
	}
}

void writeMonthlyClimate2_R(char **siteDir) {
	writeMonthlyClimate2(*siteDir);
}

/**************************************************************************************************************************************
	main function:
		- inputFile should be a .csv file containing latitude in the 8th column, longitude in the 7th column, and site name in the 11th column for each row, wherein the first row is column headings.
		- outputs the daily & monthly values for each site
		- the first line of the input file should be the column headers
**************************************************************************************************************************************/
int main (int argc, char *argv[])
{
	if(argc == 1) {
		printf("REGULAR USAGE: './cfsr_convert yearLow yearHigh input.csv'\n");
		printf("CHOP USAGE: './cfsr_convert -c\n");
		printf("exiting\n");
		exit(0);
	}
	
	struct timeval startTime, endTime; //for timing
	gettimeofday(&startTime, NULL); //for timing
	calledFromCL = 1;
	
	char *chopStr = "-c";
	// chops up the daily grib files if requested...
	if(!strcmp(argv[1], chopStr)) {
		printf("Chopping up daily grib files\n");
		toDebug = 1;
		char directory[chrBuf];
		sprintf(directory, "chopped");
		chopDailyGribFiles(directory);
		printf("Finished chopping up daily grib files, exiting program\n");
		exit(0);
	}
	
	FILE* logFile = OpenFile("stdout.log", "w");
	if(redirectStdOut == 1) {
		printf("NOTE: stdout & stderr are being redirected to stdout.log\n");
		
		char logFileBuffer[1024];
		// opening the logfile and setting the buffering correctly
		setvbuf(logFile, logFileBuffer, _IOLBF, sizeof(char) * 1024); // sets the logFile to be line-buffered (ie: it flushes the buffer every time a new-line '\n' character is encountered).
		
		// these calls redirect stdout & stderr to stdout.log
		freopen("stdout.log", "w", stdout);
		freopen("stdout.log", "w", stderr);
	}

	if(argc < 4) {
		sprintf(errstr, "INPUT ERROR: please provide two years within %d-%d, and then a filename\n", yearBoundLow, yearBoundHigh);
		error(1, errstr);
	}
		
	if(argc >= 5)
		if(atoi(argv[4]) == 1) {
			printf("Debugging is on\n");
			toDebug = 1;
		}
	if(toDebug) printf("options: multiThread %d removeFiles %d suppresswGrib2 %d usewGrib2 %d\n", multiThread, removeFiles, suppresswGrib2, usewGrib2);
	
	int yearLower = atoi(argv[1]);
	int yearHigher = atoi(argv[2]);
	char* inputFileName = argv[3];
	
	if(yearLower < yearBoundLow || yearLower > yearHigher || yearHigher > yearBoundHigh)
		error(1, "INPUT ERROR: years provided are invalid\n");
	
	if(!FileExists(inputFileName)) {
		sprintf(errstr, "INPUT ERROR: input file %s does not exist\n", inputFileName);
		error(1, errstr);
	}
	
	// overwriting the inputfile to get rid of any of those damned carriage returns that were screwing my program up... this simply replaces the carriage returns with new line characters...
	// this way fgets() will still work when it gets called in the GetALine function later...
	FILE* dealWithCarriageReturns = OpenFile(inputFileName, "r");
	FILE* tempCarriageFile = OpenFile("tempCarriage.txt", "w");
	int aChar = fgetc(dealWithCarriageReturns);
	while(aChar != EOF) {
		if(aChar != (int) '\r')
			fputc(aChar, tempCarriageFile);
		else
			fputc((int) '\n', tempCarriageFile);
		aChar = fgetc(dealWithCarriageReturns);
	}
	
	CloseFile(&dealWithCarriageReturns);
	CloseFile(&tempCarriageFile);
	
	// reading in the input file...
	FILE* inputFile = OpenFile("tempCarriage.txt", "r");  // "r" reads a file, "w" creates an empty file for writing, "a" appends to a file... lookup fopen() function for the other modes.
	char inputBuf[chrBuf];
	char *siteName[maxSites];
	double lati[maxSites], longi[maxSites];
	int sites = 0;
	
	GetALine(inputFile, inputBuf); // ignore the first line of input since it's the column headings...
	if(toDebug) printf("\n");
	
	while(GetALine(inputFile, inputBuf)) {
		if(toDebug) printf("%s\n", inputBuf);
		
		char* values[3];
		int indices[3] = {6, 7, 10};
		getValues(inputBuf, 3, indices, values, ",");
		
		siteName[sites] = values[2];
		lati[sites] = atof(values[1]);
		longi[sites] = atof(values[0]);
		
		sites++;
		if(sites >= maxSites)
			break;
	}
	CloseFile(&inputFile); // closes our input file
	removeAFile("tempCarriage.txt"); 
	
	if(sites == 0)
		error(1, "ERROR reading in input file");
		
	//handles multithreading...
	if(toDebug) printf("\n");
	int pid = 0, i, siteN = 0;
	if(multiThread == 1 && sites > 1) { // if the # of sites is <= 1, then there is no point in multi-threading (since the program is set up to use one processor per site), so the program just runs it in serial...
		for(i=1; i < sites; i++)
			if(pid == 0) {
				siteN = i - 1;
				printFlush(); // empty all i/o buffers before forking...
				pid = fork(); // forks the process
			}
			if(siteN == sites - 2 && pid == 0) //handles the index for the last child process...
				siteN = sites - 1;
		
		dailyWeather(lati[siteN], longi[siteN], yearLower, yearHigher, siteName[siteN], strdup(siteName[siteN]), siteN);
		writeMonthlyClimate(lati[siteN], longi[siteN], yearLower, yearHigher, siteName[siteN], strdup(siteName[siteN]));
	} else 
		for( i=0; i < sites; i++) {
			dailyWeather(lati[i], longi[i], yearLower, yearHigher, siteName[i], strdup(siteName[i]), i);
			writeMonthlyClimate(lati[i], longi[i], yearLower, yearHigher, siteName[i], strdup(siteName[i]));
		}
	
	if(multiThread == 1 && sites > 1)
		while ((pid = waitpid (-1, NULL, 0)) > 0) {} //waits for all the threads to finish...
	if(siteN == 0) {
		if(toDebug) {
			printf("1st one - Site: %s Lati: %5.4f Longi: %5.4f\n", siteName[0], lati[0], longi[0]);
			printf("2nd one - Site: %s Lati: %5.4f Longi: %5.4f\n", siteName[1], lati[1], longi[1]);
		}
		printf("\n");
		printf("\nFinished %d sites!\n", sites);
		gettimeofday(&endTime, NULL);
		int diffTime = endTime.tv_sec - startTime.tv_sec;
		printf("Time total: %d secs\n", diffTime);
	}
	
	CloseFile(&logFile);
	
	return 0;
}

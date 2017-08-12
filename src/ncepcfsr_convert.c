/****************************************************************************************
  cfsr_convert.c

  Author:   Donovan Miller
  Purpose:  Access CFSR data (at: "http://rda.ucar.edu/pub/cfsr.html"), and prepare daily
            weather and mean monthly climate files for SoilWat
  Date:     07/12/2012
  Usage:    ./cfsr_convert yearLow yearHigh inputFile
            - inputFile should be a .csv file containing latitude in the 8th column,
              longitude in the 7th column, and site name in the 11th column for each row,
              wherein the first row is column headings.
            - program doesn't always seem to read in .csv files made by Excel properly
              for some reason (this problem should be fixed now)
            - latitude and longitude values should be in decimal degrees

            ./cfsr_convert -c
            - this option will chop up all of the daily grib files & get rid of the
              unnecessary values... this will take a while.

  Requires:
    - wgrib2 program (obtain from "http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/").
      The compiled version of wgrib2 must be in the same folder as this program.
    - filefuncs2, generic2, & mymemory2.  For file i/o mainly. The versions used by this
      program are slightly edited from the ones in soilwat, so don't get them confused.

  WARNING: process forking & running of wgrib2 will only work on a UNIX based computer.
    For use on a windows computer, rewrite wgrib2() function to use spawnv(). Also, all
    of the multi-threading would have to be rewritten as it is also written using fork().
    Also, all the calls to system will probably have to be rewritten.  Probably many more
    things would have to be rewritten as well, but that's all I can think of at the
    moment...

  REFER TO THE MAKEFILE FOR COMPILING INSTRUCTIONS

  NOTE: wgrib2 can be a pain to compile.  On mac it was pretty painless, I just typed
    make and it worked (the terminal will look like it's freaking out for a few minutes
    though while compiling).  The compiler I was using was gcc. On JANUS it's slightly
    more complicated, as they don't have gcc.  First, I had to give "function.sh" file
    executable permissions using chmod.  Second, I had to add "CC=icc" line to the
    makefile.  Lastly, I had to remove the line that stops the makefile if the intel
    compiler (icc) is specified. There is a note in the makefile that the Jasper library
    (used for jpeg2000 compression in wgrib2) will not work correctly when compiled with
    the intel compiler, but that shouldn't matter since the grib2 files we are
    extracting data from are not packed using the jpeg2000 format... wgrib2 is also
    able to change the way the data is packed if truly necessary, but this program is
    not set up to handle that.

  USAGE in R package 'rSFSW2': commented with '// Used in R package rSFSW2'
    - externalized functions: C_dailyWeather2_R, C_dailyWeather2Write_R, C_monthlyClimate2_R,
      C_writeMonthlyClimate2_R
*****************************************************************************************/

#define RSFSW2_NCEPCFSR 1

/****************************************************************************************
  includes, defines, & variables
*****************************************************************************************/

#include <assert.h>
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <float.h>
#include <math.h>
#include <memory.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

/***************************************************
 * Basic definitions
 ***************************************************/

// Used in R package rSFSW2
#define isnull(a) (NULL == (a))

#define chrBuf 4096
extern char inbuf[];

#ifndef RSFSW2_NCEPCFSR
  char errstr[chrBuf];
#endif


// define NCEP/CFSR data
#define weathPrefix "weath"
#define yearBoundLow  1979
#define yearBoundHigh 2010

//boolean values...
typedef enum {FALSE=(1!=1), TRUE=(1==1)} Bool;

int toDebug = 0; 			// 1 to print debug messages, 0 to not. (recommended: 0)
int multiThread = 1; 		// 1 to run in multiple threads (much faster), 0 to not. (recommended: 1, 0 if on JANUS b/c it's not working correctly for some unknown reason on JANUS.  It could be issues with pointers, a bug in the intel compiler, or maybe some other arbitrary reason)
int removeFiles = 1;		// 1 to remove unnecessary files afterwards, 0 to not. (recommended: 1)
int suppresswGrib2 = 1; 	// 1 to suppress the output of wGrib2 into the terminal, 0 to not. (recommended: 1, wGrib2 outputs a crap ton...)


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

// Used in R package rSFSW2
static void uncomment_cstyle(char *p) {
/*-------------------------------------------
  overwrite chars in a string pointed to by p with
  characters after the end-comment delimiter.
  p points to the first .
  cwb - 9/11/01
 -------------------------------------------*/

  char *e; /* end of comment */

  if ( (e = strchr(p+2,'*')) ) {
    if ( *(++e) == '/' ) {
      e++;
      while(*e) *(p++) = *(e++);
    }
  }
  *p = '\0';
}



/*****************************************************/
// Used in R package rSFSW2
char *Str_TrimRight(char *s) {
/*-------------------------------------------
 * Trim a string by setting first trailing space to '\0'
 * and return the string s.

  cwb - 6/15/00
  cwb - 9/11/01 moved this from Uncomment
 -------------------------------------------*/

  char *p = s + strlen(s);

  while( (--p)>=s && isspace((int)*p))
    *(++p) = '\0';

  return s;
}



/*****************************************************/
// Used in R package rSFSW2
void UnComment( char *s) {
/*-------------------------------------------
  Decomments a string by :
    a) putting a null char over the first '#'
    b) handling c-style comments.  If the block
       is in the middle of the non-blank part
       of the string, then the whitespace is
       unchanged.  THIS ROUTINE DOES NOT
       UNCOMMENT MULTIPLE LINES however it will
       delete from the first forward-slash - star delimiter to
       the end of the string.  There CANNOT
       be any characters between the / or *.


    White space at the end of the string
    part of the string is always removed.

   cwb - 9/11/01 added c-style comment code and
                 split out the different tasks.
 -------------------------------------------*/
  char *p;

  //if ( (p=strchr(s,'#')) )
   // *p = '\0';
  //else if ( (p=strstr(s,"/*")) )
  if( (p=strstr(s,"/*")) )
    uncomment_cstyle(p);

  Str_TrimRight(s);
}


/**************************************************************/
// Used in R package rSFSW2
Bool GetALine( FILE *f, char buf[]) {
/* Read a line of possibly commented input from the file *f.
 * Skip blank lines and comment lines.  Comments within the
 * line are removed and trailing whitespace is removed.
 */
 if(isnull(f)) {
  	printf("ERROR: tried to read from NULL file\n");
  	return FALSE;
  }

  char *p;
  Bool not_eof = FALSE;
   while( !isnull( fgets(buf, 1024, f) )) {
      if ( ! isnull( p=strchr(buf, (int) '\n')) )
        *p = '\0';

      UnComment(buf);
      if (*buf != '\0') {
         not_eof = TRUE;
         break;
      }
   }
   return not_eof;
}




/**************************************************************/
// Used in R package rSFSW2
FILE * OpenFile(const char *name, const char *mode) {
  FILE *fp;

  fp=fopen(name, mode);
  if ( isnull(fp) ) {
    printf("Cannot open file %s: %s\n",name,strerror(errno));
  }

  return fp;
}

/**************************************************************/
// Used in R package rSFSW2
void CloseFile( FILE **f) {
/* This routine is a wrapper for the basic fclose() so
   it might be possible to add more code like error checking
   or other special features in the future.

   Currently, the FILE pointer is set to NULL so it could be
   used as a check for whether the file is opened or not.
*/

  if(*f == NULL) {
  	printf("ERROR: tried to close NULL file\n");
  	return;
  }

  fclose(*f);

  *f = NULL;
}

Bool file_exists(const char * filename)
{
	FILE* file = fopen(filename, "r");
    if (!isnull(file))
    {
        fclose(file);
        return TRUE;
    }
    return FALSE;
}



#ifndef RSFSW2_NCEPCFSR

/**************************************************************/
Bool DirExists(const char *dname) {
  /* test for existance of a directory
   * dname is name of directory, not including filename
  */

  struct stat statbuf;
  Bool result=FALSE;

  if (0==stat(dname, &statbuf))
    result = (statbuf.st_mode & S_IFDIR) ? TRUE : FALSE;

  return result;
}



/**************************************************************/
/* Mapping mdir() function to OS specific version */
#ifdef _WIN32                          /* 32-bit Windows OS: Windows XP, Vista, 7, 8 */
  #define mkdir(d, m) mkdir(d)
#elif _WIN64                           /* 64-bit Windows OS: Windows XP, Vista, 7, 8 */
  #define mkdir(d, m) mkdir(d)
#elif __linux__                        /* linux: Centos, Debian, Fedora, OpenSUSE, RedHat, Ubuntu */
  #define mkdir(d, m) mkdir(d, m)
#elif __APPLE__ && __MACH__            /* (Mac) OS X, macOS, iOS, Darwin */
  #define mkdir(d, m) mkdir(d, m)
#endif

Bool MkDir( const char *dname) {
  /* make a path with 'mkdir -p' -like behavior. provides an
   * interface for portability problems.
   * RELATIVE PATH ONLY solves problems like "C:\etc" and null
   * first element in absolute path.
   * if you need to make an absolute path, use ChDir() first.
   * if you care about mode of new dir, use mkdir(), not MkDir()
   * if MkDir returns FALSE, check errno.
   *
   * Notes:
   * - portability issues seem to be quite problematic, at least
   *   between platforms and GCC and Borland.  The only common
   *   error code is EACCES, so if something else happens (and it
   *   well might in unix), more tests have to be included, perhaps
   *   with macros that test the compiler/platform.
   * - we're borrowing errstr to build the path to facilitate the
   *   -p behavior.
   */

  int r, i, n;
  Bool result = TRUE;
  char *a[256] = {0}, /* points to each path element for mkdir -p behavior */
       *delim = "\\/", /* path separators */
       *c;            /* duplicate of dname so we don't change it */

  if (isnull(dname) ) return FALSE;

  if (NULL == (c = strdup(dname)) ) {
    printf("Out of memory making string in MkDir()");
    return FALSE;
  }

  n=0;
  a[n++] = strtok(c, delim);
  while( NULL != (a[n++] = strtok(NULL, delim)) ) ; /* parse path */
  n--;
  errstr[0] = '\0';
  for(i=0; i<n; i++) {
    strcat(errstr, a[i]) ;
    if (!DirExists(errstr)) {
      if ( 0 != (r = mkdir(errstr, 0777)) ) {
        if (errno == EACCES ) {
          result = FALSE;
          break;
        }
      }
    }
    strcat(errstr, "/");
  }

  return result;
}

#undef mkdir


void MkDir2( const char *dname) { // doing this the cheap and easy way...
	char temp[512];
	sprintf(temp, "mkdir %s", strdup(dname));
	system(temp);
}

#endif


// Used in R package rSFSW2
// remove file function
void removeAFile(char* filename) {
	if(file_exists(filename)) {
		if(toDebug)
			printf("Removing: %s\n", filename);
		remove(filename);
	} else if(toDebug)
		printf("Tried to remove: %s but it doesn't exist\n", filename);
}



// Used in R package rSFSW2
// this function converts temperatures in Kelvin to temperatures in Celsius
double kelvinToCelsius(double kelvin) {
	return (kelvin - 273.15); // celsius = kelvin - 273.15, pretty simple equation...
}

// Used in R package rSFSW2
// converting from (kg m^-2 s^-1) to cm/day... kg m^-2 s^-1 should be equal to mm/second
// for precipitation...
double kgToCM(double kg) {
	return ((60 * 60 * 24 * kg) / 10.0);
}

// Used in R package rSFSW2
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
  double w = sqrt( (u * u) + (v * v));

	return w; //this will get the windspeed corectly I think too...
	//return velocity; // we can simply return the velocity since 1 (m s^-1) is equal to 1 m/s, so no conversion is necessary
}


// Used in R package rSFSW2
// calls the system... will supress the output of the call if suppresswGrib2 is 1...
void callSystem(char* call) {
  #if defined(_WIN32) || defined(_WIN64)
    printf("ERROR: multi-threading for function callSystem is not supported on Windows");
    return;

  #else
    int pid = fork();
    if(pid == 0) { //child
      if(suppresswGrib2 == 1) freopen("/dev/null", "w", stdout);
      system(call);
      return;

    } else //parent
        waitpid(0, NULL, 0);
  #endif
}


#ifndef RSFSW2_NCEPCFSR

// flushes all the print statements...
void printFlush() {
  fflush(stdout); // flush stdout & stderr... call this before forking
  fflush(stderr);
}

// forks the process and then runs wgrib2 & waits until it finishes... this will only work correctly on a UNIX based computer.  Replace all the forking and use spawnv on a windows based computer.
void wgrib2(char *argv[]) {

  printFlush(); // empty the io buffers before forking...

  #if defined(_WIN32) || defined(_WIN64)
    printf("ERROR: multi-threading for function wgrib2 is not supported on Windows");
    return;

  #else
    int mypid = fork();
    if(mypid < 0) {
      printf("ERROR: process failed to fork and run wgrib2\n");
      return;

    } else if(0 == mypid) { // child process
        if(suppresswGrib2 == 1) {
          freopen("/dev/null", "w", stdout); //redirects stdout to "/dev/null".  /dev/null is a special file that discards all data written to it.  It's like a black hole.  This doesn't affect the output of the main program, since this is only done in the child process.  (which is terminated after the execv call)
          // fclose(stdout); // closes stdout... this doesn't actually work to suppress wgrib2, b/c if you close the stdout, then wgrib2 simply outputs the messages it would in the terminal in the csv files it outputs, thereby making it not work... really freaking aggravating.
        }
        execv("wgrib2", argv);
        //program should never reach this line unless there is an error calling execv
        printf("ERROR: running wgrib2 failed\n");
        return;
    } else {  // parent process
        waitpid (0, NULL, 0); // waitpid() waits for the child process to finish...
    }
  #endif
}


// calls wgrib2 to chop a daily grib file... changes the packing of the output grib file to complex3 also
// type 0 is for tmax, type 1 is for tmin, and type 2 is for ppt
void chopGribFile(char *inFileName, char *outFileName, char *tempFileName, char *tempFileName2, int type) {
	if(type < 0 || type > 2) {
		printf("Invalid chop type specified, exiting\n");
		return;
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

			return;
		}

	gettimeofday(&mtime2, NULL);
	int diffTime2 = mtime2.tv_sec - mtime1.tv_sec;
	if(toDebug) printf("chopping took: %d secs\n", diffTime2);

	return;
}

#endif



// Used in R package rSFSW2
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

// Used in R package rSFSW2
void C_dailyWeather2_R(int* nSites, double latitudes[], double longitudes[], int* year, int* month, int* type) {
	dailyWeather2(*nSites, latitudes, longitudes, *year, *month, *type);
}

// Used in R package rSFSW2
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

			fprintf(siteFiles[i], "%d\t%5.2f\t%5.2f\t%5.2f\n", j+1, kelvinToCelsius(tmax),
			  kelvinToCelsius(tmin), kgToCM( (ppt / 4.0) )); // '\t' is the tab character

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

// Used in R package rSFSW2
void C_dailyWeather2Write_R(int* nSites, char* siteNames[], char* siteDirs[], int* year) {
	dailyWeather2Write(*nSites, siteNames, siteDirs, *year);
}



// Used in R package rSFSW2
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

// Used in R package rSFSW2
void C_monthlyClimate2_R(int* nSites, double latitudes[], double longitudes[], char* siteDirs[], int* yearLow, int* yearHigh, int* type) {
	monthlyClimate2(*nSites, latitudes, longitudes, siteDirs, *yearLow, *yearHigh, *type);
}

// Used in R package rSFSW2
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

// Used in R package rSFSW2
void C_writeMonthlyClimate2_R(char **siteDir) {
	writeMonthlyClimate2(*siteDir);
}

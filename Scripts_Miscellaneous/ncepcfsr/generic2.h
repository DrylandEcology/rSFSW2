/* generic.h -- contains some generic definitions */
/* that could be useful in any program            */
/*
 * USES: generic.c
 *
 * REQUIRES: none
 */
/* Chris Bennett @ LTER-CSU 6/15/2000            */
/*   - 5/19/2001  moved rand functions to rands.h*/
/* 10/22/2010	(drs)	replaced every occurence of F_DELTA, #define F_DELTA (10*FLT_EPSILON), with ( max( 10.*FLT_EPSILON, FLT_EPSILON*pow(10., ceil(log10(max(fabs(x),max(fabs(y), FLT_EPSILON))+1.)) ) ) ) and similar for D_DELTA
						because earlier version worked only for 0<=x<fabs(31) */
/* 01/03/2011	(drs) macro 'isless' renamed to 'isless2' */
/* 05/25/2012	(DLM) added regression() function															*/
/* 05/29/2012   (DLM) added lobf(), lobfM(), & lobfCB() functions */
/* 05/29/2012   (DLM) added squared(x) definition, this squares the value x (ie. returns the value of x^2).  the definition simply calls pow(x, 2) in the cmath library.  x must be a double. added for convenience */
// 06/18/2012   (DLM) this is a stripped down version of generic for use in cfsr_convert.c


#ifndef GENERIC2_H
#define GENERIC2_H

#include <stdio.h>
#include <float.h>
#include <math.h>
#include <assert.h>



/***************************************************
 * Basic definitions
 ***************************************************/

/* ------ Convenience macros. ------ */
/* integer to boolean */
#define itob(i) ((i)?TRUE:FALSE)
/* integer versions */
#define max(a,b) (((a) > (b)) ? (a) : (b))
#define min(a,b) (((a) < (b)) ? (a) : (b))
/* floating point versions work for float or double */
#define fmax(a,b) ( ( GT((a),(b)) ) ? (a) : (b))
#define fmin(a,b) ( ( LT((a),(b)) ) ? (a) : (b))

/* redefine sqrt for double (default) or float */
#ifdef NO_SQRTF
 /* the case for Borland's compiler */
  #define sqrtf sqrt
#endif

#define sqrt(x) ((sizeof(x)==sizeof(float)) ? sqrtf(x) : sqrt(x))

#define isnull(a) (NULL == (a))

/* -------  Some Time macros that should be always on ------ */
#define YearTo4Digit(y) ((TimeInt)(( (y) > 100)  \
                      ? (y)                      \
                      : ((y)<50) ? 2000+(y)      \
                                    : 1900+(y) ) )
#define WEEKDAYS 7
#define Doy2Week(d) ((TimeInt)(((d)-1) /WEEKDAYS))


/* ---------   Redefine basic types to be more malleable ---- */
typedef float     RealF;
typedef double    RealD;
typedef int       Int;
typedef unsigned int IntU;
typedef short int IntS;
typedef unsigned short IntUS;
typedef long      IntL;
typedef enum {FALSE=(1!=1), TRUE=(1==1)} Bool;
typedef unsigned char byte;

/* an attempt to facilitate integer implementation of real */
/*
typedef long IRealF
typedef double long IRealD
#define IF_GRAIN 10000000L
#define F2I(x) ((IRealF)(x*IF_GRAIN))
#define D2I(x) ((IRealD)(x*ID_GRAIN))
#define I2F(x) ((( RealF)x/IF_GRAIN))
#define I2D(x) ((( RealD)x/ID_GRAIN))
*/

/* --------------------------------------------------*/
/* These are facilities for logging errors.          */

/* constants for LogError() mode */
#define LOGNOTE  0x01
#define LOGWARN  0x02
#define LOGERROR 0x04
#define LOGEXIT  0x08
#define LOGFATAL 0x0c  /* LOGEXIT | LOGERROR */
#define MAX_ERROR 4096

extern char errstr[];

/* --------------------------------------------------*/
/* --------------------------------------------------*/
/* The following tests account for imprecision in the
  floating point representation of either single
  or double real numbers.  Use these instead of
  the regular boolean operators.  Note that only
  the magnitude of the x operand is tested, so both
  operands (x & y) should be of the same type, or cast
  so.

#define F_DELTA (10*FLT_EPSILON)
#define D_DELTA (10*DBL_EPSILON)
*/

#define iszero(x) \
( (sizeof(x) == sizeof(float)) \
  ? ((x)>-( max( 10.*FLT_EPSILON, FLT_EPSILON*pow(10., ceil(log10(max(fabs(x),FLT_EPSILON)+1.)) ) ) ) && (x)<( max( 10.*FLT_EPSILON, FLT_EPSILON*pow(10., ceil(log10(max(fabs(x),FLT_EPSILON)+1.)) ) ) )) \
  : ((x)>-( max( 10.*DBL_EPSILON, DBL_EPSILON*pow(10., ceil(log10(max(fabs(x),DBL_EPSILON)+1.)) ) ) ) && (x)<( max( 10.*DBL_EPSILON, DBL_EPSILON*pow(10., ceil(log10(max(fabs(x),DBL_EPSILON)+1.)) ) ) )) )

#define isequal(x,y) \
( (sizeof(x) == sizeof(float)) \
  ? ((x)>(y)-( max( 10.*FLT_EPSILON, FLT_EPSILON*pow(10., ceil(log10(max(fabs(x),max(fabs(y), FLT_EPSILON))+1.)) ) ) ) && (x)<(y)+( max( 10.*FLT_EPSILON, FLT_EPSILON*pow(10., ceil(log10(max(fabs(x),max(fabs(y), FLT_EPSILON))+1.)) ) ) )) \
  : ((x)>(y)-( max( 10.*DBL_EPSILON, DBL_EPSILON*pow(10., ceil(log10(max(fabs(x),max(fabs(y), DBL_EPSILON))+1.)) ) ) ) && (x)<(y)+( max( 10.*DBL_EPSILON, DBL_EPSILON*pow(10., ceil(log10(max(fabs(x),max(fabs(y), DBL_EPSILON))+1.)) ) ) )) )


#define isless2(x,y) \
( (sizeof(x) == sizeof(float)) \
  ? ((x)<(y)-( max( 10.*FLT_EPSILON, FLT_EPSILON*pow(10., ceil(log10(max(fabs(x),max(fabs(y), FLT_EPSILON))+1.)) ) ) )) \
  : ((x)<(y)-( max( 10.*DBL_EPSILON, DBL_EPSILON*pow(10., ceil(log10(max(fabs(x),max(fabs(y), DBL_EPSILON))+1.)) ) ) )) )

#define ismore(x,y) \
( (sizeof(x) == sizeof(float)) \
  ? ((x)>(y)+( max( 10.*FLT_EPSILON, FLT_EPSILON*pow(10., ceil(log10(max(fabs(x),max(fabs(y), FLT_EPSILON))+1.)) ) ) )) \
  : ((x)>(y)+( max( 10.*DBL_EPSILON, DBL_EPSILON*pow(10., ceil(log10(max(fabs(x),max(fabs(y), DBL_EPSILON))+1.)) ) ) )) )

/* some simpler invocations */
#define ZRO(x)   iszero(x)
#define EQ(x,y)  isequal(x,y)
#define LT(x,y)  isless2(x,y)
#define GT(x,y)  ismore(x,y)
#define LE(x,y)  (LT(x,y)||EQ(x,y))
#define GE(x,y)  (GT(x,y)||EQ(x,y))

#define squared(x) pow(x, 2.0) // added for convenience


/***************************************************
 * Function definitions
 ***************************************************/

char *Str_TrimRight(char *s);
char *Str_TrimLeft(char *s);
char *Str_TrimLeftQ(char *s); /* "quick" version */
char *Str_ToUpper(char *s, char *r);
char *Str_ToLower(char *s, char *r);
int  Str_CompareI(char *t, char *s);
void UnComment( char *s) ;
Bool Is_LeapYear(int yr);

#define GENERIC2_H
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

/* int logged is to be declared in the main module of your program. */
/* global variable indicates logfile used: externed via generic.h */
/* so we can't make it Bool.  But we don't have to explicitly */
/* extern it in each module, just include generic.h. */
/* Just be sure to set logged = FALSE as the first step in main(). */
/* See also LogError() below.  */

/*
	History:
		2011/01/27	(drs) renamed from "gen_funcs.c" to "generic.c"
		05/25/2012  (DLM) added regression() function
		05/29/2012  (DLM) added lobf(), lobfM(), & lobfB() function
		05/31/2012  (DLM) added st_getBounds() function for use in the soil_temperature function in SW_Flow_lib.c
*/

#include "generic2.h"
#include "filefuncs2.h"

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
char *Str_TrimLeft(char *s) {
/*-------------------------------------------
 * Trim left blanks from a string by moving
 * leftmost non-blank chars to beginning of string.
 * Return original pointer *s.

  cwb - 18-Nov-02
 -------------------------------------------*/

  char *q, *p;
  q = p = s;
  while ( *q && isspace((int)*(q)) ) q++; /* goto nonblank */
  while ( *q) { *(p++) = *(q++); }
  *p = '\0';

  return s;
}

/*****************************************************/
char *Str_TrimLeftQ(char *s) {
/*-------------------------------------------
 * "Quick" Trim left blanks from a string by simply
 * returning a pointer to the first non-blank
 * character in s.  Useful for Str_Dup().

  cwb - 18-Nov-02
 -------------------------------------------*/

  char *q = s;
  while ( *q && isspace((int)*q) ) q++; /* goto nonblank */

  return q;
}

/*****************************************************/
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
char *Str_ToUpper(char *s, char *r) {
/*-------------------------------------------
 * Copy s as uppercase into space pointed to by r.
 * Return r.

  cwb - 10/5/01
 -------------------------------------------*/
  char *p = s, *q = r;
  while(*p) *(q++) = (char)toupper((int)(*(p++)));
  *q = '\0';
  return r;
}


/*****************************************************/
char *Str_ToLower(char *s, char *r) {
/*-------------------------------------------
 * Copy s as lowercase into space pointed to by r.
 * Return r.

  cwb - 10/5/01
 -------------------------------------------*/
  char *p = s, *q = r;
  while(*p) *(q++) = (char) tolower((int)(*(p++)));
  *q = '\0';
  return r;
}

/*****************************************************/
int Str_CompareI(char *t, char *s) {
/*-------------------------------------------
 * works like strcmp() except case-insensitive
 * cwb 4-Sep-03
 */

   char *t1 = (char *) malloc(strlen(t)+1);
   char *s1 = (char *) malloc(strlen(s)+1);
   int r = strcmp(Str_ToUpper(t,t1), Str_ToUpper(s,s1));
   free(t1); free(s1);
   return r;
}

/*****************************************************/
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

Bool Is_LeapYear(int yr) {
/* yr must be a 4 digit year number */

   int t = (yr/100) * 100;

   return (Bool) ( (( yr % 4)  == 0  ) &&
                 ( (( t     )  != yr) ||
                 (( yr % 400) == 0  )
                 ) );
}

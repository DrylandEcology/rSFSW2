#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .C calls */
extern void dailyWeather2_R(int* nSites, double latitudes[], double longitudes[],
  int* year, int* month, int* type);
extern void dailyWeather2Write_R(int* nSites, char* siteNames[], char* siteDirs[],
  int* year);
extern void monthlyClimate2_R(int* nSites, double latitudes[], double longitudes[],
  char* siteDirs[], int* yearLow, int* yearHigh, int* type);
extern void writeMonthlyClimate2_R(char **siteDir);

static R_NativePrimitiveArgType dailyWeather2_R_t[6] = {INTSXP, REALSXP, REALSXP, INTSXP,
  INTSXP, INTSXP};
static R_NativePrimitiveArgType dailyWeather2Write_R_t[4] = {INTSXP, STRSXP, STRSXP,
  INTSXP};
static R_NativePrimitiveArgType monthlyClimate2_R_t[7] = {INTSXP, REALSXP, REALSXP,
  STRSXP, INTSXP, INTSXP, INTSXP};
static R_NativePrimitiveArgType writeMonthlyClimate2_R_t[1] = {STRSXP};

static const R_CMethodDef CEntries[] = {
    {"dailyWeather2_R",        (DL_FUNC) &dailyWeather2_R,        6, dailyWeather2_R_t},
    {"dailyWeather2Write_R",   (DL_FUNC) &dailyWeather2Write_R,   4, dailyWeather2Write_R_t},
    {"monthlyClimate2_R",      (DL_FUNC) &monthlyClimate2_R,      7, monthlyClimate2_R_t},
    {"writeMonthlyClimate2_R", (DL_FUNC) &writeMonthlyClimate2_R, 1, writeMonthlyClimate2_R_t},
    {NULL, NULL, 0, NULL}
};

/* .Call calls */
extern SEXP rSFSW2_germination_wait_times(SEXP, SEXP);
extern SEXP rSFSW2_get_KilledBySoilLayers(SEXP, SEXP);
extern SEXP rSFSW2_setFALSE_SeedlingSurvival_1stSeason(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"rSFSW2_germination_wait_times",              (DL_FUNC) &rSFSW2_germination_wait_times,              2},
    {"rSFSW2_get_KilledBySoilLayers",              (DL_FUNC) &rSFSW2_get_KilledBySoilLayers,              2},
    {"rSFSW2_setFALSE_SeedlingSurvival_1stSeason", (DL_FUNC) &rSFSW2_setFALSE_SeedlingSurvival_1stSeason, 5},
    {NULL, NULL, 0}
};

/* Register package calls with R */
void R_init_rSFSW2(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
/* Cannot enforce native symbol use because of how Rcpp::compileAttributes() creates
  'R/RcppExports.R' as of Rcpp v0.12.10
    R_forceSymbols(dll, TRUE);
*/
}

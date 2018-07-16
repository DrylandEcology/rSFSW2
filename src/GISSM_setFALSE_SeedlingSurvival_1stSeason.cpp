#include <Rcpp.h>
using namespace Rcpp;

//' Determine seedling survival in the first season (\var{\sQuote{ss1s}})
//'
//' @section Note: The \pkg{Rcpp} version of the function is about 270x faster
//'  for vectors of length 365 and 12,000x faster for vectors of length 11,000
//'  than the R version. The \pkg{Rcpp} version also reduced the memory
//'  footprint by a factor of >> 3080.
//' @section Note: Previous name \code{setFALSE_SeedlingSurvival_1stSeason}.
//'
//' @section C code: \code{ss1s} is a pointer to the data and the original
//'  vector will get altered; one would need for a deep copy:
//'  \code{LogicalVector out = clone(ss1s)}
//'
//' @references Schlaepfer, D.R., Lauenroth, W.K. & Bradford, J.B. (2014).
//'  Modeling regeneration responses of big sagebrush (Artemisia tridentata)
//'  to abiotic conditions. Ecol Model, 286, 66-77.
//'
//' @examples
//'  # The \pkg{Rcpp} function is equivalent to the following R version
//'    kill_seedling_R <- function(ss1s, ry_year_day, ry_useyrs, y,
//'      doy) {
//'      ss1s[ry_year_day == ry_useyrs[y]][doy] <- FALSE
//'      ss1s
//'    }
//'
//' @export
// [[Rcpp::export]]
LogicalVector kill_seedling(LogicalVector& ss1s,
  const IntegerVector& ry_year_day, const IntegerVector& ry_useyrs, int y,
  int doy) {

  int i, n = ry_year_day.size();

  // throw input errors
  if (n != ss1s.size() || ry_useyrs.size() < y ||
      ry_useyrs[y - 1] > max(ry_year_day) || ry_useyrs[y - 1] < min(ry_year_day)) {
    throw std::invalid_argument("'kill_seedling': invalid arguments.");
  }

  // calculate
  for (i = 0; i < n && ry_year_day[i] != ry_useyrs[y -  1]; ++i); // y is a 1-based index to ry_useyrs

  // throw error
  if (i + doy > n) {
    throw std::runtime_error("'kill_seedling': doy too large for given year 'y'");
  }

  // assumes increasingly sorted vector ry_year_day
  // doy is a 1-based index
  ss1s[i + doy - 1] = false;

  return Rcpp::wrap(ss1s);
}

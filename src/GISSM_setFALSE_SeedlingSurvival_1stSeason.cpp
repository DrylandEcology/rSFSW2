#include <Rcpp.h>
using namespace Rcpp;

// Note: ss1s is a pointer to the data and the original vector will get altered; one would need for a deep copy: LogicalVector out = clone(ss1s)

// [[Rcpp::export]]
LogicalVector setFALSE_SeedlingSurvival_1stSeason(LogicalVector& ss1s,
    const IntegerVector& ry_year_day, const IntegerVector& ry_useyrs, int y, int doy) {
  int i, n = ry_year_day.size();

  // throw input errors
  if (n != ss1s.size() || ry_useyrs.size() < y ||
      ry_useyrs[y - 1] > max(ry_year_day) || ry_useyrs[y - 1] < min(ry_year_day)) {
    throw std::invalid_argument("'setFALSE_SeedlingSurvival_1stSeason': invalid arguments.");
  }

  // calculate
  for (i = 0; i < n && ry_year_day[i] != ry_useyrs[y -  1]; ++i); // y is a 1-based index to ry_useyrs

  // throw error
  if (i + doy > n) {
    throw std::runtime_error("'setFALSE_SeedlingSurvival_1stSeason': doy too large for given year 'y'");
  }

  // assumes increasingly sorted vector ry_year_day
  // doy is a 1-based index
  ss1s[i + doy - 1] = false;

  return Rcpp::wrap(ss1s);
}

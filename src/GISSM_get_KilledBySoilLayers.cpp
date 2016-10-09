#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector get_KilledBySoilLayers(const IntegerVector& relevantLayers, const LogicalMatrix& kill_conditions) {
  int n = relevantLayers.size();

  // catch errors
  if (max(relevantLayers) > kill_conditions.ncol() ||
      n > kill_conditions.nrow() ||
      is_true(any(relevantLayers < 0))) {
    throw std::invalid_argument("'get_KilledBySoilLayers': inadmissible value(s) of relevantLayers");
  }

  // calculate
  int i, j;
  IntegerVector killed(n);
  for (i = 0; i < n; i++) {
    if (IntegerVector::is_na(relevantLayers[i])) {
      killed[i] = NA_INTEGER;

    } else {
      for (j = 0; j < relevantLayers[i] && kill_conditions(i, j); ++j) ;

      killed[i] = (j == relevantLayers[i] && kill_conditions(i, j - 1) ? 1 : 0);
    }
  }

  return Rcpp::wrap(killed);
}

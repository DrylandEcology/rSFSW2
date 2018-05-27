#include <Rcpp.h>
using namespace Rcpp;

//' Determine if all conditions across rooted soil layers are deadly
//'
//' Function that checks whether all relevant (those with roots) soil layers
//'  are under conditions of mortality (kill.conditions) for each day of a
//'  given year
//'
//'  \code{relevantLayers} takes either \code{NA} if no soil layers should be
//'  considered (e.g., because not yet germinated), or an integer number
//'  between 1 and the number of simulated soil layers. The number indicates
//'  the depth to which a seedling has grown roots and over which layers
//'  \code{kill.conditions} will be evaluated.
//'
//' @section Note: The \pkg{Rcpp} version of the function is about 165x
//'  faster than the version previous to commit
//'  \var{6344857a9cdb08acf68fa031c43cf4a596613aad} 'Small speed improvements'
//'  and about 70x faster than the R version. The \pkg{Rcpp} version also
//'  reduced the memory footprint by a factor of 200.
//'
//' @param relevantLayers An integer vector, usually of length 365 or 366
//'  (days).
//' @param kill.conditions A m x p logical matrix with
//'  \code{m >= length(relevantLayers)} and p represents the number of
//'  simulated soil layers, i.e., \code{p >= max(relevantLayers, na.rm = TRUE)}.
//'
//' @references Schlaepfer, D.R., Lauenroth, W.K. & Bradford, J.B. (2014).
//'  Modeling regeneration responses of big sagebrush (Artemisia tridentata)
//'  to abiotic conditions. Ecol Model, 286, 66-77.
//'
//' @return A logical vector of the length of \code{relevantLayers} with
//'  values containing \code{NA} for days when conditions were not evaluated,
//'  \code{TRUE} if all relevant soil layers (columns) of \code{kill.conditions}
//'  were \code{TRUE}, and with \code{FALSE} otherwise
//'
//' @examples
//'  # The \pkg{Rcpp} function is equivalent to the following R version
//'     get_KilledBySoilLayers_R <- function(relevantLayers, kill.conditions) {
//'       vapply(seq_along(relevantLayers), function(k) {
//'           if (all(is.finite(relevantLayers[k]))) {
//'             all(as.logical(kill.conditions[k, seq_len(relevantLayers[k])]))
//'           } else NA
//'         }, FUN.VALUE = NA)
//'    }
//'
//' @export
// [[Rcpp::export]]
LogicalVector get_KilledBySoilLayers(const IntegerVector& relevantLayers,
  const LogicalMatrix& kill_conditions) {

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

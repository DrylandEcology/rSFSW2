#include <Rcpp.h>
using namespace Rcpp;

//' Determine wait times until germination based on information on favorable conditions
//'   and time required to germinate
//'
//' @section Note: The Rcpp version of the function is about 270x faster for vectors of
//'  length 365 and 12,000x faster for vectors of length 11,000 than the R version.
//'  The Rcpp version also reduced the memory footprint by a factor of >> 3080.
//'
//' @references Schlaepfer, D.R., Lauenroth, W.K. & Bradford, J.B. (2014). Modeling
//'  regeneration responses of big sagebrush (Artemisia tridentata) to abiotic conditions.
//'  Ecol Model, 286, 66-77.
//'
//' @examples
//'  # The Rcpp function is equivalent to the following R version
//'    germination_wait_times_R <- function(time_to_germinate, duration_fave_cond) {
//'      N <- length(time_to_germinate)
//'      stats::na.exclude(unlist(lapply(seq_len(N), function(t) {
//'        if (is.finite(time_to_germinate[t])) {
//'          t1 <- duration_fave_cond[t:N]
//'          t2 <- stats::na.exclude(t1)
//'          t3 <- which(t2[time_to_germinate[t]] == t1)[1]
//'          sum(is.na(t1[1:t3]))
//'        } else {
//'          NA
//'        }
//'      })))
//'    }
//'
//' @export
// [[Rcpp::export]]
IntegerVector germination_wait_times(const IntegerVector& time_to_germinate,
    const IntegerVector& duration_fave_cond) {
  int n = time_to_germinate.size();

  // throw input errors
  if (n != duration_fave_cond.size()) {
    throw std::invalid_argument("'germination_wait_times': arguments must be of identical length");
  }

  // calculate
  int i, j = 0, t1, n_nas;
  int k = sum(!is_na(time_to_germinate));
  IntegerVector out(k);

  for (i = 0; i < n; ++i) {

    if (!IntegerVector::is_na(time_to_germinate[i])) {
      // throw error if germination takes too long
      if (IntegerVector::is_na(duration_fave_cond[i]) ||
          time_to_germinate[i] > duration_fave_cond[i]) {
        throw std::runtime_error("'germination_wait_times': values of time_to_germinate are larger than those of duration_fave_cond (or the latter are NAs)");
      }

      n_nas = 0;
      // count NAs between i and i + time_to_germinate[i]
      for (t1 = 0; t1 - n_nas < time_to_germinate[i] && i + t1 < n; ++t1) {
        if (IntegerVector::is_na(duration_fave_cond[i + t1])) ++n_nas;
      }

      out[j++] = n_nas;
    }
  }

  return Rcpp::wrap(out);
}

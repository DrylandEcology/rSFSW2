% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ExtractData_ClimateDownscaling.R
\name{doQmapQUANT}
\alias{doQmapQUANT}
\alias{doQmapQUANT.default_drs}
\alias{doQmapQUANT_drs}
\title{Apply a quantile mapping}
\usage{
doQmapQUANT.default_drs(
  x,
  fobj,
  type = NULL,
  lin_extrapol = NULL,
  spline_method = NULL,
  monthly_extremes = NULL,
  fix_spline = NULL,
  ...
)

doQmapQUANT_drs(
  x,
  fobj,
  type_map = NULL,
  monthly_obs_base = NULL,
  monthly_extremes = NULL,
  fix_spline = NULL,
  ...
)
}
\arguments{
\item{x}{A numeric vector. The values to map.}

\item{fobj}{An object of class \code{\link[qmap]{fitQmapQUANT}}.}

\item{type}{A character string. Type of interpolation between the fitted transformed
values. See details.}

\item{lin_extrapol}{A character string. Type of extrapolation when interpolation is
linear. See details.}

\item{spline_method}{A character string. Type of spline, passed to
\code{\link[stats]{splinefun}} as \code{method} argument. The type
\var{\dQuote{monoH.FC}} is the only appropriate method here because quantile mapping
requires a monotone function if possible.}

\item{monthly_extremes}{A numeric vector of length two. The first element suggests a
monthly minimum value and the second element a monthly maximum value for the mapped
output.}

\item{fix_spline}{A character string. See details.}

\item{...}{Additional arguments are ignored.}

\item{type_map}{A character vector. The type of interpolation, extrapolation, and
spline passed to \code{\link{doQmapQUANT.default_drs}}. Possible values include
\var{\dQuote{linear_Boe}}, \var{\dQuote{linear_Thermessl2012CC.QMv1b}},
\var{\dQuote{linear_none}}, \var{\dQuote{tricub_fmm}}, \var{\dQuote{tricub_monoH.FC}},
\var{\dQuote{tricub_natural}}, and \var{\dQuote{normal_anomalies}}. See details.}

\item{monthly_obs_base}{A numeric vector. Base values used to calculate t-scores of
\code{x} which are only used if \code{type_map} is \var{\dQuote{normal_anomalies}}.}
}
\value{
A numeric vector of the length of \code{x}. Return values differ among repeated
 calls with identical input arguments if jitter correction (using random numbers) is
 applied, i.e., \code{type} is \var{\dQuote{spline}}, \code{fix_spline} is
 \var{\dQuote{attempt}} and there are values outside the range suggested by
 \code{monthly_extremes}.
}
\description{
Whereas the function \code{\link[qmap]{fitQmapQUANT}} estimates values of the empirical
cumulative distribution function of observed and modeled time series for regularly
spaced quantiles. \code{doQmapQUANT.default_drs} uses these estimates to perform
quantile mapping.
}
\section{Details}{
 \itemize{
 \item \code{type} takes one of two possible values \itemize{
   \item \var{\dQuote{linear}}: linear interpolation using \code{\link[stats]{approx}}
   \item \var{\dQuote{tricub}}: monotonic tricubic spline interpolation using
     \code{\link[stats]{splinefun}}. Splines may result in abnormally high output
     values which appears to be due to at least two reasons: \enumerate{
       \item extrapolation errors
       \item huge oscillations in the spline-function which arise from non-monotone
         splines (\code{spline_method} is \var{\dQuote{fmm}} or \var{\dQuote{natural}})
         or which arise from numerical instabilities in the exact monotonicity if
         \code{spline_method} is \var{\dQuote{monoH.FC}}.
       }
 }
 \item \code{lin_extrapol} is the extrapolation for values of \code{x} that are outside
   \code{range(fobj[["par"]]$modq)}. This argument is only in effect if \code{type}
   is \var{\dQuote{linear}} and takes one of three possible values \itemize{
   \item \var{\dQuote{none}}: no linear extrapolation is performed, i.e., output of
     \code{\link[stats]{approx}} for type = 2 is return; that is 'values at the
     closest data extreme'.
   \item \var{\dQuote{Boe}}: constant extrapolation from Boe et al. 2007
   \item \var{\dQuote{Thermessl2012CC.QMv1b}}: same extrapolation as Boe et al. 2007,
     but not including three largest/smallest values, from Themessl et al. 2012.
 }
 \item \code{fix_spline} takes one of three values \itemize{
   \item \var{\dQuote{none}}: No correction to mapped values is applied.
   \item \var{\dQuote{fail}}: If mapped values fall outside the range suggested by
     \code{monthly_extremes}, then an error is generated.
   \item \var{\dQuote{attempt}}: The spline-based mapping is repeated up to ten times
     where the values of the quantile map \code{fobj} are jittered.
 }
 }

 \itemize{
 \item \code{type_map} with \var{\dQuote{normal_anomalies}} represents a 'linear
 interpolation with extrapolation following Boe et al. 2007 and a correction using
 standard anomalies (i.e. number of standard deviations from the mean) for values
 outside the observed quantile map that is based on Tohver et al. 2014 (Appendix A)}
}

\references{
Boe, J., L. Terray, F. Habets, and E. Martin. 2007.
 Statistical and dynamical downscaling of the Seine basin climate for
 hydro-meteorological studies. International Journal of Climatology 27:1643-1655.

Themessl, M. J., A. Gobiet, and G. Heinrich. 2011. Empirical-statistical
 downscaling and error correction of regional climate models and its impact on the
 climate change signal. Climatic Change 112:449-468.

Gudmundsson, L., J. B. Bremnes, J. E. Haugen, and T. Engen-Skaugen. 2012.
 Technical Note: Downscaling RCM precipitation to the station scale using statistical
 transformations - a comparison of methods. Hydrology and Earth System Sciences
 16:3383-3390.

Tohver, I. M., A. F. Hamlet, and S.-Y. Lee. 2014. Impacts of 21st-Century
 Climate Change on Hydrologic Extremes in the Pacific Northwest Region of North
 America. Journal of the American Water Resources Association 50:1461-1476.
}
\seealso{
Based on code from \code{\link[qmap]{doQmapQUANT}} v1.0.4 (Gudmundsson et al.
 2012), but with additional methods and more granular control. See details.
}

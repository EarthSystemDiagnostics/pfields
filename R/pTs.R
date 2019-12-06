##' Create a pTs object
##'
##' This function takes a base R vector, matrix, or ts object and converts it to
##' a \code{"pTs"} object.
##'
##' \code{"pTs"} (\code{"proxy time series"}) objects are enhanced time-series
##' \code{\link[stats]{ts}} objects. \code{pTs()} adds attributes such as
##' longitude and latitude to a time series vector/time series vectors and
##' assigns the class \code{"pTs"} to the resulting object. More than one time
##' series vectors need to have the same time basis and can either stem from the
##' same location (i.e. different data sets/proxies recorded at the same site,
##' thus \code{lat}, \code{lon} are of length 1), or it can be the same proxy
##' data recorded at different sites (i.e. \code{lat}, \code{lon} are vectors of
##' the same length > 1). The latter can be used for irregular data fields which
##' cannot be shaped into a \code{\link{pField}} object.
##' @param data a vector, matrix or ts object to be converted. If missing, a
##' matrix, filled with \code{NA} values, with dimensions according to the
##' lengths of \code{time} and \code{name} is created. For this, \code{time}
##' needs to be non-\code{NULL}.
##' @param time vector of time points corresponding to the observations in the
##' proxy time series; must be equidistant. If \code{NULL} (the default), a
##' vector of length \code{length(data)} (or \code{nrow(data)}) is created
##' assuming equidistant time steps with stepsize 1. For this, \code{data}
##' needs to be non-\code{NULL}.
##' @param lat numeric vector giving the latitude(s) of the sampling location(s)
##' of the proxy time series.
##' @param lon vector of length 1 giving the longitude(s) of the sampling
##' location(s) of the proxy time series.
##' @param name vector of character strings with the names of the \code{"pTs"}
##' object to be generated (optional).
##' @param history character string to append to the history attribute of the
##'  \code{"pTs"} object (optional).
##' @param date logical, whether or not to append the current date to the
##' history attribute; defaults to \code{TRUE}.
##' @param kTol tolerance to check for equidistance of time steps; defaults to
##' 1/400.
##' @return An object of class \code{"pTs"}.
##' @seealso \code{\link{pField}}
##' @source Function copied from "proxytype.R" in paleolibary/src/.
##' @author Thomas Laepple; adapated by Thomas Münch
##' @examples
##' # 10 data sets from the same site (e.g. Kohnen Station, Antarctica)
##'
##' dat <- matrix(rnorm(10 * 100), nrow = 100, ncol = 10)
##' dat.pts <- pTs(dat, lat = -75, lon = 0,
##'                name = "10 proxy time series from Kohnen.")
##'
##' # time vector is automatically created
##' time(dat.pts)
##'
##' # supply a specific time vector
##' dat.pts <- pTs(dat, time = 1901 : 2000, lat = -75, lon = 0,
##'                name = "10 proxy time series from Kohnen.")
##' time(dat.pts)
##'
##' # supply data set names
##' dat.pts <- pTs(dat, time = 1901 : 2000, lat = -75, lon = 0,
##'                name = paste("proxy", 1 : 10))
##'
##' # 10 data sets from different sites
##' # (= irregular grid which cannot be shaped into a pField)
##'
##' lat <- seq(-75, -85, length.out = 10)
##' lon <- seq(0, 90, length.out = 10)
##'
##' dat.pts <- pTs(dat, time = 1901 : 2000, lat = lat, lon = lon,
##'                name = "Proxy time series from 10 sites.")
##' @export
pTs <- function(data,
                time = NULL,
                lat = NA,
                lon = NA,
                name = "",
                history = "",
                date = TRUE,
                kTol = 1 / 400) {

  # Input and error checking

  if (missing(data)) data <- NULL

  if (is.null(data) & is.null(time)) {
    stop("Missing input: only one of 'data' or ' time' allowed to be NULL.")
  }

  # Create default data or time objects if needed
  
  if (is.null(data)) {

    if (length(name) == 1) {
      data <- drop(matrix(NA, length(time)))
    } else {
      data <- matrix(NA, length(time), length(name))
    }
  }

  if (is.null(time)) {
    
    if (is.null(dim(data))) {
      time <- seq(length.out = length(data))
    } else {
      time <- seq(length.out = nrow(data))
    }
  }

  # Check user-supplied data

  nlon <- length(lon)
  nlat <- length(lat)

  if (nlon != nlat) stop("'lat' and 'lon' need to be of the same length.")
  
  if (!is.null(ncol(data)) && (ncol(data) > 1)) {
    
    # true matrix
    if (nrow(data) != length(time))
      stop("Number of timesteps does not match number of data rows.")
    if (nlon > 1 && nlon != ncol(data))
      stop("Number of lat-lon pairs does not match ncol(data).")
    
  } else {

    # only one data set
    if (length(data) != (length(time)))
      stop("Number of timesteps does not match length of data vector.")
    if (nlon > 1)
      stop("Only one data set but number of lat-lon pairs > 1.")

    # drop possible redundant data set column with a warning
    if (!is.null(ncol(data)) && (ncol(data) == 1)) {
      warning("Redundant data set column dropped in output.",
              call. = FALSE)
      data <- drop(data)
    }
  }

  # Create vector/matrix of time series object(s)

  if (length(time) > 1) {
    
    # real time series
    if (abs(max(diff(time)) - min(diff(time))) > kTol)
      stop("Time steps are not equidistant.")

    result <- ts(data, start = time[1], deltat = (time[2] - time[1]))

  } else {

    # only one time step exists
    result <- ts(data, start = time[1])
  }

  # Put attributes and classes
  
  attr(result, "lat") <- lat
  attr(result, "lon") <- lon
  attr(result, "name") <- name

  if (sum(nchar(history)) == 0) {
    attr(result, "history") <- GetHistory(data)
  } else {
    attr(result, "history") <- GetHistory(data)
    result <- AddHistory(result, history, date = date)
  }

  attr(result, "oclass") <- class(result)
  attr(result, "nclass") <- c("pTs", "ts")
  class(result) <- attr(result, "nclass")

  invisible(result)
}


##' Create a pField object
##'
##' This function takes supplied vectors of time points, latitudes and
##' longitudes to create an empty or convert given data into a \code{"pField"}
##' object.
##'
##' The function creates or shapes given data into a \code{"pfield"} object:
##' \itemize{
##'   \item The return object is a two-dimensional array of class
##'     \code{"pfield"} with columns corresponding to the spatial dimension
##'     (latitude-longitude pair) and rows corresponding to the time points of
##'     the observations.
##'   \item Each time series at a spatial location is a \code{\link[stats]{ts}}
##'     object.
##'   \item Longitude and latitude information are included as attributes along
##'     with a history attribute for change log information.
##'   \item If no input data is supplied, an empty matrix is created according
##'     to the information provided in \code{time}, \code{lat} and \code{lon}.
##'   \item If \code{data} is supplied of length > 1, its total number of
##'     observation points must match the product of the lengths of \code{time},
##'     \code{lat} and \code{lon}.
##'   \item Note that the order of observations in the input data is assumed to
##'     follow the typical structure of netcdf files, i.e. the observations are
##'     incremented first along longitudes, then along latitudes, and finally
##'     along time.
##' }
##' @param data input data; can be \code{NULL}, a numeric vector or an array of
##' any dimension, but note the assumed order of observations (see details). If
##' \code{NULL} or a single value (length-1 vector), an empty \code{"pfield"}
##' object or a \code{"pfield"} object filled with the single value is created
##' with dimensions according to the information (i.e., lengths) in \code{time},
##' \code{lat} and \code{lon}.
##' @param time vector of observation time points, must be equidistant.
##' @param lat vector of (unique) latitudes.
##' @param lon vector of (unique) longitudes.
##' @param name character string with the name of the \code{"pField"} object to
##' be generated.
##' @param history character string to append to the history attribute of the
##' \code{"pField"} object (optional).
##' @inheritParams pTs
##' @return A two-dimensional array of class \code{"pField"}.
##' @source Function copied from "proxytype.R" in paleolibary/src/
##' @author Thomas Laepple; adapted by Thomas Münch
##' @examples
##' # Create an empty pfield object covering two latitudes, three longitudes and
##' # four time steps:
##'
##' lat <- c(-75, -80)
##' lon <- c(0, 135, 215)
##' time <- 1 : 4
##'
##' pf1 <- pField(lat = lat, lon = lon, time = time)
##'
##' # Now fill the object with data:
##' # the important assumption is that the order of observations follows
##' # increments first along longitudes, then along latitudes, and finally along
##' # time (structure of netcdf files). Here, we let the values increase with
##' # latitude and time only:
##'
##' space <- c(1, 1, 1, 2, 2, 2)
##' spacetime <- c(space, 10 * space, 100 * space, 1000 * space)
##'
##' pf2 <- pField(data = spacetime, lat = lat, lon = lon, time = time)
##'
##' # Note that in this case, the number of data points must match the number of
##' # observations defined by lat, lon and time:
##'
##' \dontrun{
##' x <- pField(data = spacetime, lat = -75, lon = lon, time = time)
##' }
##'
##' # Since R starts to read any array along its first dimension, all of the
##' # following array shapes are identical with respect to the pfield object
##' # created:
##'
##' # i) Matrix with rows corresponding to space and columns to time
##' data <- spacetime
##' dim(data) <- c(6, 4)
##' pf3 <- pField(data, lat = lat, lon = lon, time = time)
##'
##' # ii) Vice versa
##' data <- spacetime
##' dim(data) <- c(4, 6)
##' pf4 <- pField(data, lat = lat, lon = lon, time = time)
##'
##' # iii) Array with first dimension being longitude, second dimension being
##' #      latitude, and third dimension being time:
##' data <- spacetime
##' dim(data) <- c(3, 2, 4)
##' pf5 <- pField(data, lat = lat, lon = lon, time = time)
##'
##' # However, note that if you change the order of observations in the input,
##' # the resulting pfield object will be different from the above examples!
##'
##' data <- spacetime
##' dim(data) <- c(6, 4)
##' data <- t(data) # order of observations changes
##'
##' pf.different <- pField(data, lat = lat, lon = lon, time = time)
##' @export
pField <- function(data = NULL,
                   time = 1,
                   lat = NA,
                   lon = NA,
                   name = "",
                   history = "",
                   date = TRUE,
                   kTol = 1 / 400) {

  # Check input data

  nobs <- length(lat) * length(lon) * length(time)
  
  if (length(data) <= 1) {

    if (nobs == 0) {
      stop(paste("Latitude, longitude or time information",
                 "missing for creating empty field."))
    }

    if (is.null(data[1])) {
      
      data <- rep(NA, nobs)
      
    } else {
      
      data <- rep(data[1], nobs)

    }
  }

  if (length(data) != nobs) {

    stop(paste("Number of latitudes, longitudes and time points",
               "does not match number of elements in 'data'. If you want",
               "to create and empty field, supply 'data = NULL'."))
  }

  # Shape data into a 2D array
  dim(data) <- c(length(lat) * length(lon), length(time))

  if (length(time) > 1) {
    
    # real time series
    if (abs(max(diff(time)) - min(diff(time))) > kTol)
      stop("Time steps are not equidistant.")
    
    result <- ts(t(data), start = time[1], deltat = (time[2] - time[1]))

  } else {

    # only one time step exists
    result <- ts(t(data), start = time[1])
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
  attr(result, "nclass") <- c("pField", "ts")
  class(result) <- attr(result, "nclass")

  invisible(result)
}

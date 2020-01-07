##' Create a pField object
##'
##' This function takes supplied vectors of time points, latitudes and
##' longitudes to convert, or create, a three-dimensional gridded data array
##' into a \code{"pField"} object.
##'
##' For a supplied gridded data array, the function shapes the data array into a
##' two-dimensional array with columns corresponding to the spatial dimension
##' (latitude-longitude pair) and rows corresponding to the time points of the
##' observations, i.e. similar to the structure of netcdf files. For this, the
##' total number of observation points in \code{data} must match the product of
##' the lengths of \code{time}, \code{lat} and \code{lon}. Each time series at
##' a spatial location is converted to a time series \code{\link[stats]{ts}}
##' object; and longitudes and latitudes are added to the new array as
##' attributes. If no input array is supplied, an empty three-dimensional
##' gridded data array is created from the information provided in \code{time},
##' \code{lat} and \code{lon}, and converted in a similar manner to a 2D
##' \code{"pField"} object.
##' @param data input data, usually a three-dimensional gridded data array. If
##' \code{NULL}, or a single value, an empty data array, or a data array filled
##' with the single value, is created with dimensions according to the
##' information (i.e., lengths) in \code{time}, \code{lat} and \code{lon}.
##' @param time vector of observation time points, must be equidistant.
##' @param lat vector of latitudes.
##' @param lon vector of longitudes.
##' @param name character string with the name of the \code{"pField"} object to
##' be generated.
##' @param history character string to append to the history attribute of the
##' \code{"pField"} object (optional).
##' @inheritParams pTs
##' @return A two-dimensional array of class \code{"pField"}.
##' @source Function copied from "proxytype.R" in paleolibary/src/
##' @author Thomas Laepple; adapated by Thomas Münch
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

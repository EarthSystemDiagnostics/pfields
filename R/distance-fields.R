##' Latitude and longitude fields
##'
##' Get latitude and longitude field vectors from the latitude and longitude
##' attributes of a \code{"pField"} or \code{"pTs"} object. These fields follow
##' the same spatial pattern as the input object (i.e., for the first latitude
##' the indices run along all longitudes, then jump to the next latitude, and so
##' further).
##' @param data a \code{"pField"} or \code{"pTs"} object for which to return the
##' lat/lon fields.
##' @param simplify if \code{FALSE} (the default), the fields are returned as a
##' two component list, otherwise as a 2 x n matrix, where the first row is the
##' latitude and the second row the longitude field.
##' @return a two component list or 2 x n matrix.
##' @source Function copied from "basis.R" in paleolibary/src/.
##' @author Thomas Laepple
##' @examples
##' field <- pField(lat = seq(0, 90, 10), lon = c(-10, 0, 10))
##'
##' latlons <- GetLatLonField(field)
##' latlons$lat2d
##' latlons$lon2d
##'
##' # subset incompletely to create a pTs object
##' field <- field[, 1 : 4]
##' latlons <- GetLatLonField(field)
##' latlons$lat2d
##' latlons$lon2d
##' @aliases GetLatLonField latlonField
##' @export GetLatLonField latlonField
GetLatLonField <- function(data, simplify = FALSE) {

  lat <- GetLat(data)
  lon <- GetLon(data)

  if (is.pTs(data)) {

    lon2d <- lon
    lat2d <- lat

  } else if (is.pField(data)) {

    nlat <- length(lat)
    nlon <- length(lon)

    lon2d <- rep(lon, nlat)
    lat2d <- rep(lat, each = nlon)

  } else {

    stop("Input is whether pField nor pTs object.")
  }

  res <- list(lat2d = lat2d, lon2d = lon2d)

  if (simplify) {
    res <- t(simplify2array(res))

    # need 2nd transpose to keep structure if data is only one point
    if (nlat == 1 & nlon == 1) res <- t(res)
  }

  return(res)
}
latlonField <- function(...) {
  warning("latlonField is deprecated and replaced with GetLatLonField
            to comply with ECUS R style guide.")
  GetLatLonField(...)
}

##' Calculate distance between grid points
##'
##' Calculate the distances between a specified target location and all grid
##' points of a given \code{"pField"}. The calculation is based on the function
##' \code{\link[geosphere]{distGeo}}.
##'
##' \code{latlon.field} can be supplied for efficiency if
##' \code{GetDistanceField} needs to be called many times, e.g., to calculate
##' the distances from several target locations for the same coordinate field.
##' @param field a \code{"pField"} object.
##' @param latlon.field the optional result of calling
##' \code{\link{GetLatLonField}} for \code{field}; see details. For \code{NULL}
##' (the default), \code{\link{GetLatLonField}} is called internally.
##' @param lat numeric vector of length 1 with the latitude of the target
##' location relative to which distances are to be calculated.
##' @param lon numeric vector of length 1 with the longitude of the target
##' location.
##' @param get.nearest logical to specify whether the given latitude - longitude
##' pair shall be used *as is* as the target location (\code{FALSE}), or instead
##' the nearest grid point in \code{field} (\code{TRUE}).
##' @param index an integer array index of the input \code{"pField"} for
##' specifying the target location. Setting this will override any \code{lat},
##' \code{lon} settings, while the default \code{NULL} signals to use the
##' specified \code{lat}, \code{lon} values.
##' @param verbose logical; if \code{TRUE} an informative message regarding the
##' selected target location is printed.
##' @return Numeric vector with distances in km of the grid points of
##' \code{"field"} relative to the target location following the spatial
##' ordering of a \code{"pField"} object.
##' @seealso \code{\link{GetLatLonField}}
##' @author Thomas Münch
##' @examples
##' field <- pField(lat = seq(0, 90, 10), lon = c(-10, 0, 10))
##'
##' d <- GetDistanceField(field, lat = 5, lon = 5, verbose = TRUE)
##' range(d)
##'
##' d <- GetDistanceField(field, lat = 5, lon = 5,
##'                       get.nearest = TRUE, verbose = TRUE)
##' range(d)
##'
##' d <- GetDistanceField(field, index = 1, verbose = TRUE)
##' range(d)
##' @export
GetDistanceField <- function(field, latlon.field = NULL,
                             lat, lon, get.nearest = FALSE,
                             index = NULL, verbose = FALSE) {

  # Error checking
  
  if (missing(field)) stop("No input field specified.")

  if (is.null(index)) {
    
    args <- c(missing(lat), missing(lon))
    if (sum(args) > 0) stop("'lat' and 'lon' values needed.")

    if (length(lat) != 1 | length(lon) != 1 | length(lat) != length(lon)) {
      stop("Only single values allowed for 'lat' and 'lon'.")
    }
    
  } else {

    if (!is.numeric(index)) stop("Index non-numeric.")

    if (index > ncol(field) | index <= 0) {
      
      stop("Supplied 'index' outside valid range.")
    }
  }

  # Get field coordinates
  
  if (is.null(latlon.field)) {
    field.coord <- GetLatLonField(field, simplify = TRUE)
  } else {
    field.coord <- latlon.field
  }

  # Select target location based on input

  if (!is.null(index)) {

    point <- field.coord[, index]

    if (verbose) {
      message(
        sprintf("Index grid point used: lat = %f, lon = %f.",
                point[1], point[2]))
    }

  } else if (get.nearest) {

    x <- SelPoint(field, lat, lon)
    point <- c(GetLat(x), GetLon(x))

    if (verbose) {
      message(
        sprintf("Nearest grid point used: lat = %f, lon = %f.",
                point[1], point[2]))
    }

  } else {

    if (verbose) message("Using supplied 'lat' and 'lon' value.")
    point <- c(lat, lon)
  }

  # Get distance field
  distance.field <- geostools::GetDistance(lat0 = point[1],
                                           lon0 = point[2],
                                           lat = field.coord[1, ],
                                           lon = field.coord[2, ])

  return(distance.field)
}

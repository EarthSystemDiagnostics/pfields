##' Get name of proxy object
##'
##' The function returns the \code{"name"} attribute of a \code{"pTs"} or
##' \code{"pField"} object.
##' @param data a \code{"pTs"} or \code{"pField"} object.
##' @return Character string with the \code{"name"} attribute of \code{data}.
##' @source Function copied from "basis.R" in paleolibary/src/.
##' @author Thomas Laepple
##' @examples
##' getname(c(a = 2))
##' GetName(c(a = 2))
##' @aliases getname GetName
##' @export getname GetName
GetName <- function(data) {
  return(attr(data, "name"))
}
getname <- function(...) {
  warning("getname is deprecated and replaced with GetName
           to comply with ECUS R style guide.")
  GetName(...)
}

##' Get history of a proxy object
##'
##' The function returns the \code{"history"} attribute of a \code{"pTs"} or
##' \code{"pField"} object.
##' @inheritParams GetName
##' @family history
##' @return Character string with the \code{"history"} attribute of \code{data}.
##' @source Function copied from "basis.R" in paleolibary/src/.
##' @author Thomas Laepple
##' @examples
##' # Create empty pTs object:
##' x <- pTs(lat = -75, lon = 0, time = 1,
##'          history = sprintf("Object created."))
##'
##' GetHistory(x)
##' @aliases GetHistory gethistory
##' @export gethistory GetHistory
GetHistory <- function(data) {
  #print(match.call())
  return(attr(data, "history"))
}
gethistory <- function(...){
  warning("gethistory is deprecated and replaced with GetHistory
           to comply with ECUS R style guide.")
  GetName(...)
}

##' Amend the history of a proxy object
##'
##' This function amends the history information of a \code{"pTs"} or
##' \code{"pField"} object by adding a given character string (e.g., a comment)
##' to the \code{"history"} attribute of the object together with the date
##' information of this action.
##' @param x a \code{"pTs"} or \code{"pField"} object.
##' @param newhist character string with the history information to be added.
##' @inheritParams pTs
##' @family history
##' @return the \code{"pTs"} or \code{"pField"} object with the amended
##' history.
##' @source Function copied from "basis.R" in paleolibary/src/.
##' @author Thomas Laepple
##' @examples
##' # Create empty pTs object:
##' x <- pTs(lat = -75, lon = 0, time = 1,
##'          history = sprintf("Object created."))
##'
##' # Check history
##' GetHistory(x)
##'
##' # Some changes made
##' x <- AddHistory(x, newhist = "Changed xyz.")
##'
##' # Check new history
##' GetHistory(x)
##' @aliases addhistory AddHistory
##' @export addhistory AddHistory
AddHistory <- function(x, newhist, date = TRUE) {
  if (date) newhist <- paste(date(), newhist, sep = ": ")
  attr(x, "history") <- c(attr(x, "history"), newhist)
  return(x)
}
addhistory <- function(...) {
  warning("addhistory is deprecated and replaced with AddHistory
           to comply with ECUS R style guide.")
  AddHistory(...)
}

##' Get latitudes of proxy object
##'
##' The function returns the \code{"lat"} attribute of a \code{"pTs"} or
##' \code{"pField"} object, i.e. the latitudes of the proxy sampling locations,
##' as a numeric vector.
##' @inheritParams GetName
##' @return a numeric vector of the latitudes of the \code{"pTs"} or
##' \code{"pField"}  object.
##' @source Function copied from "basis.R" in paleolibary/src/.
##' @author Thomas Laepple
##' @examples
##' # Some pField object
##' x <- pField(lat = seq(-75, -80, -5), lon = c(0, 135, 215), time = 1 : 4)
##'
##' GetLat(x)
##' @aliases getlat GetLat
##' @export getlat GetLat
GetLat <- function(data) {
  return(attr(data, "lat"))
}
getlat <- function(...) {
  warning("getlat is deprecated and replaced with GetLat
           to comply with ECUS R style guide.")
  GetLat(...)
}

##' Get longitudes of proxy object
##'
##' The function returns the \code{"lon"} attribute of a \code{"pTs"} or
##' \code{"pField"} object, i.e. the longitudes of the proxy sampling locations,
##' as a numeric vector.
##' @inheritParams GetName
##' @return a numeric vector of the longitudes of the \code{"pTs"} or
##' \code{"pField"}  object.
##' @source Function copied from "basis.R" in paleolibary/src/.
##' @author Thomas Laepple
##' @examples
##' # Some pField object
##' x <- pField(lat = seq(-75, -80, -5), lon = c(0, 135, 215), time = 1 : 4)
##'
##' GetLon(x)
##' @aliases getlon GetLon
##' @export getlon GetLon
GetLon <- function(data) {
  return(attr(data, "lon"))
}
getlon <- function(...) {
  warning("getlon is deprecated and replaced with GetLon
           to comply with ECUS R style guide.")
  GetLon(...)
}

##' pField summary
##'
##' Method function to produce the summary for a \code{"pField"} object.
##' @param object an object of class \code{"pField"}.
##' @param ... additional arguments; currently not implemented.
##' @author Thomas Münch
##' @export
summary.pField <- function(object, ...) {

  atb <- attributes(object)

  x <- c(object)
  nas <- is.na(x)
  x <- x[!nas]
  qq <- stats::quantile(x)
  qq <- c(qq[1L:3L], mean(x), qq[4L:5L])
  names(qq) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.",
                 "Max.")
  if (any(nas)) qq <- c(qq, `NA's` = sum(nas))

  class(qq) <- c("summaryDefault", "table")

  cat("Proxy field object\n")
  cat(sprintf("\nNames: %s\n", paste(atb$name, collapse = " / ")))
  cat("\nHistory:\n")
  cat(paste(atb$history, collapse = "\n"))
  cat("\n")
  cat("\nTime range:\n")
  cat(paste0("tmin = ", min(stats::time(object)),
             ", tmax = ", max(stats::time(object)),
             ", N = ", length(stats::time(object)), "\n"))
  cat("\nSpatial extent:\n")
  cat(paste0("lat: min = ", min(atb$lat), ", max = ", max(atb$lat),
             ", N = ", length(atb$lat), "\n"))
  cat(paste0("lon: min = ", min(atb$lon), ", max = ", max(atb$lon),
             ", N = ", length(atb$lon), "\n"))

  cat("\nData range:\n")
  print(qq)

  invisible(NULL)

}

##' pTs summary
##'
##' Method function to produce the summary for a \code{"pTs"} object.
##' @param object an object of class \code{"pTs"}.
##' @param ... additional arguments; currently not implemented.
##' @author Thomas Münch
##' @export
summary.pTs <- function(object, ...) {

  atb <- attributes(object)

  x <- c(object)
  nas <- is.na(x)
  x <- x[!nas]
  qq <- stats::quantile(x)
  qq <- c(qq[1L:3L], mean(x), qq[4L:5L])
  names(qq) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.",
                 "Max.")
  if (any(nas)) qq <- c(qq, `NA's` = sum(nas))

  class(qq) <- c("summaryDefault", "table")

  cat("Proxy time series object\n")
  cat(sprintf("\nNames: %s\n", paste(atb$name, collapse = " / ")))
  cat("\nHistory:\n")
  cat(paste(atb$history, collapse = "\n"))
  cat("\n")
  cat("\nTime range:\n")
  cat(paste0("tmin = ", min(stats::time(object)),
             ", tmax = ", max(stats::time(object)),
             ", N = ", length(stats::time(object)), "\n"))
  cat("\nSpatial extent:\n")
  cat(paste0("lat: min = ", min(atb$lat), ", max = ", max(atb$lat),
             ", N = ", length(atb$lat), "\n"))
  cat(paste0("lon: min = ", min(atb$lon), ", max = ", max(atb$lon),
             ", N = ", length(atb$lon), "\n"))

  cat("\nData range:\n")
  print(qq)

  invisible(NULL)

}

##' Compactly display the structure of a pField object
##'
##' Method function to compactly display the internal structure of a
##' \code{"pField"} object, a diagnostic function and an alternative to
##' \code{summary}; performed with the \code{str} method generic to the
##' \code{"oclass"} of \code{object}.
##' @param object an object of class \code{"pField"}.
##' @param ... further arguments passed on to \code{\link[utils]{str}}.
##' @seealso \code{\link[utils]{str}}
##' @author Thomas Münch
##' @export
str.pField <- function(object, ...) {

  class(object) <- attr(object, "oclass")
  utils::str(object, ...)

}

##' Compactly display the structure of a pTs object
##'
##' Method function to compactly display the internal structure of a
##' \code{"pTs"} object, a diagnostic function and an alternative to
##' \code{summary}; performed with the \code{str} method generic to the
##' \code{"oclass"} of \code{object}.
##' @param object an object of class \code{"pTs"}.
##' @param ... further arguments passed on to \code{\link[utils]{str}}.
##' @seealso \code{\link[utils]{str}}
##' @author Thomas Münch
##' @export
str.pTs <- function(object, ...) {

  class(object) <- attr(object, "oclass")
  utils::str(object, ...)

}

##' pField object
##'
##' Check if object is a \code{"pField"} object.
##' @param object object to be tested.
##' @return \code{TRUE} or \code{FALSE} depending on whether \code{object} is a
##' \code{"pField"} or not.
##' @author Thomas Laepple
##' @examples
##' # Some pField object
##' x <- pField(lat = seq(-75, -80, -5), lon = c(0, 135, 215), time = 1 : 10)
##'
##' is.pField(x)
##' is.pField(1 : 10)
##' @export
is.pField <- function(object) {

  sum(class(object) == "pField") > 0
}

##' pTs object
##'
##' Check if object is a \code{"pTs"} object.
##' @param object object to be tested.
##' @return \code{TRUE} or \code{FALSE} depending on whether \code{object} is a
##' \code{"pTs"} object or not.
##' @author Thomas Laepple
##' @examples
##' # Some pTs object
##' x <- pTs(lat = -75, lon = 0, time = 1 : 10)
##'
##' is.pTs(x)
##' is.pTs(1 : 10)
##' @export
is.pTs <- function(object) {

  sum(class(object) == "pTs") > 0
}

##' Convert field to data frame
##'
##' Convert a static \code{"pField"} or \code{"pTs"} object, i.e. including only
##' one time step, to a data frame with data, latitude and longitude columns,
##' with the possibility to cut out a specified latitude-longitude region.
##' @param data a \code{"pField"} or \code{"pTs"} object with one timestep.
##' @param lat.min set the minimum latitude for the output; per default the
##' minimum in \code{data} is used.
##' @param lat.max set the maximum latitude for the output; per default the
##' maximum in \code{data} is used.
##' @param lon.min set the minimum longitude for the output; per default the
##' minimum in \code{data} is used.
##' @param lon.max set the maximum longitude for the output; per default the
##' maximum in \code{data} is used.
##' @return A data frame with the three columns \code{lat} (latitudes),
##'   \code{lon} (longitudes) and \code{dat} (the values of \code{data} at the
##'   coordinate positions).
##' @author Thomas Münch
##' @examples
##' # Create a pfield object covering two latitudes, three longitudes and
##' # four time steps, let data values increase with latitude and time only:
##' lat <- seq(-75, -80, -5)
##' lon <- c(0, 135, 215)
##' time <- 1 : 4
##' space <- c(1, 1, 1, 2, 2, 2)
##' spacetime <- c(space, 10 * space, 100 * space, 1000 * space)
##'
##' x <- pField(data = spacetime, lat = lat, lon = lon, time = time)
##'
##' # Average across time
##' x.avg <- ApplyTime(x, mean)
##'
##' # Convert to data frame
##' x.df <- pField2df(x.avg)
##' x.df
##'
##' # Cut out some region
##' x.df <- pField2df(x.avg, lat.min = -75, lon.min = 0, lon.max = 200)
##' x.df
##'
##' # Subset incompletely to create a pTs object
##' x.pts <- x[, 1 : 4]
##' x.avg <- ApplyTime(x.pts, mean)
##'
##' # Convert to data frame
##' x.df <- pField2df(x.avg)
##' x.df # the same as x.df[1 : 4,] from above
##' @export
pField2df <- function(data,
                      lat.min = NULL, lat.max = NULL,
                      lon.min = NULL, lon.max = NULL) {

  if (nrow(data) != 1)
    stop("Method not yet suited for more than 1 timestep.")

  coord.field <- GetLatLonField(data)

  df <- data.frame(
    lat = coord.field$lat2d,
    lon = coord.field$lon2d,
    dat = c(data[1, ]))

  df$lon[df$lon > 180] <- df$lon[df$lon > 180] - 360

  if (!length(lat.min)) lat.min <- min(df$lat)
  if (!length(lat.max)) lat.max <- max(df$lat)
  if (!length(lon.min)) lon.min <- min(df$lon)
  if (!length(lon.max)) lon.max <- max(df$lon)

  i <- which(df$lat >= lat.min & df$lat <= lat.max)
  df <- df[i, ]

  i <- which(df$lon >= lon.min & df$lon <= lon.max)
  df <- df[i, ]

  df$lon[df$lon < 0] <- df$lon[df$lon < 0] + 360

  return(df)

}

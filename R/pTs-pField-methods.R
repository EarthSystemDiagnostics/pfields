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

##' Apply a function across space
##'
##' Apply a function across the spatial dimension of a \code{"pField"} object by
##' using \code{apply} on the rows of the object. The result of this gives a
##' \code{"pTs"} object.
##' @param data a \code{"pField"} object.
##' @param FUN the function to be applied.
##' @param ... further arguments passed on to \code{FUN}.
##' @return a \code{"pTs"} object with the results of the function applied.
##' @source Function copied from "basis.R" in paleolibary/src/.
##' @author Thomas Laepple
##' @examples
##' @aliases applyspace ApplySpace
##' @export applyspace ApplySpace
ApplySpace <- function(data, FUN, ...) {

    if (is.null(ncol(data))) {

        stop("Input seems not to be a pField object.")

    } else {

        index <- which(!is.na(colSums(data)))

        if (length(index) == 0) {

            stop("No non-NA columns in input to apply FUN on.")

        } else if (length(index) == 1) {

            warning(paste("Only 1 non-NA column in input;",
                          "returning original data from there."))
            res <- data[, index]

        } else {

            ts <- apply(data[, index], 1, FUN, ...)
            attr(ts, "history") <- GetHistory(data)
            res <- pTs(data = ts, time = time(data),
                       name = GetName(data),
                       history = sprintf("ApplySpace: %s",
                                         deparse(substitute(FUN))))

        }
    }

    return(res)
}
applyspace <- function(...) {
    warning("applyspace is deprecated and replaced with ApplySpace
            to comply with ECUS R style guide.")
    ApplySpace(...)
}

##' Apply a function across time
##'
##' Apply a function across the temporal dimension of a \code{"pField"} object by
##' using \code{apply} on the columns of the object. The result of this gives
##' a \code{"pField"} object with only one time step.
##' @param data a \code{"pField"} object.
##' @param FUN the function to be applied.
##' @param newtime Specify the observation time point corresponding to the
##' resulting \code{"pField"} object. For \code{NULL} (the default), the average
##' of the time points in \code{data} is used.
##' @param ... further arguments passed on to \code{FUN}.
##' @return a \code{"pField"} object with the results of the function applied.
##' @source Function copied from "basis.R" in paleolibary/src/.
##' @author Thomas Laepple
##' @examples
##' @aliases applytime ApplyTime
##' @export applytime ApplyTime
ApplyTime <- function(data, FUN, newtime = NULL, ...) {
    
    if (is.null(newtime)) {
        newtime <- mean(time(data))
    }

    field <- apply(data, 2, FUN, ...)
    attr(field, "history") <- GetHistory(data)
    res <- pField(data = field, time = newtime,
                  lat = GetLat(data), lon = GetLon(data),
                  name = GetName(data),
                  history = sprintf("ApplyTime: %s",
                                    deparse(substitute(FUN))))
    
    return(res)
}
applytime <- function(...) {
    warning("applytime is deprecated and replaced with ApplyTime
            to comply with ECUS R style guide.")
    ApplyTime(...)
}

##' Apply function on two fields
##'
##' Apply a function, which expects two arguments, row-wise, i.e. across space,
##' on two \code{"pField"} objects. The result of this gives a \code{"pField"}
##' object with only one time step.
##'
##' Note that the currect implementation expects both fields to have the exact
##' same time basis, latitudes and longitudes; any differences will result in an
##' error.
##' @param fld1 a \code{"pField"} object.
##' @param fld2 a \code{"pField"} object.
##' @param FUN the function to be applied on \code{fld1} and \code{fld2}.
##' @param newtime Specify the observation time point corresponding to the
##' resulting \code{"pField"} object. For \code{NULL} (the default), the average
##' of the time points in \code{fld1} is used.
##' @param ... further arguments passed on to \code{FUN}.
##' @return a \code{"pField"} object with the results of the function applied.
##' @author Thomas Münch
##' @examples
##' x <- pField(data = array(rnorm(10 * 10 * 100), dim = c(10, 10, 100)),
##'             time = 1 : 100, lat = 1 : 10, lon = 1 : 10)
##' y <- pField(data = array(rnorm(10 * 10 * 100), dim = c(10, 10, 100)),
##'             time = 1 : 100, lat = 1 : 10, lon = 1 : 10)
##' z <- ApplyFields(x, y, cor)
##' @export
ApplyFields <- function(fld1, fld2, FUN, newtime = NULL, ...) {

    # Check dimensions of input objects

    if (sum(dim(fld1)) != sum(dim(fld2))) {
        stop("Supplied fields have different dimensions.")
    }

    # Check fields for same time and Lat-Lon basis

    if (max(abs(time(fld1) - time(fld2))) != 0) {
        stop("Supplied fields have different observation times.")
    }
    if (max(abs(GetLat(fld1) - GetLat(fld2))) != 0) {
        stop("Supplied fields have different latitudes.")
    }
    if (max(abs(GetLon(fld1) - GetLon(fld2))) != 0) {
        stop("Supplied fields have different longitudes.")
    }

    if (is.null(newtime)) {
        newtime <- mean(time(fld1))
    }
    
    # Convert to data frames for use in mapply
    
    x <- as.data.frame(fld1)
    y <- as.data.frame(fld2)

    # Apply FUN
    
    more.args <- list(...)
    if (length(more.args) == 0) more.args <- NULL

    field <- mapply(FUN, x, y, MoreArgs = more.args,
                    SIMPLIFY = TRUE, USE.NAMES = FALSE)

    attr(field, "history") <- c(
        sprintf("A (%s):", GetName(fld1)), GetHistory(fld1),
        sprintf("B (%s):", GetName(fld2)), GetHistory(fld2))

    res <- pField(data = field, time = newtime,
                  lat = GetLat(fld1), lon = GetLon(fld1),
                  name = paste(GetName(fld1), GetName(fld2), sep = ", "),
                  history = paste0(as.character(substitute(FUN)), "(",
                                GetName(fld1), ", ", GetName(fld2), ")"))

    return(res)
}

##' pField latitude and longitude fields
##'
##' Get latitude and longitude field vectors from the latitude and longitude
##' attributes of a \code{"pField"} object. These fields follow the same spatial
##' pattern as the input object (i.e., for the first latitude the indices run
##' along all longitudes, then jump to the next latitude, and so further).
##' @param data a \code{"pField"} object for which to return the lat/lon
##' fields.
##' @param simplify if \code{FALSE} (the default), the fields are returned as a
##' two component list, otherwise as a 2 x n matrix, where the first row is the
##' latitude and the second row the longitude field.
##' @return a two component list or 2 x n matrix.
##' @source Function copied from "basis.R" in paleolibary/src/.
##' @author Thomas Laepple
##' @export
GetLatLonField <- function(data, simplify = FALSE) {

    lat <- GetLat(data)
    lon <- GetLon(data)

    nlat <- length(lat)
    nlon <- length(lon)

    lon2d <- rep(lon, nlat)
    lat2d <- rep(lat, each = nlon)

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
    distance.field <- GetDistance(lat0 = point[1],
                                  lon0 = point[2],
                                  lat = field.coord[1, ],
                                  lon = field.coord[2, ])

    return(distance.field)
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
    cat(paste0("tmin = ", min(time(object)), ", tmax = ", max(time(object)),
               ", N = ", length(time(object)), "\n"))
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
    cat(paste0("tmin = ", min(time(object)), ", tmax = ", max(time(object)),
               ", N = ", length(time(object)), "\n"))
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
##' @param ... further arguments passed on to \code{\link{str}}.
##' @seealso \code{\link{str}}
##' @author Thomas Münch
##' @export
str.pField <- function(object, ...) {

    class(object) <- attr(object, "oclass")
    str(object, ...)

}

##' Compactly display the structure of a pTs object
##'
##' Method function to compactly display the internal structure of a
##' \code{"pTs"} object, a diagnostic function and an alternative to
##' \code{summary}; performed with the \code{str} method generic to the
##' \code{"oclass"} of \code{object}.
##' @param object an object of class \code{"pTs"}.
##' @param ... further arguments passed on to \code{\link{str}}.
##' @seealso \code{\link{str}}
##' @author Thomas Münch
##' @export
str.pTs <- function(object, ...) {

    class(object) <- attr(object, "oclass")
    str(object, ...)

}

##' pField object
##'
##' Test for pField object
##' @param object object to be tested.
##' @return \code{TRUE} or \code{FALSE} depending on whether \code{object} is a
##' pField or not.
##' @author Thomas Laepple
is.pField <- function(object) {

    sum(class(object) == "pField") > 0
}

##' pTs object
##'
##' Test for pTs object
##' @param object object to be tested.
##' @return \code{TRUE} or \code{FALSE} depending on whether \code{object} is a
##' pTs or not.
##' @author Thomas Laepple
is.pTs <- function(object) {

    sum(class(object) == "pTs") > 0
}


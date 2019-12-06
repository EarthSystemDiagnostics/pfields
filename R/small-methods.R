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


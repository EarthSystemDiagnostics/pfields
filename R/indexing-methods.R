##' Extract or replace parts of a pField object
##'
##' Method for extracting or replacing a subset of a \code{"pField"} object.
##'
##' For subsetting, the class of the returned object depends on the specified
##' indices:
##' \describe{
##' \item{\code{x[p1, ]}}{a \code{"pField"} object.}
##' \item{\code{x[, p2]}}{a \code{"pTs"} object if \code{p2} is of length 1
##' (single site) or if the subset is not compatible with a \code{"pField"}
##' object (i.e. the subset sites cover more than one latitude, but, for each,
##' do not cover the full range of longitudes).}
##' \item{\code{x[p1, p2]}}{same as before for the time subset \code{p1}.}
##' \item{\code{x[p1]}}{a numeric vector of length \code{length(p1)} where the
##' time and lat-lon information are lost with a warning.}
##' \item{\code{x[]} or \code{x[,]}}{the original object \code{x}.}
##' }
##' @param x a \code{"pField"} object.
##' @param p1 possible row indices for the subset: \code{x[p1, ]}.
##' @param p2 possible column indices for the subset: \code{x[, p2]}.
##' @param ... further arguments passed on to the generic method \code{"["};
##' currently not needed.
##' @return the original object with the replaced subset \code{x[p1, p2]}, or
##' the subset \code{x[p1, p2]}. For the latter, depending on the provided
##' indices, this can be an object of class \code{"pField"}, an object of class
##' \code{"pTs"}, or simply a numeric vector; see details.
##' @seealso \code{?[.pTs}
##' @source Function adapted from "proxytype.R" in paleolibary/src/.
##' @author Thomas Laepple; adapted by Thomas Münch
##' @export
"[.pField" <- function(x, p1, p2, ...) {
  
  is.p1 <- !missing(p1)
  is.p2 <- !missing(p2)

  if (!is.p1 & !is.p2) return(x)
  
  res <- NextMethod("[")
  atb <- attributes(x)

  if (is.p1) n1 <- length(p1)
  if (is.p2) n2 <- length(p2)

  coord.field <- GetLatLonField(x)

  if (is.p1) {

    if (is.p2) {

      # [a, b] given; give back time series or field
      
      hist <- sprintf("Subset [%s, %s]",
                      deparse(substitute(p1)),
                      deparse(substitute(p2)))

      lat <- coord.field$lat2d[p2]
      lon <- coord.field$lon2d[p2]

      nobs <- prod(length(unique(lat)), length(unique(lon)), nrow(res))

      if (nobs != length(res) | n2 == 1) {

        # subset not pField-compatible or single site; return pTs object
        res <- pTs(res, stats::time(x)[p1], lat, lon,
                   atb$name, atb$history, date = FALSE)

      } else {

        # subset can be shaped into pField object
        res <- pField(t(res), stats::time(x)[p1],
                      lat = unique(lat), lon = unique(lon),
                      name = atb$name, history = atb$history,
                      date = FALSE)
      }

    } else {

      # check if [a] oder [a, ] is requested
      
      if (is.array(res) | (length(res) == ncol(x) & n1 == 1)) {

        # [a, ] requested; give back full field at times a

        hist <- sprintf("Subset [%s, ]", deparse(substitute(p1)))
        if (n1 > 1) res <- t(res)

        res <- pField(res, stats::time(x)[p1], atb$lat, atb$lon,
                      atb$name, atb$history, date = FALSE)

      } else {

        # [a] requested; directly give back value(s)

        warning(paste("All time and lat-lon information",
                      "lost in this subset method."),
                call. = FALSE)
        
        hist <- sprintf("Subset [%s]", deparse(substitute(p1)))
        attr(res, "history") <- atb$history
        attr(res, "name") <- atb$name
      }
    }
    
  } else {
    
    #[, b] given; return time series at positions b

    hist <- sprintf("Subset [, %s]", deparse(substitute(p2)))

    lat <- coord.field$lat2d[p2]
    lon <- coord.field$lon2d[p2]

    nobs <- prod(length(unique(lat)), length(unique(lon)), nrow(x))
    if (nobs != length(res) | n2 == 1) {

      # subset not pField-compatible or single site; return pTs object
      res <- pTs(res, stats::time(x), lat, lon,
                 atb$name, atb$history, date = FALSE)

    } else {

      # subset can be shaped into pField object
      res <- pField(t(res), stats::time(x), unique(lat), unique(lon),
                    atb$name, atb$history, date = FALSE)
    }
  }

  res <- AddHistory(res, hist)
  
  return(res)
}

##' Extract or replace parts of a pTs object
##'
##' Method for extracting or replacing a subset of a \code{"pTs"} object.
##' @param x a \code{"pTs"} object.
##' @param p1 possible row indices for the subset: \code{x[p1, ]}.
##' @param p2 possible column indices for the subset: \code{x[, p2]}.
##' @param ... further arguments passed on to the generic method \code{"["};
##' currently not needed.
##' @return the original object with the replaced subset \code{x[p1, p2]}, or
##' the subset \code{x[p1, p2]}. For the latter, the return value is an object
##' of class \code{"pTs"}, unless subset is \code{x[p1]} in which case it is a
##' numeric vector of length \code{length(p1)} where the time and lat-lon
##' information are lost with a warning.
##' @seealso \code{?[.pField}
##' @source Function adapted from "proxytype.R" in paleolibary/src/.
##' @author Thomas Laepple; adapted by Thomas Münch
##' @export
"[.pTs" <- function(x, p1, p2, ...) {

  is.p1 <- !missing(p1)
  is.p2 <- !missing(p2)

  if (!is.p1 & !is.p2) return(x)

  res <- NextMethod("[")
  atb <- attributes(x)

  if (is.p2) {

    p2.l <- 1
    p2.n <- 1

    if (length(atb$lat) > 1) p2.l <- p2
    if (length(atb$name) > 1) p2.l <- p2
  }

  if (is.p1) {

    if (is.p2) {

      # [a, b] given; give back time series b at times a

      hist <- sprintf("Subset [%s, %s]",
                      deparse(substitute(p1)),
                      deparse(substitute(p2)))

      res <- pTs(res, stats::time(x)[p1],
                 lat = atb$lat[p2.l], lon = atb$lon[p2.l],
                 name = atb$name[p2.n], atb$history, date = FALSE)

    } else {

      # check if input is univariate pTs

      if (!is.array(x)) {

        hist <- sprintf("Subset [%s]", deparse(substitute(p1)))

        res <- pTs(res, stats::time(x)[p1], lat = atb$lat, lon = atb$lon,
                   name = atb$name, atb$history, date = FALSE)

        # check if [a] oder [a, ] is requested from multivariate pTs
        
      } else if (is.array(res) |
                 (length(res) == ncol(x) & length(p1) == 1)) {

        # [a, ] requested; give back time series at times a

        hist <- sprintf("Subset [%s, ]", deparse(substitute(p1)))

        res <- pTs(res, stats::time(x)[p1], lat = atb$lat, lon = atb$lon,
                   name = atb$name, atb$history, date = FALSE)

      } else {

        # [a] requested; directly give back value(s)

        warning(paste("All time and lat-lon information",
                      "lost in this subset method."),
                call. = FALSE)
        
        hist <- sprintf("Subset [%s]", deparse(substitute(p1)))
        attr(res, "history") <- atb$history
        attr(res, "name") <- atb$name

      }
    }
    
  } else {

    #[, b] given; give back time series b

    hist <- sprintf("Subset [, %s]", deparse(substitute(p2)))

    res <- pTs(res, stats::time(x),
               lat = atb$lat[p2.l], lon = atb$lon[p2.l],
               name = atb$name[p2.n], atb$history, date = FALSE)        

  }

  res <- AddHistory(res, hist)

  return(res)

}

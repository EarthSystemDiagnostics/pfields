##' Correlation of point with field
##'
##' Calculate the correlation between a point time series of class \code{"pTs"}
##' and a field of class \code{"pField"} or \code{"pTs"}, but also supports
##' standard objects.
##' @param point a \code{"pTs"} point object.
##' @param field a \code{"pField"} or \code{"pTs"} field.
##' @param debug \code{point} and \code{field} are brought onto the same time
##' basis. If \code{debug = TRUE}, this new time basis is printed as a message.
##' @param ... further arguments passed on to the base R correlation function
##' \code{\link[stats]{cor}}, such as the \code{use} argument.
##' @return the correlation(s) between \code{point} and \code{field}. The class
##' of the result depends on the class of \code{field}, so a \code{"pField"}
##' or \code{"pTs"} object usually.
##' @source Function based on "mat.pField.R" in paleolibary/src/.
##' @author Thomas Laepple, modified by Thomas Münch
##' @examples
##' point <- rnorm(100)
##' field <- pField(rnorm(100 * 10 * 10), time = 1 : 100,
##'                 lat = 1 : 10, lon = 1 : 10)
##'
##' correlation <- cor.pTs(point, field)
##'
##' pts.field <- field[, 1 : 25]
##' correlation <- cor.pTs(point, pts.field)
##' @export
cor.pTs <- function(point, field, debug = FALSE, ...) {

  if (!is.null(dim(point))) {
    if (ncol(point) > 1) stop("'point' is not a single point time series.")
  }

  # Bring input data on the same time basis
  start <- max(start(point)[1], start(field)[1])
  end <- min(end(point)[1], end(field)[1])

  if (start > end) {
    stop("'point' and 'field' have non-overlapping observation times.")
  }

  if (debug) {
    message(paste("Common time period:", start, end))
  }

  x <- stats::window(point, start, end)
  y <- stats::window(field, start, end)

  if (is.pField(field)) {

    # Correlation with pField
    class(y) <- "matrix"

    res <- stats::cor(x, y, ...)
    attr(res, "history") <- c(
      sprintf("A (%s):", GetName(point)), GetHistory(point),
      sprintf("B (%s):", GetName(field)), GetHistory(field))
    hist <- paste0("cor.pTs(", GetName(point), ", ", GetName(field), ")")

    res <- pField(res, time = max(stats::time(field)),
                  lat = GetLat(field), lon = GetLon(field),
                  name = "correlation", history = hist)

  } else if (is.pTs(field)) {

    # Correlation with pTs
    class(y) <- "matrix"

    res <- stats::cor(x, y, ...)
    attr(res, "history") <- c(
      sprintf("A (%s):", GetName(point)), GetHistory(point),
      sprintf("B (%s):", GetName(field)), GetHistory(field))
    hist <- paste0("cor.pTs(", GetName(point), ", ", GetName(field), ")")

    res <- pTs(res, time = max(stats::time(field)),
               lat = GetLat(field), lon = GetLon(field),
               name = "correlation", history = hist)

  } else {

    # Correlation with other object
    res <- stats::cor(x, y, ...)

    attr(res, "history") <- c(
      sprintf("A (%s):", GetName(point)), GetHistory(point),
      sprintf("B (%s):", deparse(substitute(field))))
    hist <- paste0("cor.pTs(", GetName(point), ", ",
                   deparse(substitute(field)), ")")

    res <- AddHistory(res, hist)
  }

  return(res)
}

##' Decorrelation of field
##'
##' Calculate the decorrelation length of a \code{"pField"} data set for given
##' target locations.
##'
##' The decorrelation length estimation is based on fitting an exponential of
##' the form \code{exp(-x/tau)}, where \code{x} is distance and \code{tau} the
##' fit parameter, i.e. the decorrelation length, to the distance-correlation
##' relationship as obtained from correlating the time series at the target
##' location with the time series from all other grid points of the input
##' \code{"pField"}. The fit is performed using the built-in R function
##' \code{\link[stats]{nlm}}.
##' @param field a \code{"pField"} object.
##' @param lat1 numeric vector of length 1 with the latitude of the target
##' location for which the decorrelation length is to be calculated. Omit to
##' analyse the entire field.
##' @param lon1 numeric vector of length 1 with the longitude of the target
##' location for which the decorrelation length is to be calculated. Omit to
##' analyse the entire field.
##' @param lat2 optional numeric vector of length 1 with a second latitude to
##' define a coordinate region for which decorrelation lengths are to be
##' correlated. OBS! Method to select a region from the \code{"pField"} object
##' is not yet implemented!
##' @param lon2 Same as \code{lat2} for a second longitude value.
##' @param return.scatter logical; if \code{TRUE}, a data frame with distance-
##' correlation pairs is returned for each target location additionally to the
##' decorrelation length. Defaults to \code{FALSE}.
##' @param verbose logical; if \code{TRUE}, informative messages are printed on
##' the target location selection and the decorrelation length
##' estimation. Defaults to \code{FALSE}.
##' @param ... further arguments passed on to the correlation function
##' \code{\link{cor.pTs}}.
##' @param print.progress numeric value to specify an index interval at which
##' messages on the computation progress are printed (useful for large analysed
##' fields). Ignored for \code{NULL} (the default).
##' @return Decorrelation lengths are returned in units of km. The return
##' structure depends on the supplied input:
##' \itemize{
##' \item For a single target point (lat1-lon1 pair specified), the returned
##' decorrelation length is a numeric length-1 vector.
##' \item For analysing a region (via specifying lat2 and lon2) or the entire
##' field (no lat-lon's specified), the returned decorrelation length is an
##' object of class \code{"pField"} of the same coordinate structure as the
##' analysed region/field.
##' \item For \code{return.scatter = TRUE}, the return object is a 2-element
##' list where the first list element is the decorrelation length result as
##' defined above, and the second list element is a list of data frames for each
##' analysed location where each data frame has elements \code{d} and \code{cor}
##' with all distance-correlation value pairs for this location.
##' }
##' @seealso \code{\link{cor.pTs}}
##' @author Thomas Münch
##' @export
Decor.pField <- function(field, lat1, lon1, lat2, lon2,
                         return.scatter = FALSE,
                         verbose = FALSE, print.progress = NULL, ...) {

  # Aux function
  LastNonNA <- function(x) {
    i <- which(!is.na(x))
    ifelse(length(i) == 0, NA, x[max(which(!is.na(x)))])
  }

  # Input error checking and selection of target location
  
  args <- c(missing(lat1), missing(lon1))

  if (sum(args) == 1) {
    
    stop("Specify both 'lat1' and 'lon1' to select a target point
              or region, or none to analyse the entire field.")
    
  } else if (sum(args) == 0) {

    args2 <- c(missing(lat2), missing(lon2))

    if (sum(args2) == 2) {

      if (verbose) message("Selecting target point...")
      sfield <- SelPoint(field, lat1, lon1,
                         simplify = FALSE, verbose = verbose)

    } else if (sum(args2) == 1) {

      stop("Specify both 'lat2' and 'lon2'
                  if you want to select a target region.")

    } else {

      stop("Method to select region not yet implemented!")
      #sfield <- selspace(field, lat1 = lat1, lat2 = lat2,
      #                   lon1 = lon1, lon2 = lon2)

    }

  } else {

    if (verbose) message("Using entire field...")
    sfield <- field
  }

  # Coordinate fields of input and selected target in pField structure
  
  latlon.field <-  GetLatLonField(field, simplify = TRUE)
  latlon.sfield <- GetLatLonField(sfield, simplify = TRUE)

  # Calculate decorrelation lengths

  tau <- rep(NA, ncol(sfield))
  if (return.scatter) scatter <- list()

  tau.start <- 1000

  for (i in 1 : ncol(sfield)) {

    if (!is.null(print.progress)) {
      if ((i %% print.progress) == 0) {
        message(sprintf("Analysing site no %i of %i.", i, ncol(sfield)))
      }
    }

    # calculate distance field for site i
    lat <- latlon.sfield[1, i]
    lon <- latlon.sfield[2, i]
    d <- GetDistanceField(field, latlon.field = latlon.field,
                          lat = lat, lon = lon, verbose = verbose)

    # calculate correlation field for site i
    corr <- cor.pTs(sfield[, i], field, ...)

    if (all(is.na(corr))) {

      # no non-missing correlations = no estimate possible
      if (verbose) {
        message(sprintf("Site %i: All field correlations are NA.", i))
      }

    } else {

      # fit exponential decay model; catch errors

      # start parameter = last found value, or 1000 km
      tau.start <- LastNonNA(tau[1 : i])
      if (is.na(tau.start)) tau.start <- 1000

      tau[i] <- tryCatch(
      {
        # decorrelation length when no error...

        mod <- stats::nls(y ~ exp(-x / tau),
                          data = data.frame(x = d, y = c(corr)),
                          start = list(tau = tau.start),
                          control = stats::nls.control(maxiter = 100))

        mod.sum <- summary(mod)
        if (verbose) print(mod.sum)

        # ... but only when successful convergence and p < 0.1

        if (mod$convInfo$isConv & mod.sum$parameters[1, 4] <= 0.1) {

          dcr.len <- mod.sum$parameters[1, 1]

        } else {

          dcr.len <- NA
        }

        dcr.len
      },
      error = function(cond) {

        message(conditionMessage(cond))
        return(NA)
      })
    }

    # scatter of distances versus correlations
    if (return.scatter) {
      scatter[[i]] <- as.data.frame(list(d = d, cor = c(corr)))
    }

  }

  # Arrange output

  if (ncol(sfield) == 1) {

    decor.length <- tau
    if (return.scatter) scatter <- scatter[[1]]

  } else {

    decor.length <- pField(tau, time = max(stats::time(field)),
                           lat = GetLat(sfield), lon = GetLon(sfield),
                           name = "decorrelation",
                           history = GetHistory(field), date = FALSE)
    decor.length <- AddHistory(decor.length,
                               paste0("Decor.pField(", GetName(field), ")"))
  }

  res <- decor.length
  if (return.scatter) {
    res <- list(decor.length = decor.length, scatter = scatter)
  }

  return(res)

}


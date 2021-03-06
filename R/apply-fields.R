##' Apply a function across space
##'
##' Apply a function across the spatial dimension of a \code{"pField"} or
##' \code{"pTs"} object by using \code{apply} on the rows of the object. The
##' result of this gives a \code{"pTs"} vector object, i.e. a single time
##' series.
##'
##' Input columns which only contain NA values are stripped befor applying the
##' function. If only one non-NA column is present, this column is returned
##' unchanged with a warning.
##' @param data a \code{"pField"} or \code{"pTs"} object.
##' @param FUN the function to be applied.
##' @param ... further arguments passed on to \code{FUN}.
##' @return a single \code{"pTs"} time series with the results of the function
##'   applied.
##' @source Function copied from "basis.R" in paleolibary/src/.
##' @author Thomas Laepple
##' @examples
##' x <- pField(data = array(rnorm(10 * 10 * 100), dim = c(10, 10, 100)),
##'             time = 1 : 100, lat = 1 : 10, lon = 1 : 10)
##' ApplySpace(x, sd)
##'
##' x <- x[, 1 : 15]
##' ApplySpace(x, sd)
##' @aliases applyspace ApplySpace
##' @export applyspace ApplySpace
ApplySpace <- function(data, FUN, ...) {

  # Input error checking

  if (!is.pField(data) & !is.pTs(data)) {
    stop("Input is whether pField nor pTs object.")
  }

  if (is.null(dim(data))) {
    stop("Input is not a matrix.")
  }

  if (ncol(data) == 1) {
    stop("Cannot apply across space: input has only one spatial dimension.")
  }

  # Checking number of non-NA columns

  index <- which(!is.na(colSums(data)))

  if (length(index) == 0) {
    stop("No non-NA columns in input to apply FUN on.")
  }

  if (length(index) == 1) {
    warning("Only 1 non-NA column in input; ",
            "returning original data from there.")
    return(data[, index])
  }

  # Parse history attribute

  arg <- deparse(match.call()[-c(1, 2)])
  nam <- trimws(gsub("\\s+", " ", paste(arg, collapse = "")))
  hist <- sprintf("ApplySpace: %s", nam)

  # Apply across space

  ts <- apply(data[, index], 1, FUN, ...)
  attr(ts, "history") <- GetHistory(data)
  res <- pTs(data = ts, time = stats::time(data),
             name = GetName(data), history = hist)

  return(res)
}
applyspace <- function(...) {
  warning("applyspace is deprecated and replaced with ApplySpace
            to comply with ECUS R style guide.")
  ApplySpace(...)
}

##' Apply a function across time
##'
##' Apply a function across the temporal dimension of a \code{"pField"} or
##' \code{"pTs"} object by using \code{apply} on the columns of the
##' object. Depending on the output of the applied function, this gives a
##' \code{"pField"} or \code{"pTs"} object with only one, or with several time
##' steps.
##' @param data a \code{"pField"} or \code{"pTs"} object.
##' @param FUN the function to be applied.
##' @param newtime the observation time point(s) of the result. For \code{NULL}
##' (the default), the average of the time points in \code{data} is used,
##' if the applied function yields an aggregated result (i.e. one time step),
##' and the original time points in \code{data} if the result has as many time
##' steps as the input data. For other cases, you need to provide the
##' respective new time axis here.
##' @param ... further arguments passed on to \code{FUN}.
##' @return a \code{"pField"} or \code{"pTs"} object with the results of the
##' function applied.
##' @source Function copied from "basis.R" in paleolibary/src/.
##' @author Thomas Laepple
##' @examples
##' x <- pField(data = array(rnorm(10 * 10 * 100), dim = c(10, 10, 100)),
##'             time = 1 : 100, lat = 1 : 10, lon = 1 : 10)
##' y <- ApplyTime(x, mean)
##'
##' # subset incompletely to create a pTs object
##' x.pts <- x[, 1 : 15]
##' y.pts <- ApplyTime(x.pts, mean) # is the same as y[, 1 : 15]
##'
##' # you need to supply a new time axis e.g. for use cases such as
##' y <- ApplyTime(x, range, newtime = c(1, 2))
##' @aliases applytime ApplyTime
##' @export applytime ApplyTime
ApplyTime <- function(data, FUN, newtime = NULL, ...) {

  # Input error checking

  if (!is.pField(data) & !is.pTs(data)) {
    stop("Input is whether pField nor pTs object.")
  }

  if (is.null(dim(data))) {
    stop("Input is not a matrix.")
  }

  if (nrow(data) == 1) {
    stop("Cannot apply across time: input has only one temporal dimension.")
  }

  # Parse history attribute

  if (is.null(newtime)) {argpos <- 1 : 2} else {argpos <- c(1, 2, 4)}
  arg <- deparse(match.call()[-argpos])
  nam <- trimws(gsub("\\s+", " ", paste(arg, collapse = "")))
  hist <- sprintf("ApplyTime: %s", nam)

  # Apply across time

  field <- apply(data, 2, FUN, ...)
  attr(field, "history") <- GetHistory(data)

  # Set output time step

  nt <- ifelse(is.null(dim(field)), 1, nrow(field))

  if (is.null(newtime)) {

    if (nt == 1) {
      newtime <- mean(stats::time(data))
    } else if (nt == nrow(data)) {
      newtime <- stats::time(data)
    } else {
      stop("Need to provide a new time axis for result of apply operation.",
           call. = FALSE)
    }

  } else {

    if (length(newtime) != nt) {
      stop("Provided time axis does not match result of apply operation.",
           call. = FALSE)
    }
  }

  # Shape into field

  if (is.pField(data)) {

    res <- pField(data = t(field), time = newtime,
                  lat = GetLat(data), lon = GetLon(data),
                  name = GetName(data), history = hist)
  } else {

    if (length(newtime) == 1) {
      dim(field) <- c(1, ncol(data))
    }

    res <- pTs(data = field, time = newtime,
               lat = GetLat(data), lon = GetLon(data),
               name = GetName(data), history = hist)
  }
  
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

  # Validate input
  if (!is.pField(fld1) | !is.pField(fld2)) {
    stop("Input fields must be pField objects.")
  }

  # Check dimensions of input objects

  if (sum(dim(fld1)) != sum(dim(fld2))) {
    stop("Supplied fields have different dimensions.")
  }

  # Check fields for same time and Lat-Lon basis

  if (max(abs(c(stats::time(fld1)) - c(stats::time(fld2)))) != 0) {
    stop("Supplied fields have different observation times.")
  }
  if (max(abs(GetLat(fld1) - GetLat(fld2))) != 0) {
    stop("Supplied fields have different latitudes.")
  }
  if (max(abs(GetLon(fld1) - GetLon(fld2))) != 0) {
    stop("Supplied fields have different longitudes.")
  }

  if (is.null(newtime)) {
    newtime <- mean(stats::time(fld1))
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

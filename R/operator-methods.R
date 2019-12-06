##' pField operator method
##'
##' Group generic method for the \code{Ops} group of functions to be applied on
##' \code{"pField"} objects.
##' @param e1 an object of class \code{"pField"}.
##' @param e2 an object, optionally of class \code{"pField"} (or missing for
##' unary operator).
##' @return an object of class \code{"pField"} with the result of the operator
##' applied on \code{e1} and \code{e2}.
##' @source Function adapted from "proxytype.R" in paleolibary/src/
##' @seealso \code{?Ops}
##' @author Thomas Laepple; adapated by Thomas Münch
##' @export
Ops.pField <- function (e1, e2) {

  SLIMIT <- 1

  nField <- 0
  if (is.pField(e1)) {

    nField <- nField + 1
    nam1 <- attr(e1, "name")

  } else {

    nam1 <- e1[1]
  }

  if (is.pField(e2)) {

    nField <- nField + 1
    nam2 <- attr(e2, "name")

  } else {

    nam2 <- e2[1]
  }

  newname <- paste(nam1, .Generic, nam2)

  if (nField == 2) {

    atb1  <- attributes(e1)
    atb2  <- attributes(e2)
    tim1 <- time(e1)
    tim2 <- time(e2)

    if ((length(atb1$lat) != length(atb2$lat)) |
        (length(atb1$lon) != length(atb2$lon))) {
      stop("Lat-lon grid dimensions do not match.")
    }

    if ((sum(abs(atb1$lat - atb2$lat)) +
         sum(abs(atb1$lon - atb2$lon))) > SLIMIT) {
      stop("Operator applied on incompatible lat-lon grids.")
    }

    st <- max(c(attr(tim1, "tsp")[1], attr(tim2, "tsp")[1]))
    en <- min(c(attr(tim1, "tsp")[2], attr(tim2, "tsp")[2]))
    if (st > en) {
      stop("Operator applied on non-intersecting time series.")
    }

    if (length(tim1) != length(tim2)) {
      warning(paste("Different observation time lengths:",
                    "Only time range of shorter series used."))
    }

    if (attr(tim1, "tsp")[3] != attr(tim2, "tsp")[3]) {
      warning(paste("Operator applied on two time bases!",
                    "Result time base set to first time base."))
    }

    e1 <- ts(e1, start = attr(tim1, "tsp")[1],
             frequency = attr(tim1, "tsp")[3])
    e2 <- ts(e2, start = attr(tim2, "tsp")[1],
             frequency = attr(tim1, "tsp")[3])

    e <- NextMethod()

    attr(e, "history") <- c(
      sprintf("A (%s):", nam1), atb1$history,
      sprintf("B (%s):", nam2), atb2$history)

    res <- pField(t(e), time = time(e),
                  lat = atb1$lat, lon = atb1$lon,
                  name = newname, history = newname)

  } else {

    e <- NextMethod(.Generic)
    if (!is.null(attr(e, "nclass"))) class(e) <- attr(e, "nclass")
    res <- AddHistory(e, newname)
  }

  return(res)

}

##' pTs operator method
##'
##' Group generic method for the \code{Ops} group of functions to be applied on
##' \code{"pTs"} objects.
##' @param e1 an object of class \code{"pTs"}.
##' @param e2 an object, optionally of class \code{"pTs"} (or missing for
##' unary operator).
##' @return an object of class \code{"pTs"} with the result of the operator
##' applied on \code{e1} and \code{e2}.
##' @source Function adapted from "proxytype.R" in paleolibary/src/
##' @seealso \code{?Ops}
##' @author Thomas Laepple; adapated by Thomas Münch
##' @export
Ops.pTs <- function (e1, e2) {

  SLIMIT <- 1

  nField <- 0
  if (is.pTs(e1)) {

    nField <- nField + 1
    nam1 <- attr(e1, "name")

  } else {

    nam1 <- e1[1]
  }

  if (is.pTs(e2)) {

    nField <- nField + 1
    nam2 <- attr(e2, "name")

  } else {

    nam2 <- e2[1]
  }

  newname <- paste(nam1, .Generic, nam2)

  if (nField == 2) {

    atb1 <- attributes(e1)
    atb2 <- attributes(e2)
    tim1 <- time(e1)
    tim2 <- time(e2)

    if ((length(atb1$lat) != length(atb2$lat)) |
        (length(atb1$lon) != length(atb2$lon))) {
      stop("Lat-lon grid dimensions do not match.")
    }

    if ((sum(abs(atb1$lat - atb2$lat)) +
         sum(abs(atb1$lon - atb2$lon))) > SLIMIT) {
      stop("Operator applied on incompatible lat-lon grids.")
    }

    st <- max(c(attr(tim1, "tsp")[1], attr(tim2, "tsp")[1]))
    en <- min(c(attr(tim1, "tsp")[2], attr(tim2, "tsp")[2]))
    if (st > en) {
      stop("Operator applied on non-intersecting time series.")
    }

    if (length(tim1) != length(tim2)) {
      warning(paste("Different observation time lengths:",
                    "Only time range of shorter series used."))
    }

    if (attr(tim1, "tsp")[3] != attr(tim2, "tsp")[3]) {
      warning(paste("Operator applied on two time bases!",
                    "Result time base set to first time base."))
    }

    e1 <- ts(e1, start = attr(tim1, "tsp")[1],
             frequency = attr(tim1, "tsp")[3])
    e2 <- ts(e2, start = attr(tim2, "tsp")[1],
             frequency = attr(tim1, "tsp")[3])

    e <- NextMethod()

    attr(e, "history") <- c(
      sprintf("A (%s):", nam1), atb1$history,
      sprintf("B (%s):", nam2), atb2$history)

    if (!all(atb1$lat == atb2$lat)) lat <- NA else lat = atb1$lat
    if (!all(atb1$lon == atb2$lon)) lon <- NA else lon = atb1$lon

    res <- pTs(e, time = time(e), lat = lat, lon = lon,
               name = newname, history = newname)

  } else {

    e <- NextMethod(.Generic)
    if (!is.null(attr(e, "nclass"))) class(e) <- attr(e, "nclass")
    res <- AddHistory(e, newname)
  }

  return(res)

}


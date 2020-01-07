#' SelSpace3D
#' @param data pField object
#' @param lat1 vector length 1
#' @param lon1 vector legnth 1
#' @param SBOX vector length 1, defaults to 5
#' @param tolLon vector length 1, defaults to 10
#' @param bNN logical, defaults to FALSE
#' @param timeindexNA at which timestep should the Next Neighbour algorithm
#'   search for missing values
#'
#' @description Function to interpolate a field to a given point specified by
#'   latitude and longitude. It uses the nearest neighbour if the adjancents
#'   points are missing, if not it uses bilinear interpolation
#'
#' @return \code{\link{pTs}} object
#' @export SelSpace3D selspace.3D
#' @aliases SelSpace3D selspace.3D
#' @author Thomas Laepple
SelSpace3D <- function(data, lat1, lon1, SBOX = 5, tolLon = 10, 
                       bNN = FALSE, timeindexNA = 1) {
  warning("'SelSpace3D()' is experimental and in development; use with care.")
  choice.lat <- lat1
  choice.lon <- lon1
  temp <- attributes(data)
  
  if (prod(dim(data)) != length(temp$lon) * length(temp$lat) * 
    length(stats::time(data))) 
    stop("N(data) != N(lat)*N(lon)*N(time)")
  
  # make a 2D array [lon,lat] containing the orginal indices
  pointer3d.raw <- array(1:dim(data)[2], c(1, length(temp$lon), 
    length(temp$lat)))
  
  # arrange to get a continous field
  nlon <- length(temp$lon)
  nTime <- length(stats::time(data))
  d <- diff(temp$lon)
  
  if (max(d) > (min(d) + 0.01)) {
    edgelon <- which(d == max(d))
    pointer3d <- array(NA, c(1, length(temp$lon), length(temp$lat)))
    pointer3d[, (edgelon + 1):nlon, ] <- pointer3d.raw[, 
      (edgelon + 1):nlon, ]
    pointer3d[, 1:edgelon, ] <- data3d.raw[, 1:edgelon, ]
    temp$lon <- c(temp$lon[(edgelon + 1):nlon], temp$lon[1:edgelon] + 
      360)
  } else pointer3d <- pointer3d.raw
  
  # longitude jump by wrapping
  wrap.dLon <- utils::head(temp$lon, 1) - (utils::tail(temp$lon, 1) - 360)
  
  if ((wrap.dLon > 0) & (wrap.dLon < tolLon)) {
    ### Check if we the data is global on the longitudes, than Copy
    ### the data 3 times.... to avoid breaks in the longitude
    pointer3d.3c <- array(NA, c(1, 3 * length(temp$lon), 
      length(temp$lat)))
    pointer3d.3c[, 1:nlon, ] <- pointer3d[, 1:nlon, ]
    pointer3d.3c[, (nlon + 1):(2 * nlon), ] <- pointer3d[, 
      1:nlon, ]
    pointer3d.3c[, (2 * nlon + 1):(3 * nlon), ] <- pointer3d[, 
      1:nlon, ]
    
    lon.3c <- c(temp$lon[1:nlon] - 360, temp$lon[1:nlon], 
      temp$lon[1:nlon] + 360)
  } else {
    # Do wrap the longitudes at the longitudes could not be
    # connected
    lon.3c <- temp$lon
    pointer3d.3c <- pointer3d
  }
  
  if (temp$lat[2] < temp$lat[1]) 
    # if the latitudes are from + to -, reverse them
  {
    temp$lat <- rev(temp$lat)
    pointer3d.3c <- pointer3d.3c[, , rev(seq(temp$lat))]
  }
  
  
  # attention... midpoints are given...
  if ((lat1 > utils::tail(temp$lat, 1)) | (lat1 < utils::head(temp$lat, 1))) {
    warning("Latitude outside field")
    return(NULL)
  }
  if ((lon1 > utils::tail(lon.3c, 1)) | (lon1 < utils::head(lon.3c, 1))) {
    warning("Longitude outside field")
    return(NULL)
  }
  
  indexLat <- rev(which(temp$lat <= lat1))[1]:which(temp$lat >= 
    lat1)[1]
  indexLon <- rev(which(lon.3c <= lon1))[1]:which(lon.3c >= 
    lon1)[1]
  
  # First check if we have any missing data
  pointer.select <- pointer3d.3c[, indexLon, indexLat]
  
  
  if (is.null(dim(pointer.select))) {
    dim(pointer.select) <- c(1, length(pointer.select))
  } else {
    dim(pointer.select) <- c(1, dim(pointer.select))
    }
     
  
  # data.select<-(as.matrix(data)[,pointer.select]) if
  # (is.null(dim(data.select)) nData<-sum(!is.na(data.select))
  # else nData<-colSums(!is.na(data.select))
  
  # if (min(colSums(!is.na(data)[,pointer.select]==0) {
  # warning('One neighbour point only contains missing values;
  # therefore Next Neighbour is used') bNN=TRUE }
  
  if (bNN) {
    # Create an area around the point
    tempindexLat <- ((indexLat[1] - SBOX):(indexLat[1] + 
      SBOX))
    tempindexLon <- ((indexLon[1] - SBOX):(indexLon[1] + 
      SBOX))
    
    # remove areas outside the boundaries
    tempindexLat <- tempindexLat[tempindexLat > 0]
    tempindexLon <- tempindexLon[tempindexLon > 0]
    tempindexLat <- tempindexLat[tempindexLat <= length(temp$lat)]
    tempindexLon <- tempindexLon[tempindexLon <= length(lon.3c)]
    
    # Get the nearest neighbours
    res <- expand.grid(tempindexLon, tempindexLat)
    
    # only retain the nearest nonmissing neighbours
    IndexRegion <- diag(pointer3d.3c[1, res[, 1], res[, 2]])
    indexV <- !is.na(data[timeindexNA, IndexRegion])
    
    if (sum(indexV) == 0) 
      return(NA)
    x <- res[indexV, 1]
    y <- res[indexV, 2]
    
    # Distances to the points in the area
    D2i <- (lon.3c[x] - lon1)^2 + (temp$lat[y] - lat1)^2
    
    # nearest neighbour is the one with the smallest distance
    neighbour <- order(D2i)[1]
    
    # Switch to real data
    intpoldata <- data[, pointer3d.3c[, x[neighbour], y[neighbour]]]
    choice.lat <- temp$lat[y[neighbour]]
    choice.lon <- lon.3c[x[neighbour]]
  } else {
    # here the interpolation starts
    
    if (length(indexLat) == 1) 
      ey = NA else ey <- (lat1 - temp$lat[indexLat[1]])/(temp$lat[indexLat[2]] - 
      temp$lat[indexLat[1]])
    if (length(indexLon) == 1) 
      ex = NA else ex <- (lon1 - lon.3c[indexLon[1]])/(lon.3c[indexLon[2]] - 
      lon.3c[indexLon[1]])
    
    
    if ((!is.finite(ex)) & (!is.finite(ey))) 
      intpoldata <- data[, pointer.select] else # we are on the point, no interpolation
    if (!is.finite(ex)) {
      intpoldata <- data[, pointer.select[, 1]] + (data[, 
        pointer.select[, 2]] - data[, pointer.select[, 
        1]]) * ey
      # only latitudonal interpolation
    } else {
      if (!is.finite(ey)) {
        intpoldata <- data[, pointer.select[, 1]] + (data[, 
          pointer.select[, 2]] - data[, pointer.select[, 
          1]]) * ex
        # only longitudonal interpolation #Switch to real data
      } else intpoldata <- (1 - ex) * (1 - ey) * data[, pointer.select[, 
        1, 1]] + (1 - ex) * (ey) * data[, pointer.select[, 
        1, 2]] + (ex) * (1 - ey) * data[, pointer.select[, 
        2, 1]] + (ex * ey) * data[, pointer.select[, 
        2, 2]]
    }
  }
  # create time series
  result <- pTs(intpoldata, stats::time(data), choice.lat, choice.lon, 
    GetName(data), GetHistory(data), date = FALSE)
  
  hist <- paste("selspace: lat=", lat1, " lon=", lon1, sep = "")
  
  return(AddHistory(result, hist))
}

selspace.3D <- function(...) {
  warning("selspace.3D is deprecated and replaced with SelSpace3D
    to comply with ECUS R style guide")
  SelSpace3D(...)
}

##' Select nearest point
##'
##' Select the timeseries from a \code{"pField"} object at the grid point
##' nearest to a given pair of latitude and longitude values.
##'
##' This is a function in development; the current method to find the nearest
##' grid point is to minimise the root mean square deviation between the
##' position vectors in spherical coordinates of the requested point and all
##' observation points in the \code{"pField"} object, which is done by calling
##' \code{\link{MinimizeSpherical}}. In the future, a different method may be
##' implemented for this. Note that requested lat/lon values outside the range
##' of grid points in \code{data} will be processed with a warning.
##' @param data a \code{"pField"} object.
##' @param lat the latitude of the requested point.
##' @param lon the longitude of the requested point.
##' @param simplify logical; if \code{TRUE} (the default) the result will be
##' simplified to a \code{"pTs"} object, otherwise it will be a \code{"pField"}
##' object with only one spatial dimension.
##' @param verbose if \code{TRUE}, print a message with the requested and
##' nearest found coordinates. Defaults to \code{FALSE}.
##' @return a \code{"pTs"} or \code{"pField"} (if \code{simplify = FALSE})
##' object with the time series at the grid point nearest to the requested
##' point.
##' @author Thomas Münch
##' @examples
##' # Create some empty pField object
##' lat <- seq(-75, -80, -5)
##' lon <- c(0, 135, 215)
##' time <- 1 : 4
##'
##' x <- pField(lat = lat, lon = lon, time = time)
##'
##' # Extract grid point nearest to lat = -80, lon = 200:
##' SelPoint(x, lat = -80, lon = 200, verbose = TRUE)
##' SelPoint(x, lat = -80, lon = 200, simplify = FALSE, verbose = TRUE)
##' @export
SelPoint <- function(data, lat, lon, simplify = TRUE, verbose = FALSE) {

    # 2D fields of data latitudes and longitudes
    field.coord <- GetLatLonField(data, simplify = TRUE)

    # Warn if target latitude or longitude is outside coordinate range in data

    if (lat < min(field.coord[1, ]) | lat > max(field.coord[1, ])) {
        warning("Requested latitude is outside range of data.")
    }
    if (lon < min(field.coord[2, ]) | lon > max(field.coord[2, ])) {
        warning("Requested longitude is outside range of data.")
    }

    # Select data at the point of minimum deviation from requested target

    i <- MinimizeSpherical(lat0 = lat, lon0 = lon,
                           lat = field.coord[1, ],
                           lon = field.coord[2, ])

    lat.nn <- unname(field.coord[1, i])
    lon.nn <- unname(field.coord[2, i])

    if (verbose) {
        message(
            sprintf("requested: lat = %f, lon = %f;\nfound: lat = %f, lon = %f",
                    lat, lon, lat.nn, lon.nn))
    }

    if (simplify) {
        
        res <- pTs(data = data[, i], time = stats::time(data),
                   lat = lat.nn, lon = lon.nn,
                   name = GetName(data), history = GetHistory(data),
                   date = FALSE)
    } else {
        
        res <- pField(data = data[, i], time = stats::time(data),
                      lat = lat.nn, lon = lon.nn,
                      name = GetName(data), history = GetHistory(data),
                      date = FALSE)
    }

    res <- AddHistory(res, paste0("SelPoint: lat = ", lat,", lon = ", lon))

    return(res)

}

##' Minimize difference between coordinates
##'
##' This function finds the position in a latitude/longitude field that is
##' closest to a given target position by minimizing the root mean square
##' deviation of their position vectors in spherical coordinates.
##' @param lat0 a length-1 vector with the latitude [degree] of the target
##' position.
##' @param lon0 a length-1 vector with the longitude [degree] of the target
##' position.
##' @param lat numeric vector of latitudes [degree] from a field of coordinates
##' for which the point closest to the target is to be determined.
##' @param lon numeric vector of longitudes [degree] from a field of coordinates
##' for which the point closest to the target is to be determined. Must have the
##' same length as \code{lat}.
##' @param return.coordinates logical; shall the actual coordinates of the
##' position closest to the target be returned? Defaults to \code{FALSE}, which
##' returns just the vector index of the closest position.
##' @return per default, the index position in \code{lat} (or \code{lon}) of the
##' position closest to the target; or the actual coordinates of this position
##' as a length-2 numeric vector (for \code{return.coordinates = TRUE}).
##' @author Thomas Münch
##' @examples
##' # some coordinates
##' lat0 <- -75
##' lon0 <- 0
##' lat <- seq(-74, -80, -2)
##' lon <- seq(-2, 7, 3)
##'
##' # get closest point to lat0/lon0 in degree
##' MinimizeSpherical(lat0, lon0, lat, lon,
##'                   return.coordinates = TRUE)
##' @export
MinimizeSpherical <- function(lat0, lon0, lat, lon,
                              return.coordinates = FALSE) {

  # Error checking

  if (length(lat0) != length(lon0))
    stop("'lat0' and 'lon0' must both have length 1.")

  if (length(lat) != length(lon))
    stop("'lat' and 'lon' must have equal length.")

  n <- length(lat)

  # Convert angles from degree to radian

  x <- deg2rad(lat0)
  y <- deg2rad(lon0)

  xx <- deg2rad(lat)
  yy <- deg2rad(lon)

  # Convert input to vectors in spherical coordinates

  target <- c(cos(x) * cos(y),
              cos(x) * sin(y),
              sin(x))

  field.vectors <- rbind(cos(xx) * cos(yy),
                         cos(xx) * sin(yy),
                         sin(xx))

  # Root mean square deviation between target and data position vectors

  dev <- rep(NA, n)
  for (i in 1 : n) {

    dev[i] <- rmsd(field.vectors[, i], target)
  }

  # Return index of the position vector with minimum deviation to target

  i <- which.min(dev)

  if (return.coordinates) {
    res <- c(lat = lat[i], lon = lon[i])
  } else {
    res <- i
  }

  return(res)
}


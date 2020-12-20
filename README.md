# pfields: Analyse gridded field data

## Overview
**pfields** is an R package that provides functions to create and analyse objects of class `pField` and `pTs`:

* `pField` and `pTs` class objects are enhanced two-dimensional time-series objects, i.e. matrices where the rows represent a time-series (`ts` class) object (temporal dimension) and the columns a spatial or typological dimension.

More specifically:
* `pField` objects follow the structure of netcdf files, i.e. matrices where the spatial dimension respresents a latitude-longitude grid with indices running subsequently over the available longitudes (towards the East) for each available latitude (North to South). `pField` objects can thus be used to store gridded field data (e.g. from climate model simulations or from observations), either for the entire globe, or for "complete" subsets thereof, where complete is defined in the sense that for each covered latitude, the full range of longitudes is available.
* `pTs` objects are more general in the sense that their second dimension must not follow a certain struture. Thus, they can be used to store incomplete spatial subsets of gridded field data, or data of different types for the same spatial location (e.g. different proxy observations from the same ice or marine sediment core).
* Both class objects exhibit common attributes that store their meta information, i.e. spatial (latitude and longitude) and history (change log) information. These attributes can be accessed by special functions provided by **pfields**.

## Installation
**pfields** can be installed directly from GitHub:
```
# install.packages("remotes")
remotes::install_github("EarthSystemDiagnostics/pfields")
```
## Dependencies
**pfields** depends on the package
[`geostools`](https://github.com/EarthSystemDiagnostics/geostools), which is
installed from GitHub upon installing **pfields**.

## Functionality

* Construct an empty `pField` or `pTs` object or convert exisiting data into such an object with the functions `pField()` and `pTs()`.
* Available (generic) methods for `pField` and `pTs` objects include
  * extracting/replacing subsets of an object;
  * performing operators on an object (generic methods for the `"Ops"` group, such as +, -, ...);
  * `str.*` and `summary.*` methods;
  * checking the class of an object.
* Base R apply-type functions are available to apply a function to the temporal domain of an object, to the spatial domain, or on two fields across space.
* Access the name, history, latitude and longitude attributes of an object.
* Select points from a field and calculate distances from a given point.
* Compute correlations between objects.
* Easily convert from a `pfield` object to a `data.frame`.

## Limitations

* The analysis functions in **pfields** are not optimised for speed. Thus, if you need to loop over many matrix operations performed on a `pField` or `pTs` object and you do not rely on their class functionality, it might be computationally more efficient to return to the original class of the object, i.e., if `x` is the `pField` or `pTs` object, then set `class(x) <- attr(x, "oclass")`.
* Plotting routines for `pField` and `pTs` objects are not yet available.

# pfields 0.3.2

* `ApplyTime()` now correctly handles applied functions which yield more than
  one time step for the result. New behaviour is tested;
* specification of a new fitting time axis for apply results with more than one
  time step is tricky to automate and thus has been coded such that it is to be
  supplied manually; omitting the new time axis now causes an error issueing an
  informative message;
* the name of the function which is used in the apply step is now handled such
  that also on-the-fly function definitions supplied as argument to
  `ApplyTime()` and `ApplySpace()` are supported and produce nice strings for
  the history attribute of the output;
* the history attribute now also nicely includes optional arguments passed to
  the applied function;
* the ".9000" development suffix is omitted from now on.

# pfields 0.3.1.9000

* fixed missing remote package source in DESCRIPTION file.

# pfields 0.3.0.9000

Since package `geostools` is available, the functions `GetDistance()`,
`MinimizeSpherical()`, `rmsd()` and `deg2rad()` no longer need to be a direct
part of `pfields`. Therefore, these functions have been removed and are instead
called from `geostools`, which makes `pfields` now dependent on `geostools`.

# pfields 0.2.1.9000

* the following functions now handle consistently both `pFields` and `pTs`
  objects:
  `GetLatLonField()`, `ApplyTime()`, `ApplySpace()`, `cor.pTs()`, and
  `pField2df()`;
* new tests have been added to check the functionality of `ApplyTime()`,
  `ApplySpace()`, `ApplyFields()`, and `cor.pTs()`.

# pfields 0.1.1.9000

* the subsetting of `pField` and `pTs` objects has been updated to fix an error
  which occurred when either subsetting a single time step but leaving multiple
  columns, or when subsetting multiple columns on an object which already has only
  one time step, and when the subset result is a pTs object;
* test infrastructure has been set up and the updated indexing methods are
  tested.


# pfields 0.0.0.9000

* Development version.

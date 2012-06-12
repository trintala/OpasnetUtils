opbase <- function(x, ...)
UseMethod("opbase")

# Read data from the opasnet
opbase.data <- function(dsn, ident, include = NULL, exclude = NULL, series_id = NULL, iterations = NULL) {
	return(op_baseGetData(dsn, ident, include, exclude, series_id, iterations, TRUE, TRUE))
}

# Write data to the opasnet
opbase.upload <- function(dsn, input, ident = NULL, name = NULL, unit = NULL, objtype_id = NULL, who = NULL, acttype = NULL, rescol = NULL, n.obs.const = FALSE) {
	return(op_baseWrite(dsn, input, ident, name, unit, objtype_id, who, acttype, rescol, n.obs.const, 10000, TRUE, TRUE, TRUE, TRUE))
}

# Locations of an object
opbase.locations <- function(dsn, ident, series_id = NULL) {
	return(op_baseGetLocs(dsn, ident, series_id, use.utf8 = TRUE, apply.utf8 = TRUE))
}
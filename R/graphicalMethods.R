setMethod(
	f = "plot",
	signature = signature(
		x = "size_distribution"
	),
	definition = function(x, ...) {
		plot(x@midpoints, x@sizes, ...)
	}
)

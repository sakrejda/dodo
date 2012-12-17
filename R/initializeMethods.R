setMethod(
	f = "initialize",
	signature = signature(
		.Object = "size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object@sizes = vector(mode="numeric", length=n_bins)
		.Object@n_bins = n_bins
		.Object@limits = limits
		h <- (limits[['max']] - limits[['min']]) / n_bins
		.Object@midpoints = limits[['min']] + ((1:n_bins)-0.5) * h
		estimator <- function(x, seed, bw) {
			1/(length(seed)*bw) * sum(dnorm(x=x, mean=seed, sd=1))
		}
		.Object@sizes = sapply(
			X=.Object@midpoints, 
			FUN=estimator,
			seed=seed_sample, bw=bw
		)
  	return(.Object)
	}
)


###########################################################################
## Riverine pre-juveniles:

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "egg_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "fry_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

###########################################################################
## Riverine juveniles:

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "zero_summer_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "zero_autumn_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "zero_winter_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "one_spring_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "one_summer_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "one_autumn_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "one_winter_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "two_spring_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "two_summer_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "two_autumn_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "two_winter_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)


setMethod(
	f = "initialize",
	signature = signature(
		.Object = "three_spring_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "three_summer_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "three_autumn_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "three_winter_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "plus_spring_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "plus_summer_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "plus_autumn_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "plus_winter_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

###########################################################################
## Riverine smolts:

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "two_spring_riverine_smolt_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)


setMethod(
	f = "initialize",
	signature = signature(
		.Object = "two_autumn_riverine_smolt_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "three_spring_riverine_smolt_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "three_autumn_riverine_smolt_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "plus_spring_riverine_smolt_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "plus_autumn_riverine_smolt_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)

###########################################################################
## Estuarine smolts:

setMethod(
	f = "initialize",
	signature = signature(
		.Object = "two_spring_estuarine_smolt_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)



setMethod(
	f = "initialize",
	signature = signature(
		.Object = "three_spring_estuarine_smolt_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)


setMethod(
	f = "initialize",
	signature = signature(
		.Object = "plus_spring_estuarine_smolt_size_distribution"
	),
	definition = function(
			.Object, 
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		.Object <- callNextMethod(.Object, seed_sample,
															n_bins, limits, bw)
  	return(.Object)
	}
)


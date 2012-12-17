SEAL = FALSE

size_distribution <- setClass(
	Class = "size_distribution",
	representation = representation(
		sizes = "numeric",
		n_bins = "numeric",
		limits = "numeric",
		midpoints = "numeric"
	),
	prototype = prototype(
		sizes = vector(mode="numeric", length=0),
		n_bins = 0,
		limits = vector(mode="numeric", length=2),
		midpoints = vector(mode="numeric", length=0)
	),
	validity = function(object) {
		# Write fail conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

egg_size_distribution <- setClass(
	Class = "egg_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = -1,
		stage_name = "egg",
		month = 10
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)


fry_size_distribution <- setClass(
	Class = "fry_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 0,
		stage_name = "fry",
		month = 13 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

###########################################################################
## Freshwater juveniles:

zero_summer_size_distribution <- setClass(
	Class = "zero_summer_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 1,
		stage_name = "0+ summer",
		month = 16 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

zero_autumn_size_distribution <- setClass(
	Class = "zero_autumn_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 2,
		stage_name = "0+ autumn",
		month = 19 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

zero_winter_size_distribution <- setClass(
	Class = "zero_winter_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 3,
		stage_name = "0+ winter",
		month = 19 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

one_spring_size_distribution <- setClass(
	Class = "one_spring_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 4,
		stage_name = "1+ spring",
		month = 21 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)


one_summer_size_distribution <- setClass(
	Class = "one_summer_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 5,
		stage_name = "1+ summer",
		month = 24 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

one_autumn_size_distribution <- setClass(
	Class = "one_autumn_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 6,
		stage_name = "1+ autumn",
		month = 27 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

one_winter_size_distribution <- setClass(
	Class = "one_winter_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 7,
		stage_name = "1+ winter",
		month = 30 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

two_spring_size_distribution <- setClass(
	Class = "two_spring_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 8,
		stage_name = "2+ spring",
		month = 33 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)


two_summer_size_distribution <- setClass(
	Class = "two_summer_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 9,
		stage_name = "2+ summer",
		month = 36 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

two_autumn_size_distribution <- setClass(
	Class = "two_autumn_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 10,
		stage_name = "2+ autumn",
		month = 39
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

two_winter_size_distribution <- setClass(
	Class = "two_winter_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 11,
		stage_name = "2+ winter",
		month = 42 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

three_spring_size_distribution <- setClass(
	Class = "three_spring_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 12,
		stage_name = "3+ spring",
		month = 45 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)


three_summer_size_distribution <- setClass(
	Class = "three_summer_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 13,
		stage_name = "3+ summer",
		month = 48 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

three_autumn_size_distribution <- setClass(
	Class = "three_autumn_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 14,
		stage_name = "3+ autumn",
		month = 51
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

three_winter_size_distribution <- setClass(
	Class = "three_winter_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 15,
		stage_name = "3+ winter",
		month = 54 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)


plus_spring_size_distribution <- setClass(
	Class = "plus_spring_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 16,
		stage_name = "++ spring",
		month = 57 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)


plus_summer_size_distribution <- setClass(
	Class = "plus_summer_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 17,
		stage_name = "++ summer",
		month = 60 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

plus_autumn_size_distribution <- setClass(
	Class = "plus_autumn_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 18,
		stage_name = "++ autumn",
		month = 63
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

plus_winter_size_distribution <- setClass(
	Class = "plus_winter_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 19,
		stage_name = "++ winter",
		month = 66 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

###########################################################################
## Riverine smolts:


two_spring_riverine_smolt_size_distribution <- setClass(
	Class = "two_spring_riverine_smolt_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 9,
		stage_name = "2+ spring riverine smolt",
		month = 35 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)


two_autumn_riverine_smolt_size_distribution <- setClass(
	Class = "two_autumn_riverine_smolt_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 11,
		stage_name = "2+ autumn riverine smolt",
		month = 41
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

three_spring_riverine_smolt_size_distribution <- setClass(
	Class = "three_spring_riverine_smolt_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 13,
		stage_name = "3+ spring riverine smolt",
		month = 47 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)


three_autumn_riverine_smolt_size_distribution <- setClass(
	Class = "three_autumn_riverine_smolt_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 15,
		stage_name = "3+ autumn riverine smolt",
		month = 53
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)

plus_spring_riverine_smolt_size_distribution <- setClass(
	Class = "plus_spring_riverine_smolt_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 17,
		stage_name = "++ spring riverine smolt",
		month = 59 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)


plus_autumn_riverine_smolt_size_distribution <- setClass(
	Class = "plus_autumn_riverine_smolt_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 19,
		stage_name = "++ autumn riverine smolt",
		month = 66
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)


###########################################################################
## Estuarine smolts:


two_spring_estuarine_smolt_size_distribution <- setClass(
	Class = "two_spring_estuarine_smolt_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 9,
		stage_name = "2+ spring estuarine smolt",
		month = 35 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)


three_spring_estuarine_smolt_size_distribution <- setClass(
	Class = "three_spring_estuarine_smolt_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 13,
		stage_name = "3+ spring estuarine smolt",
		month = 47 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)


plus_spring_estuarine_smolt_size_distribution <- setClass(
	Class = "plus_spring_estuarine_smolt_size_distribution",
	contains = "size_distribution",
	representation = representation(
		stage = "numeric",
		stage_name = "character",
		month = "numeric"
	),
	prototype = prototype(
		stage = 17,
		stage_name = "++ spring estuarine smolt",
		month = 59 
	),
	validity = function(object) {
		# Write fails conditions which return(FALSE)
		return(TRUE)
	},
	sealed = SEAL
)




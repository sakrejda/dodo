###########################################################################
## Base class.
###########################################################################

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


setMethod(
	f = "initialize",
	signature = signature(
		.Object = "size_distribution"
	),
	definition = function(
			.Object, 
			Object_ = FALSE,
			seed_sample = rnorm(100), 
			n_bins = length(seed_sample)/10, 
			limits = c(
				min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
				max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
			),
			bw=as.integer(length(seed_sample)/15)+1
	) {
		if (is(Object_,"size_distribution")) {
			.Object@sizes 		= Object_@sizes
			.Object@n_bins 		= Object_@n_bins
			.Object@limits 		= Object_@limits
			.Object@midpoints = Object_@midpoints

		} else {

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
			.Object@sizes <- length(seed_sample) * (.Object@sizes/sum(.Object@sizes))
		}
  	return(.Object)
	}
)

################################################################################
### Pooling densities / numbers:
################################################################################


setMethod(
	f = "pool",
	signature = signature("size_distribution"),
	definition = function(...) {
		dots = list(...)
		szs = sapply(X=dots, FUN=function(x) x@sizes)
		szs <- apply(szs, 1, sum)
		o <- dots[[1]]
		o@sizes <- szs
		return(o)
	}
)

	
###########################################################################
## Class FACTORY function
###########################################################################

staged_size_distribution <- function(
	stage_name,
	stage, 
	age,
	where = .GlobalEnv
) { 
	### This factory sets the derived class, sets its init method, then
	### defines the reference class with its initialize method (which
	### just passes on the call.

	staged_size_distribution <- setClass(
		Class = paste(stage_name, "size_distribution", sep='_'),
		contains = "size_distribution",
		representation = representation(
			id = "character",
			stage = "numeric",
			stage_name = "character",
			month = "numeric"
		),
		prototype = prototype(
			id = "",
			stage = stage,
			stage_name = stage_name,
			age = age
		),
		validity = function(object) {
			# Write fails conditions which return(FALSE)
			return(TRUE)
		},
		where = where,
		sealed = SEAL
	)

	init_method <- setMethod(
		f = "initialize",
		signature = signature(
			.Object = paste(stage_name, "size_distribution", sep='_')
		),
		definition = function(
				.Object, 
				Object_ = FALSE,
				seed_sample = rnorm(100), 
				n_bins = length(seed_sample)/10, 
				limits = c(
					min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
					max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
				),
				bw=as.integer(length(seed_sample)/15)+1
		) {
			.Object <- callNextMethod(
				.Object = .Object,
				Object_ = Object_,
				seed_sample = seed_sample,
				n_bins = n_bins,
				limits = limits,
				bw = bw
			)
	  	return(.Object)
		},
		where = where
	)
	return(staged_size_distribution)	
}


################################################################################
### Staged growth method factory, takes stage_name, pGLM, env
################################################################################

staged_growth_factory <- function(
	stage_name,
	model,
	where = .GlobalEnv
) {

	growth_function <- function(.Object, covariates) {
			newdata <- data.frame(
				row = 1:(.Object@n_bins)
			)
			for ( nom in names(covariates) ) {
				### Relies on recycling to get the right number of entries:
				newdata[[nom]] <- covariates[[nom]]
			}
			## Calculate means:
			mu <- mapply(
				FUN = function(size, newdata) {
					newdata[['sizes']] <- size
					mu <- model$predict(newdata = newdata)
				},
				size = .Object@midpoints,
				MoreArgs = list(newdata=newdata)
			)
	
			## Apply error:
			S <- mapply(
				FUN = function(x,y, mod_sd) {
					dnorm(x=y, mean=x, sd=mod_sd)
				},
				x = mu,
				MoreArgs = list(
					y = .Object@midpoints, 
					mod_sd = sd(model$errors(1000))
				)
			)
	
			## Making sure that the transformation preserves mass:
			S <- apply(
				X=S, MARGIN=2, 
				FUN=function(x) {if(sum(x) != 0) x/sum(x) else x})
	
			## Transform:
			.Object@sizes <- as.vector(S %*% .Object@sizes)
			return(.Object)
	
	}

	return(growth_function)
}

################################################################################
### Staged transition method factory, takes stage_name, pGLM, env
################################################################################

staged_transition_factory <- function(
	stage_name_from,
	stage_name_to,
	model,
	where = .GlobalEnv
) {
	model

	transition_function <- function(.Object, covariates) {
			## First keep copy, transition will be from .ObjectA to .ObjectB,
		  ## leftovers in .ObjectA:
			.ObjectB <- new(
				paste(stage_name_to,'size_distribution',sep='_'),
				.Object
			)
			.ObjectA <- .Object

			## Calculate the proportion which transition
			S <- diag(.Object@n_bins)
			newdata <- data.frame(
				row = 1:(.Object@n_bins),
				sizes = .Object@midpoints
			)
			for ( nom in names(covariates) ) {
				### Relies on recycling to get the right number of entries:
				newdata[[nom]] <- covariates[[nom]]
			}
			diag(S) <- model$predict(newdata = newdata)
			eye <- diag(rep(1,nrow(S)))

			## Calculate density/count of remaining:
			.ObjectA@sizes <- as.vector((eye-S) %*% .Object@sizes)

			## Calculate density/count of transitioning:
			.ObjectB@sizes <- as.vector((  S) %*% .Object@sizes)

			
			return(list(.ObjectA,.ObjectB))
	}

	return(transition_function)
}

################################################################################
### Staged growth/"survival" method factory, takes stage_name, pGLM, env
################################################################################

staged_transformition_factory <- function(
	stage_name_from,
	stage_name_to,
	growth_model,
	transition_model,
	scaling_model,
	where = .GlobalEnv
) {

	transformition_function <- function(.Object, covariates) {
			newdata <- data.frame(
				row = 1:(.Object@n_bins)
			)
			for ( nom in names(covariates) ) {
				### Relies on recycling to get the right number of entries:
				newdata[[nom]] <- covariates[[nom]]
			}
			## Calculate means:
			mu <- mapply(
				FUN = function(size, newdata) {
					newdata[['sizes']] <- size
					mu <- model$predict(newdata = newdata)
				},
				size = .Object@midpoints,
				MoreArgs = list(newdata=newdata)
			)
	
			## Apply error:
			S <- mapply(
				FUN = function(x,y, mod_sd) {
					dnorm(x=y, mean=x, sd=mod_sd)
				},
				x = mu,
				MoreArgs = list(
					y = .Object@midpoints, 
					mod_sd = sd(model$errors(1000))
				)
			)
	
			## Making sure that the transformation preserves mass:
			S <- apply(
				X=S, MARGIN=2, 
				FUN=function(x) {if(sum(x) != 0) x/sum(x) else x})
	
			## Transform:
			.Object@sizes <- as.vector(S %*% .Object@sizes)

			## First keep copy, transition will be from .ObjectA to .ObjectB,
		  ## leftovers in .ObjectA:
			.ObjectB <- new(
				paste(stage_name_to,'size_distribution',sep='_'),
				.Object
			)
			.ObjectA <- .Object

			## Calculate the proportion which transition
			S <- diag(.Object@n_bins)
			newdata <- data.frame(
				row = 1:(.Object@n_bins),
				sizes = .Object@midpoints
			)
			for ( nom in names(covariates) ) {
				### Relies on recycling to get the right number of entries:
				newdata[[nom]] <- covariates[[nom]]
			}
			diag(S) <- model$predict(newdata = newdata)
			eye <- diag(rep(1,nrow(S)))

			## Calculate density/count of remaining:
			.ObjectA@sizes <- as.vector((eye-S) %*% .Object@sizes)

			## Calculate density/count of transitioning:
			.ObjectB@sizes <- as.vector((  S) %*% .Object@sizes)

			
			############# As a final step, .ObjectB is scaled by some model
			### based multiplier here:

			### Cool code goes here (in that ironic, code is never cool
			### sense).


			return(list(.ObjectA,.ObjectB))

	}

	return(transformition_function)
}

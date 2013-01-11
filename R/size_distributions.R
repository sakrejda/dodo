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
		.Object@sizes <- length(seed_sample) * (.Object@sizes/sum(.Object@sizes))
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
				seed_sample = rnorm(100), 
				n_bins = length(seed_sample)/10, 
				limits = c(
					min = min(seed_sample) - .1*(max(seed_sample)-min(seed_sample)),
					max = max(seed_sample) + .1*(max(seed_sample)-min(seed_sample))
				),
				bw=as.integer(length(seed_sample)/15)+1
		) {
			.Object <- callNextMethod()
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

	transition_function <- function(.Object, covariates) {
			## First keep copy, transition will be from .ObjectA to .Object,
		  ## leftovers in .ObjectA:
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

			## Calculate density/count of remaining:
			.ObjectA@sizes <- as.vector((1-S) %*% .Object@sizes)

			## Calculate density/count of transitioning:
			.Object@stage_name <- stage_name_to
			.Object@sizes <- as.vector((  S) %*% .Object@sizes)

			
			return(list(.ObjectA,.Object))
	}

	return(transition_function)
}

################################################################################
### Static methods for projecting size_distributions.
################################################################################

setMethod(
	f = "survival",
	signature = signature(
		.Object = "size_distribution",
		model = "glm",
		covariates = "list"
	),
	definition = function(.Object, model, covariates) {
#		S <- matrix(0, nrow = .Object@n_bins, ncol = .Object@n_bins)
		S <- diag(.Object@n_bins)
		newdata <- data.frame(
			row = 1:(.Object@n_bins),
			sizes = .Object@midpoints
		)
		for ( nom in names(covariates) ) {
			### Relies on recycling to get the right number of entries:
			newdata[[nom]] <- covariates[[nom]]
		}
		diag(S) <- predict(
				object = model,
				newdata = newdata,
				type = 'response'
			)
		.Object@sizes <- as.vector(S %*% .Object@sizes)
		return(.Object)
	}
)

setMethod(
	f = "survival",
	signature = signature(
		.Object = "size_distribution",
		model = "pGLM",
		covariates = "list"
	),
	definition = function(.Object, model, covariates) {
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
		.Object@sizes <- as.vector(S %*% .Object@sizes)
		return(.Object)
	}
)

setMethod(
	f = "survival",
	signature = signature(
		.Object = "size_distribution",
		model = "life_cycle",
		covariates = "list"
	),
	definition = function(.Object, model, covariates) {
		s_model = get_lc_node_model(model, .Object@stage_name, "survival")
		covariates = covariates[[.Object@stage_name]]
		.Object <- survival(.Object, model = s_model, covariates = covariates)
		return(.Object)
	}
)

setMethod(
	f = "survival",
	signature = signature(
		.Object = "size_distribution",
		model = "formula",
		covariates = "list",
		coefficients = "numeric",
		sigma = "numeric",
		inverse_link = "function"
	),
	definition = function(
		.Object, model, covariates,
		coefficients, sigma, inverse_link
	) {
#		S <- matrix(0, nrow = .Object@n_bins, ncol = .Object@n_bins)
		S <- diag(.Object@n_bins)
		newdata <- data.frame(
			row = 1:(.Object@n_bins),
			sizes = .Object@midpoints
		)
		for ( nom in names(covariates) ) {
			### Relies on recycling to get the right number of entries:
			newdata[[nom]] <- covariates[[nom]]
		}
		mf <- model.frame(formula = formula, data = newdata)
		diag(S) <- inverse_link(   # func passed as argument.
				model.matrix(mf) %*% coefficients +
				rnorm(nrow(mf),0,sigma)
		)

		.Object@sizes <- as.vector(S %*% .Object@sizes)
		return(.Object)
	}
)


setMethod(
	f = "growth",
	signature = signature(
		.Object = "size_distribution",
		model = "lm",
		covariates = "list"
	),
	definition = function(.Object, model, covariates) {
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
				mu <- predict(object = model, newdata = newdata, type = 'response')
				mu <- unique(mu)
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
				mod_sd = sd(residuals(model))
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
)


setMethod(
	f = "growth",
	signature = signature(
		.Object = "size_distribution",
		model = "pGLM",
		covariates = "list"
	),
	definition = function(.Object, model, covariates) {
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
)

setMethod(
	f = "growth",
	signature = signature(
		.Object = "size_distribution",
		model = "life_cycle",
		covariates = "list"
	),
	definition = function(.Object, model, covariates) {
		g_model = get_lc_node_model(model, .Object@stage_name, "grow")
		covariates = covariates[[.Object@stage_name]]
		.Object <- growth(.Object, model = g_model, covariates = covariates)
		return(.Object)
	}
)

setMethod(
	f = "growth",
	signature = signature(
		.Object = "size_distribution",
		model = "formula",
		covariates = "list",
		coefficients = "numeric",
		sigma = "numeric",
		inverse_link = "function"
	),
	definition = function(
		.Object, model, covariates,
		coefficients, sigma, inverse_link
	) {
		newdata <- data.frame(
			row = 1:(.Object@n_bins),
			sizes = .Object@midpoints
		)
		for ( nom in names(covariates) ) {
			### Relies on recycling to get the right number of entries:
			newdata[[nom]] <- covariates[[nom]]
		}

		## Calculate means:
		mf <- model.frame(formula = formula, data = newdata)
		mu <- inverse_link(model.matrix(mf) %*% coefficients)

		## Apply error:
		S <- mapply(
			FUN = function(x,y, mod_sd) {
				dnorm(x=y, mean=x, sd=mod_sd)
			},
			x = mu,
			MoreArgs = list(
				y = .Object@midpoints, 
				mod_sd = sigma
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
)



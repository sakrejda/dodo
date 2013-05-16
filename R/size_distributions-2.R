###########################################################################
## Base class.
###########################################################################

size_distribution <- setRefClass(
	Class = "size_distribution",
	fields = list(
		densities = "numeric",
		n_bins = "numeric",
		limits = "numeric",
		midpoints = "numeric"
	),
	methods = list(
		initialize = function(n_bins, limits, FUN=NULL, ...) {
			densities <<- vector(mode="numeric", length=n_bins)
			n_bins <<- n_bins
			limits <<- limits
			h <- (limits[['max']] - limits[['min']]) / n_bins
			midpoints <<- limits[['min']] + ((1:n_bins)-0.5) * h
			if (!is.null(FUN)) {
				d <- sapply(X=midpoints, FUN=FUN, ...)
				if (is.numeric(d) && is.vector(d)) densities <<- d
			}
			return(.self)
		},
		seed = function(sample, bw=NULL) {
			if (is.null(bw)) as.integer(length(sample)/15)+1
			estimator <- function(x, seed, bw) {
				1/(length(seed)*bw) * sum(dnorm(x=x, mean=seed, sd=1))
			}
			densities <<- sapply( X=midpoints, FUN=estimator, seed=sample, bw=bw)
			densities <<- length(sample) * (densities/sum(densities))
		}
	),
	sealed = SEAL
)


###########################################################################
## Class FACTORY function
###########################################################################

staged_size_distribution <- function(
	stage_name,
	stage, 
	traits,
	covariates,
	where = .GlobalEnv
) { 
	### This factory sets the derived class, sets its init method, then
	### defines the reference class with its initialize method (which
	### just passes on the call.
	stage_name; stage; traits; covariates

	staged_size_distribution <- setRefClass(
		Class = paste(stage_name, "size_distribution", sep='_'),
		contains = "size_distribution",
		fields = list(
			id = "character",
			stage = "numeric",
			stage_name = "character",
			traits = "list",
			covariates = "list",
			newdata = function(x=NULL) {
				if (!is.null(x)) stop("Data is a calculated field.")
				newdata <- data.frame(
					row = 1:(n_bins),
					sizes = midpoints,
					covariates,
					traits[!sapply(traits,is.function)]   ### FIXED traits.
				)
	
				for (f in traits[sapply(traits,is.function)]) {
					newdata <- f(newdata)
				}   ### FUNCTION value traits
				return(newdata)
			}
		),
		methods = list(
			initialize = function(traits, covariates=list(), ...) {
				traits <<- traits
				covariates <<- covariates
				callSuper(...)
				return(.self)
			}
		),
		where = where,
		sealed = SEAL
	)
	return(staged_size_distribution)
}

################################################################################
### Staged growth method factory, takes stage_name, pGLM, env
################################################################################


dnorm_projection_factory <- function(
	stage_name,
	mean_model,
	variance_model,
	where = .GlobalEnv
) {
	mean_model, variance_model   ## Closure!

	dnorm_projection <- function(.Object, covariates) {

			## Calculate mean and variance:
			nd <- .Object$newdata
			mu <- mean_model$predict(newdata = nd)
			variance <- variance_model$predict(newdata = nd)
	
			## Apply error:
			S <- mapply(
				FUN = function(x,y,sd) {
					dnorm(x=y, mean=x, sd=sd)
				},
				x = mu,
				sd = sd,
				MoreArgs = list(y = .Object$midpoints)
			)
	
			## Making sure that the transformation preserves mass:
			S <- apply(
				X=S, MARGIN=2, 
				FUN=function(x) {if(sum(x) != 0) x/sum(x) else x})
	
			return(S)
	
	}

	return(dnorm_projection)
}

################################################################################
### Staged transition method factory, takes stage_name, pGLM, env
################################################################################

transition_factory <- function(
	transition_model,
	where = .GlobalEnv
) {
	transition_model		## Closure!

	transition_projection <- function(.Object, covariates) {

			## Transition matrix:
			S <- diag(.Object$n_bins)

			## Build environment/trait data frame:
			ndata <- .Object$newdata

			## Predict:
			diag(S) <- model$predict(newdata = newdata)
			
			return(S)
	}

	return(transition_function)
}


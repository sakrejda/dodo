
################################################################################
### Factories of projection functions to wed the function to a
### particular model or set of models.
################################################################################


dnorm_projection_factory <- function(
	mean_model,
	variance_model,
	target_dims = NULL,
	where = .GlobalEnv
) {
	mean_model; variance_model; target_dims   ## Closure!

	dnorm_projection <- function(.Object, stage, covariates) {
			if (is.null(target_dims)) { 
				target_dims <- c(
					n_bins = .Object@n_bins[.Object@stage_names == stage],
					minimum = .Object@minima[.Object@stage_names == stage],
					maximum = .Object@maxima[.Object@stage_names == stage])
					midpts = .Object$get_midpoints(stage=stage)
			} else {
				h <- (maximum - minimum) / n_bins
				midpts <- minimum + ((1:n_bins)-0.5) * h
			}
			if (!all(c('n_bins', 'minimum', 'maximum') %in% names(target_dims)) ) {
				stop("Target dimensins must be a vector specifying the number of\n
						  bins ('n_bins'), as well as the limits ('minumum', 'maximum')")
			}

			## Calculate mean and variance:
			nd <- .Object$newdata(stage=stage, covariates=covariates)
			mu <- mean_model$predict(newdata = nd)
			variance <- variance_model$predict(newdata = nd)
	
			## Apply:
			S <- mapply(
				FUN = function(x,y,sd) {
					dnorm(x=y, mean=x, sd=sd)
				},
				x = mu,
				sd = sd,
				MoreArgs = list(y = midpts)
			)
	
			## Making sure that the transformation preserves mass:
			S <- apply(
				X=S, MARGIN=2, 
				FUN=function(x) {if(sum(x) != 0) x/sum(x) else x})
	
			return(S)
	
	}

	return(dnorm_projection)
}

dlnorm_projection_factory <- function(
	mean_model,
	variance_model,
	where = .GlobalEnv
) {
	mean_model; variance_model   ## Closure!

	dlnorm_projection <- function(.Object, stage, covariates) {

			## Calculate mean and variance:
			nd <- .Object$newdata(stage=stage, covariates=covariates)
			mu <- mean_model$predict(newdata = nd)
			variance <- variance_model$predict(newdata = nd)
	
			## Apply:
			S <- mapply(
				FUN = function(mu,to,sd) {
					if (to <= mu) 
						return(0) 
					else 
						return(dlnorm(x=to, meanlog=mu, sdlog=sd))
				},
				mu = mu,
				sd = sd,
				MoreArgs = list(to = .Object$get_midpoints(stage=stage))
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
### self-projection allows only contributions from a bin to itself, or
### to the same bin in another stage.
################################################################################

self_projection_factory <- function(
	self_model,
	where = .GlobalEnv
) {
	self_model		## Closure!

	self_projection <- function(.Object, stage, covariates) {
			S <- diag(.Object$n_bins)
			diag(S) <- model$predict(
				newdata = .Object$newdata(stage=stage, covariates=covariates))
			return(S)
	}

	return(self_projection)
}



################################################################################
### stretch-projection allows you to keep the shape of a density but to 
### change the change the midpoints.
################################################################################


stretch_projection_factory <- function(
	target_dims = NULL,
	where = .GlobalEnv
) {
	if (is.null(target_dims) || 
			!all(c('n_bins', 'minimum', 'maximum') %in% names(target_dims)) 
	) { 
		stop("Target dimensins must be a vector specifying the number of\n
				  bins ('n_bins'), as well as the limits ('minumum', 'maximum')")
	} else {
		n_bins  <- target_dims['n_bins' ]
		minimum <- target_dims['minimum']
		maximum <- target_dims['maximum']
		h <- (maximum - minimum) / n_bins
		midpts <- minimum + ((1:n_bins)-0.5) * h
	}
	target_dims; midpts; n_bins; minimum; maximum

	stretch_projection <- function(.Object, stage, covariates) {
		j <- length(.Object$get_midpoints(stage=stage))
		k <- length(midpts)
		S <- interpol.matrix(n=k, m=j)
		return(S) 
	}
	return(stretch_projection)
}












################################################################################
### Factories of projection functions to wed the function to a
### particular model or set of models.
################################################################################


dnorm_projection_factory <- function(
	mean_model,
	variance_model = NULL,
	sd_model = NULL,
	target_dims = NULL,
	where = .GlobalEnv
) {
	mean_model; variance_model; sd_model; target_dims   ## Closure!

	dnorm_projection <- function(.Object, stage, covariates) {
			if (is.null(target_dims)) { 
				target_dims <- c(
					n_bins = .Object@n_bins[.Object@stage_names == stage],
					minimum = .Object@minima[.Object@stage_names == stage],
					maximum = .Object@maxima[.Object@stage_names == stage])
					midpts = .Object$get_midpoints(stage=stage)
			} else {
				n_bins <- target_dims['n_bins']
				minimum <- target_dims['minimum']
				maximum <- target_dims['maximum']
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
			if (is.null(sd_model) && !is.null(variance_model)) {
				variance <- as.numeric(variance_model$predict(newdata = nd))
			} else if (is.null(variance_model) && !is.null(sd_model)) {
				variance <- as.numeric(sd_model$predict(newdata = nd))^2
			}
	
			## Apply:
			S <- mapply(
				FUN = function(x,y,sd) {
					dnorm(x=y, mean=x, sd=sd)
				},
				x = mu,
				sd = sqrt(variance),
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
	variance_model = NULL,
	sd_model = NULL,
	target_dims = NULL,
	where = .GlobalEnv
) {
	mean_model; variance_model; sd_model; target_dims   ## Closure!

	dlnorm_projection <- function(.Object, stage, covariates) {

			if (is.null(target_dims)) { 
				target_dims <- c(
					n_bins = .Object@n_bins[.Object@stage_names == stage],
					minimum = .Object@minima[.Object@stage_names == stage],
					maximum = .Object@maxima[.Object@stage_names == stage])
					midpts = .Object$get_midpoints(stage=stage)
			} else {
				n_bins <- target_dims['n_bins']
				minimum <- target_dims['minimum']
				maximum <- target_dims['maximum']
				h <- (maximum - minimum) / n_bins
				midpts <- minimum + ((1:n_bins)-0.5) * h
			}
			if (!all(c('n_bins', 'minimum', 'maximum') %in% names(target_dims)) ) {
				stop("Target dimensins must be a vector specifying the number of\n
						  bins ('n_bins'), as well as the limits ('minumum', 'maximum')")
			}

			## Calculate mean and variance:
			nd <- .Object$newdata(stage=stage, covariates=covariates)
			mu <- as.numeric(mean_model$predict(newdata = nd))
			if (is.null(sd_model) && !is.null(variance_model)) {
				variance <- as.numeric(variance_model$predict(newdata = nd))
			} else if (is.null(variance_model) && !is.null(sd_model)) {
				variance <- as.numeric(sd_model$predict(newdata = nd))^2
			}
	
			## Apply:
			S <- mapply(
				FUN = function(mu,to,sd) {
					return(dlnorm(x=to, meanlog=mu, sdlog=sd))
				},
				mu = mu,
				sd = sqrt(variance),
				MoreArgs = list(to=midpts)
			)
	
			## Making sure that the transformation preserves mass:
			S <- apply(
				X=S, MARGIN=2, 
				FUN=function(x) {if(sum(x) != 0) x/sum(x) else x})
	
			return(S)
	
	}

	return(dlnorm_projection)
}


offset_dlnorm_projection_factory <- function(
	mean_model,
	variance_model = NULL,
	sd_model = NULL,
	target_dims = NULL,
	where = .GlobalEnv
) {
	mean_model; variance_model; sd_model; target_dims   ## Closure!

	offset_dlnorm_projection <- function(.Object, stage, covariates) {

			if (is.null(target_dims)) { 
				target_dims <- c(
					n_bins = .Object@n_bins[.Object@stage_names == stage],
					minimum = .Object@minima[.Object@stage_names == stage],
					maximum = .Object@maxima[.Object@stage_names == stage])
					midpts = .Object$get_midpoints(stage=stage)[,1]
			} else {
				n_bins <- target_dims['n_bins']
				minimum <- target_dims['minimum']
				maximum <- target_dims['maximum']
				h <- (maximum - minimum) / n_bins
				midpts <- minimum + ((1:n_bins)-0.5) * h
			}
			if (!all(c('n_bins', 'minimum', 'maximum') %in% names(target_dims)) ) {
				stop("Target dimensins must be a vector specifying the number of\n
						  bins ('n_bins'), as well as the limits ('minumum', 'maximum')")
			}

			## Calculate mean and variance:
			nd <- .Object$newdata(stage=stage, covariates=covariates)
			mu <- as.numeric(mean_model$predict(newdata = nd))
			if (is.null(sd_model) && !is.null(variance_model)) {
				variance <- as.numeric(variance_model$predict(newdata = nd))
			} else if (is.null(variance_model) && !is.null(sd_model)) {
				variance <- as.numeric(sd_model$predict(newdata = nd))^2
			}
	
			## Apply:
			S <- mapply(
				FUN = function(mu,to,y,sd) {
					return(dolnorm(x=to, y=y, meanlog=mu, sdlog=sd))
				},
				mu = mu,
				sd = sqrt(variance),
				y = .Object$get_midpoints(stage=stage)[,1],
				MoreArgs = list(to=midpts)
			)
	
			## Making sure that the transformation preserves mass:
			S <- apply(
				X=S, MARGIN=2, 
				FUN=function(x) {if(sum(x) != 0) x/sum(x) else x})
	
			return(S)
	
	}

	return(offset_dlnorm_projection)
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
			j <- length(.Object$get_midpoints(stage=stage))
			S <- diag(j)
			diag(S) <- self_model$predict(
				newdata = .Object$newdata(stage=stage, covariates=covariates))
			return(S)
	}

	return(self_projection)
}



################################################################################
### squash and stretch-projection allows you to keep the shape of a density but to 
### change the change the number of bins.
################################################################################

squash_projection_factory <- function(
	target_dims = NULL,
	where = .GlobalEnv
) {
	target_dims

	squash_projection <- function(.Object, stage, covariates) {
		n_bins_orig = .Object@n_bins[.Object@stage_names == stage]
		minimum_orig = .Object@minima[.Object@stage_names == stage]
		maximum_orig = .Object@maxima[.Object@stage_names == stage]
		h_orig = (maximum_orig - minimum_orig) / n_bins
		midpts_orig = .Object$get_midpoints(stage=stage)

		if (is.null(target_dims)) { 
			n_bins = .Object@n_bins[.Object@stage_names == stage]
			minimum = .Object@minima[.Object@stage_names == stage]
			maximum = .Object@maxima[.Object@stage_names == stage]
			h <- (maximum - minimum) / n_bins
			midpts = .Object$get_midpoints(stage=stage)
		} else {
			if (!all(c('n_bins', 'minimum', 'maximum') %in% names(target_dims)) ) {
				stop("Target dimensins must be a vector specifying the number of\n
						  bins ('n_bins'), as well as the limits ('minumum', 'maximum')")
			}
			n_bins <- target_dims['n_bins']
			minimum <- target_dims['minimum']
			maximum <- target_dims['maximum']
			h <- (maximum - minimum) / n_bins
			midpts <- minimum + ((1:n_bins)-0.5) * h
		}
		n <- length(midpts)
		m <- length(midpts_orig)
		
		Slist <- mapply(
			FUN = function(idx, M, x, xstar) {
				if (xstar[idx] < min(x)) { return(rep(0,ncol(M))) }
				if (xstar[idx] > max(x)) { return(rep(0,ncol(M))) }
			
				row <- M[idx,]
				xa <- max(x[x <= xstar[idx]])
			  a <- which(x == xa)
				if (xa == xstar[idx]) {
					row[a] <- 1; return(row)
				}

				xb <- min(x[x >= xstar[idx]])
				b <- which(x == xb)
				if (xb == xstar[idx]) {
					row[b] <- 1; return(row)
				}
				
				k <- (xstar[idx] - xa)/(xb - xa)
				print(k)
				row[a] <- 1-k
				row[b] <- k

				return(row)
			},
			idx   = seq(1,n),
			MoreArgs = list(
				M     = matrix(data = 0, nrow=n, ncol=m),
				x     = midpts_orig,
				xstar = midpts
			),
			SIMPLIFY = FALSE
		)
		S <- do.call(what=rbind, args = Slist)
		S <- S / (h / h_orig)

		return(S)
	}

	return(squash_projection)
}

stretch_projection_factory <- squash_projection_factory





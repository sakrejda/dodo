setMethod(
	f = "survive",
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
	f = "survive",
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
	f = "survive",
	signature = signature(
		.Object = "size_distribution",
		model = "life_cycle",
		covariates = "list"
	),
	definition = function(.Object, model, covariates) {
		s_model = get_lc_node_model(model, .Object@stage_name, "survival")
		covariates = covariates[[.Object@stage_name]]
		.Object <- grow(.Object, model = s_model, covariates = covariates)
		return(.Object)
	}
)

setMethod(
	f = "survive",
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

			
		#predict(
		#		object = model,
		#		newdata = newdata,
		#		type = 'response'
		#	)

		.Object@sizes <- as.vector(S %*% .Object@sizes)
		return(.Object)
	}
)

setMethod(
	f = "grow",
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
	f = "grow",
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
	f = "grow",
	signature = signature(
		.Object = "size_distribution",
		model = "life_cycle",
		covariates = "list"
	),
	definition = function(.Object, model, covariates) {
		g_model = get_lc_node_model(model, .Object@stage_name, "grow")
		covariates = covariates[[.Object@stage_name]]
		.Object <- grow(.Object, model = g_model, covariates = covariates)
		return(.Object)
	}
)

setMethod(
	f = "grow",
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

setMethod(
	f = "pool",
	signature = signature("size_distribution"),
	definition = function(...) {
		dots = list(...)
		szs = sapply(X=dots, FUN=function(x) x@sizes)
		szs <- matrix(
			data = unlist(szs),
			nrow = length(dots[[1]]),
			ncol = length(dots)
		)
		szs <- apply(szs, 2, sum)
		o <- dots[[1]]
		o@sizes <- szs
		return(o)
	}
)

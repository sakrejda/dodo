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




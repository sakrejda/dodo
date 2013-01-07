#  definition = function(
#    .Object, model, covariates,
#    coefficients, sigma, inverse_link
#  ) {
#    S <- diag(.Object@n_bins)
#    newdata <- data.frame(
#      row = 1:(.Object@n_bins),
#      sizes = .Object@midpoints
#    )
#    for ( nom in names(covariates) ) {
#      ### Relies on recycling to get the right number of entries:
#      newdata[[nom]] <- covariates[[nom]]
#    }
#    mf <- model.frame(formula = formula, data = newdata)
#    diag(S) <- inverse_link(   # func passed as argument.
#        model.matrix(mf) %*% coefficients +
#        rnorm(nrow(mf),0,sigma)
#    )
#    .Object@sizes <- as.vector(S %*% .Object@sizes)
#    return(.Object)
#  }
#

setOldClass(Classes="family", prototype=gaussian())

setRefClass(Class = "pGLM",
	fields = list(
		formula = "formula",
		family = "family",
		coefficients = "numeric",
		epsilon = "function",
		covariates = "list",
		data = "data.frame",
		model_frame = "data.frame",
		mm = "matrix",
		mf = "data.frame",
		dat = "data.frame"
	),
	methods = list(
		initialize = function(
			formula = ~ 1, 
			family = gaussian(),
			coefficients = list(),
			epsilon = function() {0}
		) {
			formula 			<<- formula
			family 				<<- family
			coefficients 	<<- coefficients
			epsilon 			<<- epsilon
			return(.self)
		},
		model_matrix = function(
			new_data = NULL, 
			covariates = NULL,
			n = NULL 
		) {
			if (!is.null(new_data)) n <- nrow(new_data)
			if (is.null(new_data) && !is.null(covariates) && !is.null(n)) {
		    new_data <- data.frame(row = 1:n)
		    for ( nom in names(covariates) ) {
    		  ### Relies on recycling to get the right number of entries:
		      new_data[[nom]] <- covariates[[nom]]
		    }
			} 
			if (!is.null(new_data) && !is.null(n)) {
				mf <<- model.frame(formula = formula, data = new_data)
			} else { stop("Bad call to model_matrix method.") }
			mm <<- model.matrix(object = formula, data = mf)
		},
		predict = function(
			newdata = NULL, 
			covariates = NULL,
			n = NULL, 
			...
		) {
			ml <- as.list(match.call())
			if (!is.null(newdata) || !is.null(covariates))
				model_matrix(newdata, covariates, n)
			if (ncol(.self$mm) == length(.self$coefficients)) {
				pred <- .self$mm %*% coefficients + epsilon(n=n)
			} else {
				stop("Object not ready for predictions, model matrix not formed.\n\n")
			}
			return(pred)
		},
		errors = function(n = 100) epsilon(n=n)
	)
)

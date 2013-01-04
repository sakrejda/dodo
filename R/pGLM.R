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

setRefClass(Class = "pGLM",
	fields = list(
		formula = "formula",
		family = "family",
		coefficients = "list",
		epsilon = "function",
		covariates = "list",
		data = "data.frame",
		model_frame = "data.frame"
	),
	representation = list(
		mm = "matrix",
		mf = "data.frame",
		dat = "data.frame"
	),
	methods = list(
		initialize = function(
			formula = y ~ 1, 
			family = gaussian,
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
			n = 1
		) {
			if (!is.null(new_data) && is.null(covariates) && (n == 1)) {
				mf <- model.frame(formula = formula, data = new_data)
			} else if (is.null(new_data) && !is.null(covariates) && !is.null(n)) {
		    new_data <- data.frame(row = 1:n)
		    for ( nom in names(covariates) ) {
    		  ### Relies on recycling to get the right number of entries:
		      new_data[[nom]] <- covariates[[nom]]
		    }
			} else { stop("Bad call to model_matrix method.") }
			.self@mm <- model.matrix(mf)
		},
		predict = function(
			newdata = NULL, 
			covariates = NULL,
			...
		) {
			ml <- as.list(match.call())
			model_matrix(newdata, covariates)
			pred <- .self@mm %*% coefficients
			return(pred)
		}
	)
)

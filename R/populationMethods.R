setMethod(
	f = "stage_names",
	signature = signature(.Object = "population"),
	definition = function(.Object) {
		return(stage_names(.Object@life_cycle))
	}
)

setMethod(
	f = "+",
	signature = signature(e1 = "population", e2 = "size_distribution"),
	definition = function(e1, e2) {
		if (e2@stage_name %in% stage_names(e1)) {
			e1@promoted[[e2@stage_name]] <- c(			
				e1@promoted[[e2@stage_name]], list(e2))
		} else {
			msg <- paste(
				"'", e2@stage_name, ", does not belong in the population.",
				sep = '')
			stop(msg)
		}
		return(e1)
	}
)

setMethod(
	f = "+",
	signature = signature(e1 = "size_distribution", e2 = "population"),
	definition = function(e1, e2) e2 + e1
)

	

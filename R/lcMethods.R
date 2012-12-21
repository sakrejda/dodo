
setMethod(
	f = "stage_names",
	signature = signature(
		.Object = "life_cycle"
	),
	definition = function(.Object) {
		return(.Object@stage_data[['stage_name']])
	}
)


setMethod(
	f = "stage_names",
	signature = signature(
		.Object = "life_cycle"
	),
	definition = function(.Object) {
		return(.Object@stage_data[['stage_name']])
	}
)

setMethod(
	f = "add_lc_model",
	signature = signature(
		.Object = "life_cycle",
		from = "character",
		to = "character",
		type = "character",
		model = "lc_lm"
	),
	definition = function(.Object, from, to, type, model) {
		edge_name <- paste("from","to",sep="|")
		if (!(edge_name %in% names(.Object@edgeData))) {
			.Object@graph <- edgeDataDefaults(
				self = .Object@graph,
				attr = paste('lc_lm', type, sep='_')
			) <- new('lc_lm')
		}
		edgeData(
			self = .Object@graph,
			from = from,
			to = to,
			attr = paste('lc_lm', type, sep = '_')
		) <- model
		return(.Object)
	}
)

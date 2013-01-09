setClass("size_distribution")
SEAL = FALSE


## For life cycles:
setGeneric(name="make_life_cycle",
					 def=function(stages, parents, ...)
						 standardGeneric("make_life_cycle"),
					 	valueClass = "life_cycle")
setGeneric(name="add_lc_node_model",
					 def=function(.Object, node, type, model)
						 standardGeneric("add_lc_node_model"),
						valueClass = "life_cycle")
setGeneric(name="get_lc_node_model",
					 def=function(.Object, node, type)
						 standardGeneric("get_lc_node_model"),
					)
setGeneric(name="add_lc_transition_model",
					 def=function(.Object, from, to, type, model)
						 standardGeneric("add_lc_transition_model"),
						valueClass = "life_cycle")
setGeneric(name="get_lc_transition_model",
					 def=function(.Object, from, to, type)
						 standardGeneric("get_lc_transition_model")
					)
setGeneric(name="stage_names",
					 def=function(.Object)
						 standardGeneric("stage_names"),
						 valueClass = "character")

## For size_distributions, returning same, or list of same (use union on
## valueClass?
setGeneric(name="survival",
					 def=function(.Object, model, covariates, 
												coefficients, sigma, inverse_link)
						 standardGeneric("survival"),
						valueClass = "size_distribution")
setGeneric(name="growth",
					 def=function(.Object, model, covariates,
												coefficients, sigma, inverse_link)
						 standardGeneric("growth"),
						valueClass = "size_distribution")
setGeneric(name="smolting",
					 def=function(.Object, model, covariates,
												coefficients, sigma, inverse_link)
						 standardGeneric("smolting")
					)
setGeneric(name="aging",
					 def=function(.Object, model, covariates,
												coefficients, sigma, inverse_link)
						 standardGeneric("aging"),
						valueClass = "size_distribution")

setClassUnion(name="listORsize_distribution", 
							members = c('list','size_distribution'))
setGeneric(name="transition",
					 def=function(.Object, model, covariates,
												coefficients, sigma, inverse_link)
						 standardGeneric("transition"),
						valueClass = "listORsize_distribution"
					)

## For size_distributions, just adds up the density/number per bin.
setGeneric(name="pool",
					 def=function(...)
						 standardGeneric("pool")
					 )

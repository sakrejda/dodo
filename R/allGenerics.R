
# Examples...
#setGeneric(name="get_N", 
#					 def=function(.Object) standardGeneric("get_N"), 
#					 valueClass = "numeric")
#setGeneric(name="get_recaptures", 
#					 def=function(.Object, id) standardGeneric("get_recaptures"),
#					 valueClass = "numeric")
#setGeneric(name="set_deaths",
#					 def=function(.Object, id, times_of_deaths) standardGeneric("set_deaths"),
#					 valueClass = "numeric")
#setGeneric(name="get_sampled",
#					 def=function(.Object) standardGeneric("get_sampled"),
#					 valueClass = "logical")
#
setGeneric(name="make_life_cycle",
					 def=function(stages, parents, ...)
						 standardGeneric("make_life_cycle"),
					 	valueClass = "life_cycle")
setGeneric(name="add_lc_node_model",
					 def=function(.Object, node, type, model)
						 standardGeneric("add_lc_node_model"),
						valueClass = "life_cycle")
setGeneric(name="add_lc_transition_model",
					 def=function(.Object, from, to, type, model)
						 standardGeneric("add_lc_transition_model"),
						valueClass = "life_cycle")
setGeneric(name="survive",
					 def=function(.Object, model, covariates, 
												coefficients, sigma, inverse_link)
						 standardGeneric("survive"),
						valueClass = "size_distribution")
setGeneric(name="grow",
					 def=function(.Object, model, covariates,
												coefficients, sigma, inverse_link)
						 standardGeneric("grow"),
						valueClass = "size_distribution")
setGeneric(name="stage_names",
					 def=function(.Object)
						 standardGeneric("stage_names"),
						 valueClass = "character")
setGeneric(name="sync",
					 def=function(p)
						 standardGeneric("sync")
					 )
setGeneric(name="pool",
					 def=function(l)
						 standardGeneric("pool")
					 )

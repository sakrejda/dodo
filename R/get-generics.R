

setGeneric(name="get_N", 
					 def=function(.Object) standardGeneric("get_N"), 
					 valueClass = "numeric")
setGeneric(name="get_K", 
					 def=function(.Object) standardGeneric("get_K"), 
					 valueClass = "numeric")
setGeneric(name="get_recaptures", 
					 def=function(.Object, id) standardGeneric("get_recaptures"),
					 valueClass = "numeric")
setGeneric(name="get_recaptures_matrix",
					 def=function(.Object) standardGeneric("get_recaptures_matrix"),
					 valueClass = "matrix")
setGeneric(name="get_surveys",
					 def=function(.Object) standardGeneric("get_surveys"),
					 valueClass = "numeric")
setGeneric(name="get_births",
					 def=function(.Object) standardGeneric("get_births"),
					 valueClass = "numeric")
setGeneric(name="get_first_obs",
					 def=function(.Object) standardGeneric("get_first_obs"),
					 valueClass = "numeric")
setGeneric(name="get_last_obs",
					 def=function(.Object) standardGeneric("get_last_obs"),
					 valueClass = "numeric")
setGeneric(name="get_deaths",
					 def=function(.Object) standardGeneric("get_deaths"),
					 valueClass = "numeric")

setGeneric(name="get_ll_phi_components",
					 def=function(.Object) standardGeneric("get_ll_phi_components"),
					 valueClass = "numeric")
setGeneric(name="get_ll_p_components",
					 def=function(.Object) standardGeneric("get_ll_p_components"),
					 valueClass = "numeric")

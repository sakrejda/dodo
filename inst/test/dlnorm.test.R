library(dodo)

o <- within(data = list(), expr = {
	stages = letters
	n_bins = rbinom(n=length(letters), size=5, prob=0.3) + 1
	minima = rep(0,length(letters))
	maxima = rep(100, length(letters))
	allowed_projections = c('survive', 'age')

	pop <- new('population', 
							stages = stages,
							bins = n_bins,
							minima = minima,
							maxima = maxima)

	gr_mean <- new('pGLM',
	  formula = ~ 1,
	  family = gaussian(),
	  coefficients = list(
	    "(Intercept)"  = 55
	  ),
	  epsilon = function(n=1) { rep(0,n) },
	  samp = FALSE
	)

	gr_var <- new('pGLM',
	  formula = ~ 1,
	  family = gaussian(link="log"),
	  coefficients = list(
	    "(Intercept)"  = 0.5 
	  ),
	  epsilon = function(n=1) { rep(0,n) },
	  samp = FALSE
	)


	stretcher <- dlnorm_projection_factory(
								mean_model = gr_mean,
								variance_model = gr_var,
								target_dims = c(n_bins=50, minimum=20, maximum=100))

	S <- stretcher(pop$distribution, 'a', covariates=c(a=3))       
})

## Fry growth:


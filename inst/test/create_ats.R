library(dodo)
data('estimates-ATS-smolt-M3')
data('stages.ats')

## Data:
stages <- structure(
	list(
	standardize_size = function(df) {
		df[['std_sizes']] <- (df[['sizes']] - df[['sizeAtAge']])/df[['sdAtAge']]
		return(df)
	},
	flow_conversion = function(df) {
		flowMeans <- c(28,11,12,26)
		flowSds <- c(9,5,6,7.5)
		df[['flow_std']] <- 
			(df[['flow']] - flowMeans[df[['season']]]) / flowSds[df[['season']]]
		return(df)
	},
	temp_conversion = function(df) {
		tempMeans <- c(18.02234,18.50568,18.06938,17.96012)
		tempSds <- c(0.019765602, 0.031461813, 0.022657542, 0.00111111)
		df[['temp_std']] <-
			(df[['WB']] - tempMeans[df[['season']]]) / tempSds[df[['season']]]
		return(df)
	}

	),
	.Names = c("standardize_size", "flow_conversion", "temp_conversion") 

)

stages <- c(stages, stages.ats)

pop <- new('population', 
	stages = stages.ats[['stage']],
	bins = stages.ats[['n_bins']],
	minima = stages.ats[['minima']],
	maxima = stages.ats[['maxima']],
	projections = c('survive', 'stretch', 'smolt', 'grow'),
	traits = stages 
)

egg_self_model <- new('pGLM',
	formula = ~ 1,
	family = binomial(link=logit),
	coefficients = list(
		"(Intercept)" = 2.2
	)
)
egg_surv_model <- self_projection_factory(egg_self_model)

pop$add_transition(from='autumn_eggs', to='winter_eggs', 
	projections = list(survive = egg_surv_model)

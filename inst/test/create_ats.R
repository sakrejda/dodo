library(dodo)
data('estimates-ATS-smolt-M4')
data('stages.ats')

stages.ats$season <- factor(stages.ats$season)

stages.ats <- stages.ats[ !(stages.ats$stage %in% 
	c('four_summer_parr','four_autumn_parr','four_winter_parr',
		'three_autumn_sea2', 'three_autumn_sea3', 'three_autumn_sea4',
		'three_winter_sea2', 'three_winter_sea3', 'three_winter_sea4')),]
mod <- !is.na(stages.ats$ageInSamples)

#stages.ats$ageInSamples <- factor(stages.ats$ageInSamples)

## Data:
stages <- structure(
	list(
	standardize_size = function(df) {
		df[['std_sizes']] <- (df[['sizes']] - df[['sizeAtAge']])/df[['sdAtAge']]
		return(df)
	},
	flow_conversion = function(df) {
		flowMeans <- c(spring=28,summer=11,autumn=12,winter=26)
		flowSds <- c(spring=9,summer=5,autumn=6,winter=7.5)
		season <- as.character(df[['season']])
		df[['flow_std']] <- 
			(df[['flow']] - flowMeans[season]) / flowSds[season]
		return(df)
	},
	temp_conversion = function(df) {
		tempMeans <- c(spring=18.02234,summer=18.50568,autumn=18.06938,winter=17.96012)
		tempSds <- c(spring=0.019765602, summer=0.031461813, autumn=0.022657542, winter=0.00111111)
		season <- as.character(df[['season']])
		df[['temp_std']] <-
			(df[['WB']] - tempMeans[season]) / tempSds[season]
		return(df)
	}

	),
	.Names = c("standardize_size", "flow_conversion", "temp_conversion") 

)

stages <- lapply( X=stages.ats[['stage']],
	FUN=function(stage) {
		o2 <- as.list(stages.ats[stages.ats[['stage']] == stage,])
		if (!is.na(stages.ats[stages.ats[['stage']] == stage, 'ageInSamples'])) {
			o1 <- stages
			return(c(o1,o2))
		} else {
			return(o2)
		}
	}
)
names(stages) <- stages.ats[['stage']]


pop <- new('population', 
	stages = stages.ats[['stage']],
	bins = stages.ats[['n_bins']],
	minima = stages.ats[['minima']],
	maxima = stages.ats[['maxima']],
	projections = c('survive', 'stretch', 'squash', 'smolt', 'grow'),
	traits = stages 
)

## AS A TEST:
flowMeans <- c(spring=28,summer=11,autumn=12,winter=26)
flowSds <- c(spring=9,summer=5,autumn=6,winter=7.5)
tempMeans <- c(spring=18.02234,summer=18.50568,autumn=18.06938,winter=17.96012)
tempSds <- c(spring=0.019765602, summer=0.031461813, autumn=0.022657542, winter=0.00111111)
e <- list()
for ( i in 1:nrow(stages.ats)) {
	stage <- stages.ats$stage[[i]]
	season <- as.character(stages.ats$season[[i]])
	e[[stage]] <- new.env()
	freshwater <- !is.na(stages.ats$ageInSamples[[i]]) || (i < 4)
	if (freshwater) {
		assign(x='flow', value=flowMeans[season], envir=e[[stage]])
		assign(x='WB', value=tempMeans[season], envir=e[[stage]])
	} else {
		assign(x='flow', value=NA, envir=e[[stage]])
		assign(x='WB', value=NA, envir=e[[stage]])
	}
}

pop$set_env(e)
##

egg_self_model <- new('pGLM',
	formula = ~ 1,
	family = binomial(link=logit),
	coefficients = list(
		"(Intercept)" = 2.2
	)
)
egg_surv_model <- self_projection_factory(egg_self_model)

################################################################################
pop$add_transition(from='autumn_eggs', to='winter_eggs', 
	projections = list(survive = egg_surv_model))
pop$add_transition(from='winter_eggs', to='spring_fry',
	projections = list(survive = egg_surv_model))
################################################################################

fry_surv_model <- new('pGLM',
	formula = ~ 1,
	family = binomial(link=logit),
	coefficients = list(
		"(Intercept)" = -0.9
	)
)
fry_self_model <- self_projection_factory(fry_surv_model)

fry_mean_size_model <- new('pGLM',
	formula = ~ 1,
	family = gaussian(),
	coefficients = list( "(Intercept)" = 55),
	epsilon = function(n=1) { rep(0,n) }, samp=FALSE
)

fry_var_size_model <- new('pGLM',
	formula = ~ 1,
	family = gaussian(link="log"),
	coefficients = list( "(Intercept)" = 0.5),
	epsilon = function(n=1) { rep(0,n) }, samp=FALSE
)

recruit_size_model <- dnorm_projection_factory(
	mean_model = fry_mean_size_model,
	variance_model = fry_var_size_model,
	target_dims = c(n_bins = 24, minimum = 49.9, maximum = 73.9)
)

################################################################################
pop$add_transition(from='spring_fry', to='zero_summer_parr',
	projection = list( 
		survive = fry_self_model, 
		grow = recruit_size_model)
)
################################################################################

juvenile_growth_mu_model <- new('pGLM',
  formula = ~ 1 + ageInSamples,
  family = gaussian(link="identity"),
  coefficients = list(
    "(Intercept)"  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 1,,],
    ageInSamples2  =  -s[['gr_beta_ais']][1,,] +  s[['gr_beta_ais']][ 2,,],
    ageInSamples3  =  -s[['gr_beta_ais']][1,,] +  s[['gr_beta_ais']][ 3,,],
    ageInSamples4  =  -s[['gr_beta_ais']][1,,] +  s[['gr_beta_ais']][ 4,,],
    ageInSamples5  =  -s[['gr_beta_ais']][1,,],
    ageInSamples6  =  -s[['gr_beta_ais']][1,,] +  s[['gr_beta_ais']][ 6,,],
    ageInSamples7  =  -s[['gr_beta_ais']][1,,] +  s[['gr_beta_ais']][ 7,,],
    ageInSamples8  =  -s[['gr_beta_ais']][1,,] +  s[['gr_beta_ais']][ 8,,],
    ageInSamples9  =  -s[['gr_beta_ais']][1,,] +  s[['gr_beta_ais']][ 9,,],
    ageInSamples10 =  -s[['gr_beta_ais']][1,,] +  s[['gr_beta_ais']][10,,],
    ageInSamples11 =  -s[['gr_beta_ais']][1,,] +  s[['gr_beta_ais']][11,,],
    ageInSamples12 =  -s[['gr_beta_ais']][1,,] +  s[['gr_beta_ais']][12,,],
    ageInSamples13 =  -s[['gr_beta_ais']][1,,] +  s[['gr_beta_ais']][13,,],
    ageInSamples14 =  -s[['gr_beta_ais']][1,,] +  s[['gr_beta_ais']][13,,],  ### coef num. 14 is not well estimated, N too low.
    ageInSamples15 = rep(0, length(s[['gr_beta_0']]))
  ),
  epsilon = function(n=1) { rep(0,n) }, samp = FALSE
)

juvenile_growth_sd_model <- new('pGLM',
	formula = ~ -1 + season,
	family = gaussian(link="identity"),
	coefficients = list(
		seasonautumn = s[['gr_sigma_beta_autumn']],
		seasonspring = s[['gr_sigma_beta_spring']],
		seasonsummer = s[['gr_sigma_beta_summer']],
		seasonwinter = s[['gr_sigma_beta_winter']]
	),
	epsilon = function(n=1) { rep(0,n) }, samp = FALSE
)

juvenile_self_model <- new('pGLM',
	formula = ~ 1 + ageInSamples + 
									ageInSamples:temp_std +
									ageInSamples:flow_std +
									ageInSamples:std_sizes,
	family = binomial(link="logit"),
	coefficients = list(
    "(Intercept)"  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 1,,],
    ageInSamples2  =  -s[['phi_beta_ais']][1,,] +  s[['phi_beta_ais']][ 2,,],   
    ageInSamples3  =  -s[['phi_beta_ais']][1,,] +  s[['phi_beta_ais']][ 3,,],
    ageInSamples4  =  -s[['phi_beta_ais']][1,,] +  s[['phi_beta_ais']][ 4,,],
    ageInSamples5  =  -s[['phi_beta_ais']][1,,],
    ageInSamples6  =  -s[['phi_beta_ais']][1,,] +  s[['phi_beta_ais']][ 6,,],
    ageInSamples7  =  -s[['phi_beta_ais']][1,,] +  s[['phi_beta_ais']][ 7,,],
    ageInSamples8  =  -s[['phi_beta_ais']][1,,] +  s[['phi_beta_ais']][ 8,,],
    ageInSamples9  =  -s[['phi_beta_ais']][1,,] +  s[['phi_beta_ais']][ 9,,],
    ageInSamples10 =  -s[['phi_beta_ais']][1,,] +  s[['phi_beta_ais']][10,,],
    ageInSamples11 =  -s[['phi_beta_ais']][1,,] +  s[['phi_beta_ais']][11,,],
    ageInSamples12 =  -s[['phi_beta_ais']][1,,] +  s[['phi_beta_ais']][12,,],
    ageInSamples13 =  -s[['phi_beta_ais']][1,,] +  s[['phi_beta_ais']][13,,],
    ageInSamples14 =  -s[['phi_beta_ais']][1,,] +  s[['phi_beta_ais']][14,,],
    ageInSamples15 =  -s[['phi_beta_ais']][1,,] +  s[['phi_beta_ais']][11,,],#rep(-100,length(s[['phi_beta_0']])),

    "ageInSamples1:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][1,,],
    "ageInSamples2:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][2,,],
    "ageInSamples3:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][3,,],
    "ageInSamples4:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][4,,],
    "ageInSamples5:temp_std" = s[['phi_beta_t']],
    "ageInSamples6:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][6,,],
    "ageInSamples7:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][7,,],
    "ageInSamples8:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][8,,],
    "ageInSamples9:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][9,,],
    "ageInSamples10:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][10,,],
    "ageInSamples11:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][11,,],
    "ageInSamples12:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][12,,],
    "ageInSamples13:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][13,,],
    "ageInSamples14:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][14,,],
    "ageInSamples15:temp_std" = rep(0, length(s[['gr_beta_0']])),

    "ageInSamples1:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][1,,],
    "ageInSamples2:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][2,,],
    "ageInSamples3:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][3,,],
    "ageInSamples4:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][4,,],
    "ageInSamples5:flow_std" = s[['phi_beta_d']],
    "ageInSamples6:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][6,,],
    "ageInSamples7:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][7,,],
    "ageInSamples8:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][8,,],
    "ageInSamples9:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][9,,],
    "ageInSamples10:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][10,,],
    "ageInSamples11:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][11,,],
    "ageInSamples12:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][12,,],
    "ageInSamples13:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][13,,],
    "ageInSamples14:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][14,,],
    "ageInSamples15:flow_std" = rep(0, length(s[['gr_beta_0']])),

    "ageInSamples1:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][1,,],
    "ageInSamples2:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][2,,],
    "ageInSamples3:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][3,,],
    "ageInSamples4:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][4,,],
    "ageInSamples5:std_sizes" = s[['phi_beta_size']],
    "ageInSamples6:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][6,,],
    "ageInSamples7:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][7,,],
    "ageInSamples8:std_sizes" = rep(0,length(s[['gr_beta_0']])), #s[['phi_beta_size']] + s[['phi_beta_size_ais']][8,,],
    "ageInSamples9:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][9,,],
    "ageInSamples10:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][10,,],
    "ageInSamples11:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][11,,],
    "ageInSamples12:std_sizes" = rep(0, length(s[['gr_beta_0']])), #s[['phi_beta_size']] + s[['phi_beta_size_ais']][12,,],
    "ageInSamples13:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][13,,],
    "ageInSamples14:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][14,,],
    "ageInSamples15:std_sizes" = rep(0, length(s[['gr_beta_0']]))

	),
  epsilon = function(n=1) { rep(0,n) }, samp = FALSE
)

juvenile_survival_model <- self_projection_factory(juvenile_self_model)



################################################################################
juvenile_growth_models <- list()
for ( i in 1:15) {
	juvenile_growth_models[[i]] <- offset_dlnorm_projection_factory(
		mean_model = juvenile_growth_mu_model,
		sd_model = juvenile_growth_sd_model,
		target_dims = c(
			n_bins = stages.ats[['n_bins']][i+4], 
			minimum = stages.ats[['minima']][i+4], 
			maximum = stages.ats[['maxima']][i+4]
		)
	)
	pop$add_transition(from=stages.ats[['stage']][i+3], to=stages.ats[['stage']][i+4],
		projection = list(
			survive = juvenile_survival_model, 
			grow = juvenile_growth_models[[i]]
		)
	)
}

################################################################################
parr_smolt_model <- new('pGLM',
	formula = ~ 1 + ageInSamples + ageInSamples:std_sizes,
	family = binomial(link="logit"),
	coefficients = list(
    "(Intercept)"  =  rep(-10^4,  length(s[['phi_beta_0']])),
    ageInSamples2  =  rep(-10^4,  length(s[['phi_beta_0']])),
    ageInSamples3  =  rep(-10^4,  length(s[['phi_beta_0']])),
    ageInSamples4  =  rep(-10^4,  length(s[['phi_beta_0']])),
    ageInSamples5  =  rep(-10^4,  length(s[['phi_beta_0']])),
    ageInSamples6  =  rep(-10^4,  length(s[['phi_beta_0']])),
    ageInSamples7  =  rep(-10^4,  length(s[['phi_beta_0']])),
    ageInSamples8  =  rep(0+10^4, length(s[['phi_beta_0']])),
    ageInSamples9  =  rep(-10^4,  length(s[['phi_beta_0']])),
    ageInSamples10 =  rep(-10^4,  length(s[['phi_beta_0']])),
    ageInSamples11 =  rep(-10^4,  length(s[['phi_beta_0']])),
    ageInSamples12 =  rep(1+10^4, length(s[['phi_beta_0']])),
    ageInSamples13 =  rep(-10^4,  length(s[['phi_beta_0']])),
    ageInSamples14 =  rep(-10^4,  length(s[['phi_beta_0']])),
    ageInSamples15 =  rep(-10^4,  length(s[['phi_beta_0']])),

    "ageInSamples1:std_sizes"  = rep(0, length(s[['phi_beta_0']])),
    "ageInSamples2:std_sizes"  = rep(0, length(s[['phi_beta_0']])),
    "ageInSamples3:std_sizes"  = rep(0, length(s[['phi_beta_0']])),
    "ageInSamples4:std_sizes"  = rep(0, length(s[['phi_beta_0']])),
    "ageInSamples5:std_sizes"  = rep(0, length(s[['phi_beta_0']])),
    "ageInSamples6:std_sizes"  = rep(0, length(s[['phi_beta_0']])),
    "ageInSamples7:std_sizes"  = rep(0, length(s[['phi_beta_0']])),
    "ageInSamples8:std_sizes"  = - s[['phi_beta_size']] - s[['phi_beta_size_ais']][8,,],
    "ageInSamples9:std_sizes"  = rep(0, length(s[['phi_beta_0']])),
    "ageInSamples10:std_sizes" = rep(0, length(s[['phi_beta_0']])),
    "ageInSamples11:std_sizes" = rep(0, length(s[['phi_beta_0']])),
    "ageInSamples12:std_sizes" = - s[['phi_beta_size']] - s[['phi_beta_size_ais']][12,,],
    "ageInSamples13:std_sizes" = rep(0, length(s[['phi_beta_0']])),
    "ageInSamples14:std_sizes" = rep(0, length(s[['phi_beta_0']])),
    "ageInSamples15:std_sizes" = rep(0, length(s[['phi_beta_0']]))
	),
	epsilon = function(n=1) { rep(0,n) }, samp = FALSE
)

parr_smolt_projection <- self_projection_factory( self_model = parr_smolt_model)
squash_smolt_projection <- squash_projection_factory(
	target_dims = c(n_bins=1, minimum=0, maximum=10^3))

pop$add_transition(from='two_spring_parr', to='one_summer_sea2',
	projection = list(smolt = parr_smolt_projection, squash = squash_smolt_projection))
pop$add_transition(from='three_spring_parr', to='one_summer_sea3',
	projection = list(smolt = parr_smolt_projection, squash = squash_smolt_projection))
pop$add_transition(from='four_spring_parr', to='one_summer_sea4',
	projection = list(smolt = parr_smolt_projection, squash = squash_smolt_projection))

################################################################################


################################################################################

ocean_self_model <- new('pGLM',
	formula = ~ 1,
	family = binomial(link="logit"),
	coefficients = list("(Intercept)" = 0),
	epsilon = function(n=1) { rep(0,n) }, samp = FALSE
)

ocean_self_projection <- self_projection_factory(ocean_self_model)

pop$add_transition(from='one_summer_sea2', to='one_autumn_sea2', projection = list(survive=ocean_self_projection))
pop$add_transition(from='one_autumn_sea2', to='one_winter_sea2', projection = list(survive=ocean_self_projection))
pop$add_transition(from='one_winter_sea2', to='two_spring_sea2', projection = list(survive=ocean_self_projection))
pop$add_transition(from='two_spring_sea2', to='two_summer_sea2', projection = list(survive=ocean_self_projection))
pop$add_transition(from='two_summer_sea2', to='two_autumn_sea2', projection = list(survive=ocean_self_projection))
pop$add_transition(from='two_autumn_sea2', to='two_winter_sea2', projection = list(survive=ocean_self_projection))
pop$add_transition(from='two_winter_sea2', to='three_spring_sea2', projection = list(survive=ocean_self_projection))
pop$add_transition(from='three_spring_sea2', to='three_summer_sea2', projection = list(survive=ocean_self_projection))
#pop$add_transition(from='three_summer_sea2', to='three_autumn_sea2', projection = list(survive=ocean_self_projection))
#pop$add_transition(from='three_autumn_sea2', to='three_winter_sea2', projection = list(survive=ocean_self_projection))


pop$add_transition(from='one_summer_sea3', to='one_autumn_sea3', projection = list(survive=ocean_self_projection))
pop$add_transition(from='one_autumn_sea3', to='one_winter_sea3', projection = list(survive=ocean_self_projection))
pop$add_transition(from='one_winter_sea3', to='two_spring_sea3', projection = list(survive=ocean_self_projection))
pop$add_transition(from='two_spring_sea3', to='two_summer_sea3', projection = list(survive=ocean_self_projection))
pop$add_transition(from='two_summer_sea3', to='two_autumn_sea3', projection = list(survive=ocean_self_projection))
pop$add_transition(from='two_autumn_sea3', to='two_winter_sea3', projection = list(survive=ocean_self_projection))
pop$add_transition(from='two_winter_sea3', to='three_spring_sea3', projection = list(survive=ocean_self_projection))
pop$add_transition(from='three_spring_sea3', to='three_summer_sea3', projection = list(survive=ocean_self_projection))
#pop$add_transition(from='three_summer_sea3', to='three_autumn_sea3', projection = list(survive=ocean_self_projection))
#pop$add_transition(from='three_autumn_sea3', to='three_winter_sea3', projection = list(survive=ocean_self_projection))

pop$add_transition(from='one_summer_sea4', to='one_autumn_sea4', projection = list(survive=ocean_self_projection))
pop$add_transition(from='one_autumn_sea4', to='one_winter_sea4', projection = list(survive=ocean_self_projection))
pop$add_transition(from='one_winter_sea4', to='two_spring_sea4', projection = list(survive=ocean_self_projection))
pop$add_transition(from='two_spring_sea4', to='two_summer_sea4', projection = list(survive=ocean_self_projection))
pop$add_transition(from='two_summer_sea4', to='two_autumn_sea4', projection = list(survive=ocean_self_projection))
pop$add_transition(from='two_autumn_sea4', to='two_winter_sea4', projection = list(survive=ocean_self_projection))
pop$add_transition(from='two_winter_sea4', to='three_spring_sea4', projection = list(survive=ocean_self_projection))
pop$add_transition(from='three_spring_sea4', to='three_summer_sea4', projection = list(survive=ocean_self_projection))
#pop$add_transition(from='three_summer_sea4', to='three_autumn_sea4', projection = list(survive=ocean_self_projection))
#pop$add_transition(from='three_autumn_sea4', to='three_winter_sea4', projection = list(survive=ocean_self_projection))

################################################################################
## Returns
################################################################################

return_self_model <- new('pGLM',
	formula = ~ 1,
	family = binomial(link="logit"),
	coefficients = list("(Intercept)" = 0),
	epsilon = function(n=1) { rep(0,n) }, samp = FALSE
)

return_self_projection <- self_projection_factory(return_self_model)

pop$add_transition(from='one_winter_sea2', to='two_spring_return2', projection = list(survive=return_self_projection))
pop$add_transition(from='one_winter_sea3', to='two_spring_return3', projection = list(survive=return_self_projection))
pop$add_transition(from='one_winter_sea4', to='two_spring_return4', projection = list(survive=return_self_projection))

pop$add_transition(from='two_spring_return2', to='two_summer_return2', projection = list(survive=return_self_projection))
pop$add_transition(from='two_spring_return3', to='two_summer_return3', projection = list(survive=return_self_projection))
pop$add_transition(from='two_spring_return4', to='two_summer_return4', projection = list(survive=return_self_projection))

pop$add_transition(from='two_spring_sea2', to='two_summer_return2', projection = list(survive=return_self_projection))
pop$add_transition(from='two_spring_sea3', to='two_summer_return3', projection = list(survive=return_self_projection))
pop$add_transition(from='two_spring_sea4', to='two_summer_return4', projection = list(survive=return_self_projection))

pop$add_transition(from='two_summer_return2', to='two_autumn_return2', projection = list(survive=return_self_projection))
pop$add_transition(from='two_summer_return3', to='two_autumn_return3', projection = list(survive=return_self_projection))
pop$add_transition(from='two_summer_return4', to='two_autumn_return4', projection = list(survive=return_self_projection))

pop$add_transition(from='two_summer_sea2', to='two_autumn_return2', projection = list(survive=return_self_projection))
pop$add_transition(from='two_summer_sea3', to='two_autumn_return3', projection = list(survive=return_self_projection))
pop$add_transition(from='two_summer_sea4', to='two_autumn_return4', projection = list(survive=return_self_projection))

pop$add_transition(from='two_winter_sea2', to='three_spring_return2', projection = list(survive=return_self_projection))
pop$add_transition(from='two_winter_sea3', to='three_spring_return3', projection = list(survive=return_self_projection))
pop$add_transition(from='two_winter_sea4', to='three_spring_return4', projection = list(survive=return_self_projection))

pop$add_transition(from='three_spring_return2', to='three_summer_return2', projection = list(survive=return_self_projection))
pop$add_transition(from='three_spring_return3', to='three_summer_return3', projection = list(survive=return_self_projection))
pop$add_transition(from='three_spring_return4', to='three_summer_return4', projection = list(survive=return_self_projection))

pop$add_transition(from='three_spring_sea2', to='three_summer_return2', projection = list(survive=return_self_projection))
pop$add_transition(from='three_spring_sea3', to='three_summer_return3', projection = list(survive=return_self_projection))
pop$add_transition(from='three_spring_sea4', to='three_summer_return4', projection = list(survive=return_self_projection))

pop$add_transition(from='three_summer_return2', to='three_autumn_return2', projection = list(survive=return_self_projection))
pop$add_transition(from='three_summer_return3', to='three_autumn_return3', projection = list(survive=return_self_projection))
pop$add_transition(from='three_summer_return4', to='three_autumn_return4', projection = list(survive=return_self_projection))

pop$add_transition(from='three_summer_sea2', to='three_autumn_return2', projection = list(survive=return_self_projection))
pop$add_transition(from='three_summer_sea3', to='three_autumn_return3', projection = list(survive=return_self_projection))
pop$add_transition(from='three_summer_sea4', to='three_autumn_return4', projection = list(survive=return_self_projection))

################################################################################
## Fecundity
################################################################################

fecundity_self_model <- new('pGLM',
	formula = ~ 1,
	family = poisson(link="log"),
	coefficients = list("(Intercept)" = 8.5),
	epsilon = function(n=1) { rep(0,n) }, samp = FALSE
)

fecundity_self_projection <- self_projection_factory(fecundity_self_model)

pop$add_transition(from='two_autumn_return2', to='autumn_eggs', projection = list(survive=fecundity_self_projection))
pop$add_transition(from='two_autumn_return3', to='autumn_eggs', projection = list(survive=fecundity_self_projection))
pop$add_transition(from='two_autumn_return4', to='autumn_eggs', projection = list(survive=fecundity_self_projection))

pop$add_transition(from='three_autumn_return2', to='autumn_eggs', projection = list(survive=fecundity_self_projection))
pop$add_transition(from='three_autumn_return3', to='autumn_eggs', projection = list(survive=fecundity_self_projection))
pop$add_transition(from='three_autumn_return4', to='autumn_eggs', projection = list(survive=fecundity_self_projection))

################################################################################
pop$make_matrix()
pop$projection[1:10,1:10]
################################################################################

dat <- list()
dat_o <- list()
for ( i in stages.ats$stage[4:19]) {
	dat[[i]] <- pop$distribution$newdata(stage=i, covariates=as.list(pop$env[[i]]))
	dat_o[[i]] <-
		cbind(dat[[i]],phi=juvenile_self_model$predict(newdata=dat[[i]]),
								 mu=juvenile_growth_mu_model$predict(newdata=dat[[i]]),
								 sd=juvenile_growth_sd_model$predict(newdata=dat[[i]]))
	print(i)
	sizes_in <- dat[[i]]$std_sizes
	sizes_out <- min(sizes_in):round(2*max(sizes_in))
	M <- lapply(
		X = 1:length(sizes_in),
		FUN = function(j, szi, szo, meanlog, sdlog) {
			dolnorm(x=szo, y=szi[j], meanlog=meanlog, sdlog=sdlog)
		},
		szi=sizes_in,
		szo=sizes_out,
		meanlog = unique(dat_o[[i]]$mu),
		sdlog = unique(dat_o[[i]]$sd)
	)
	M <- do.call(what=cbind, args = M)
	plot(c(0,100),c(0,100), type='n', xlab=i, ylab="")
	rasterImage(M,0,0,length(sizes_in), length(sizes_out), FALSE); readline()

	print(curve(from=0, to=300, 
		expr=dlnorm(x=x,meanlog=unique(dat_o[[i]]$mu), sdlog=unique(dat_o[[i]]$sd))))
	readline()
	print(ggplot(data=dat_o[[i]], aes(x=std_sizes, y=phi)) + geom_point()
				+ xlim(-3,3))
	readline()
}

# Right and left eigenvalue decomposition
Reig <- eigen(  pop$projection$A )
Leig <- eigen(t(pop$projection$A))

# Plots of decaying magnitude of real and
# imaginary parts of eigenvalues.
plot(Re(Reig$values[1:100]))
plot(Im(Reig$values[1:100]))

# Dominant right eigenvector:
right_ev <- (-Re(Reig$vectors[,1]))
stages <- vector(mode='character', length=length(right_ev))
for ( i in 1:length(pop$distribution$stage_names)) {
	stages[pop$distribution$start_index[i]:pop$distribution$stop_index[i]] <- 
		pop$distribution$stage_names[i]
}
stage_distribution <- aggregate(
	x=right_ev, 
	by=list(stage=stages), FUN=sum)
sortifying <- sapply(
	X = stage_distribution$stage,
	FUN = function(stage) {
		return(which(stage == pop$distribution$stage_names))
	},
	USE.NAMES=FALSE
)
stage_distribution$sort <- sortifying
stage_distribution <- stage_distribution[order(stage_distribution$sort),]
stage_distribution$stage_factor <- 
	factor(x=stage_distribution$stage, levels=rev(stage_distribution$stage))
ggplot(data=stage_distribution, aes(x=x, y=stage_factor)) + geom_point()

# Stable stage distribution
right_ev <- (-Re(Reig$vectors[,1]))
stages <- vector(mode='character', length=length(right_ev))
for ( i in 1:length(pop$distribution$stage_names)) {
	stages[pop$distribution$start_index[i]:pop$distribution$stop_index[i]] <- 
		pop$distribution$stage_names[i]
}
stage_distribution <- aggregate(
	x=right_ev, 
	by=list(stage=stages), FUN=sum)
sortifying <- sapply(
	X = stage_distribution$stage,
	FUN = function(stage) {
		return(which(stage == pop$distribution$stage_names))
	},
	USE.NAMES=FALSE
)
stage_distribution$sort <- sortifying
stage_distribution <- stage_distribution[order(stage_distribution$sort),]
stage_distribution$stage_factor <- 
	factor(x=stage_distribution$stage, levels=rev(stage_distribution$stage))

stage_distribution_pl <- ggplot(
	data=stage_distribution, 
	aes(x=x, y=stage_factor)
) + geom_point() +
		scale_x_log10()


size_distributions <- by(
	data = data.frame(stage=stages, density=right_ev,
										size=pop$distribution$midpoints[,1]),
	INDICES = list(stage=stages),
	FUN = function(dat) {
		o <- data.frame(
			stage = unique(dat$stage),
			total = sum(dat$density),
			mode = dat$size[dat$density == max(dat$density)],
			sd = 1
		)
		return(o)
	},
	simplify=FALSE
)
size_distributions <- do.call(what=rbind, args=size_distributions)
rownames(size_distributions) <- NULL
size_distributions$proportion <- size_distributions$total / sum(size_distributions$total)






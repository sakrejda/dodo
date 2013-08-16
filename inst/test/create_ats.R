library(dodo)
data('estimates-ATS-smolt-M3')
data('stages.ats')

stages.ats$ageInSamples <- factor(stages.ats$ageInSamples)

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
		df[['flow_std']] <- 
			(df[['flow']] - flowMeans[df[['season']]]) / flowSds[df[['season']]]
		return(df)
	},
	temp_conversion = function(df) {
		tempMeans <- c(spring=18.02234,summer=18.50568,autumn=18.06938,winter=17.96012)
		tempSds <- c(spring=0.019765602, summer=0.031461813, autumn=0.022657542, winter=0.00111111)
		df[['temp_std']] <-
			(df[['WB']] - tempMeans[df[['season']]]) / tempSds[df[['season']]]
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
	projections = c('survive', 'stretch', 'smolt', 'grow'),
	traits = stages 
)

## AS A TEST:
e <- new.env()
e$flow <- 0
e$WB <- 0
e$season <- factor(x="spring", levels=sort(c("spring","summer","autumn","winter")))
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
	projection = list( survive = fry_self_model, grow = recruit_size_model)
)
################################################################################

juvenile_growth_mu_model <- new('pGLM',
  formula = ~ 1 + ageInSamples + offset(sizes),
  family = gaussian(link="identity"),
  coefficients = list(
    "(Intercept)"  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 1],
    ageInSamples2  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 2],
    ageInSamples3  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 3],
    ageInSamples4  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 4],
    ageInSamples5  = s[['gr_beta_0']],
    ageInSamples6  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 6],
    ageInSamples7  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 7],
    ageInSamples8  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 8],
    ageInSamples9  = s[['gr_beta_0']] +  s[['gr_beta_ais']][ 9],
    ageInSamples10 = s[['gr_beta_0']] +  s[['gr_beta_ais']][10],
    ageInSamples11 = s[['gr_beta_0']] +  s[['gr_beta_ais']][11],
    ageInSamples12 = s[['gr_beta_0']] +  s[['gr_beta_ais']][12],
    ageInSamples13 = s[['gr_beta_0']] +  s[['gr_beta_ais']][13],
    ageInSamples14 = s[['gr_beta_0']] +  s[['gr_beta_ais']][14],
    ageInSamples15 = rep(0, length(s[['gr_beta_0']]))
  ),
  epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.5) }, samp = FALSE
)

juvenile_growth_sd_model <- new('pGLM',
	formula = ~ -1 + season,
	family = gaussian(link="log"),
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
    "(Intercept)"  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 1],
    ageInSamples2  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 2],
    ageInSamples3  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 3],
    ageInSamples4  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 4],
    ageInSamples5  = s[['phi_beta_0']],
    ageInSamples6  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 6],
    ageInSamples7  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 7],
    ageInSamples8  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 8],
    ageInSamples9  = s[['phi_beta_0']] +  s[['phi_beta_ais']][ 9],
    ageInSamples10 = s[['phi_beta_0']] +  s[['phi_beta_ais']][10],
    ageInSamples11 = s[['phi_beta_0']] +  s[['phi_beta_ais']][11],
    ageInSamples12 = s[['phi_beta_0']] +  s[['phi_beta_ais']][12],
    ageInSamples13 = s[['phi_beta_0']] +  s[['phi_beta_ais']][13],
    ageInSamples14 = s[['phi_beta_0']] +  s[['phi_beta_ais']][14],
    ageInSamples15 = rep(-100,length(s[['phi_beta_0']])),

    "ageInSamples1:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][1],
    "ageInSamples2:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][2],
    "ageInSamples3:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][3],
    "ageInSamples4:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][4],
    "ageInSamples5:temp_std" = s[['phi_beta_t']],
    "ageInSamples6:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][6],
    "ageInSamples7:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][7],
    "ageInSamples8:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][8],
    "ageInSamples9:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][9],
    "ageInSamples10:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][10],
    "ageInSamples11:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][11],
    "ageInSamples12:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][12],
    "ageInSamples13:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][13],
    "ageInSamples14:temp_std" = s[['phi_beta_t']] + s[['phi_beta_t_ais']][14],
    "ageInSamples15:temp_std" = rep(0, length(s[['gr_beta_0']])),

    "ageInSamples1:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][1],
    "ageInSamples2:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][2],
    "ageInSamples3:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][3],
    "ageInSamples4:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][4],
    "ageInSamples5:flow_std" = s[['phi_beta_d']],
    "ageInSamples6:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][6],
    "ageInSamples7:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][7],
    "ageInSamples8:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][8],
    "ageInSamples9:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][9],
    "ageInSamples10:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][10],
    "ageInSamples11:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][11],
    "ageInSamples12:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][12],
    "ageInSamples13:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][13],
    "ageInSamples14:flow_std" = s[['phi_beta_d']] + s[['phi_beta_d_ais']][14],
    "ageInSamples15:flow_std" = rep(0, length(s[['gr_beta_0']])),

    "ageInSamples1:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][1],
    "ageInSamples2:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][2],
    "ageInSamples3:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][3],
    "ageInSamples4:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][4],
    "ageInSamples5:std_sizes" = s[['phi_beta_size']],
    "ageInSamples6:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][6],
    "ageInSamples7:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][7],
    "ageInSamples8:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][8],
    "ageInSamples9:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][9],
    "ageInSamples10:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][10],
    "ageInSamples11:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][11],
    "ageInSamples12:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][12],
    "ageInSamples13:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][13],
    "ageInSamples14:std_sizes" = s[['phi_beta_size']] + s[['phi_beta_size_ais']][14],
    "ageInSamples15:std_sizes" = rep(0, length(s[['gr_beta_0']]))

	),
  epsilon = function(n=1) { rnorm(n=n, mean=0, sd=0.5) }, samp = FALSE
)

juvenile_survival_model <- self_projection_factory(juvenile_self_model)

juvenile_growth_models <- list()
for ( i in 1:18) {
	juvenile_growth_models[[i]] <- dlnorm_projection_factory(
	mean_model = juvenile_growth_mu_model,
	sd_model = juvenile_growth_sd_model,
	target_dims = c(
		n_bins = stages.ats[['n_bins']][i+4], 
		minimum = stages.ats[['minima']][i+4], 
		maximum = stages.ats[['maxima']][i+4])
	)
	pop$add_transition(from=stages.ats[['stage']][i+3], to=stages.ats[['stage']][i+4],
		projection = list(survive = juvenile_survival_model,
											grow = juvenile_growth_models[[i]])
	)
}



################################################################################
pop$make_matrix()
pop$projection[1:10,1:10]



